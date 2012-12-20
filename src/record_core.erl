%%%-------------------------------------------------------------------
%%% @author Luis F Urrea <lfurrea@simplecs.net>
%%% @copyright (C) 2012, Luis F Urrea
%%% @doc
%%%
%%% @end
%%% Created : 19 Dec 2012 by Luis F Urrea <lfurrea@simplecs.net>
%%%-------------------------------------------------------------------
-module(record_core).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% API calls
-export([get_count/0, stop/0]).

-define(SERVER, ?MODULE). 

-include_lib("kernel/include/file.hrl").
-include_lib("deps/amqp_client/include/amqp_client.hrl").

-record(state, {limit = 2 :: integer(), 
		retries = 3 :: integer(), 
		received_count = 0 :: integer() ,
		connection :: pid(), 
		channel :: pid()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Fetches the number of received messages from the AMQP queue
%% @spec get_count() -> {ok, Count}
%% where
%%   Count = integer()
%% @end
%%--------------------------------------------------------------------

get_count() ->
    gen_server:call(?SERVER, get_count).

%%--------------------------------------------------------------------
%% @doc Closes channel/connection to AMQP broker and stops the server
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------

stop() ->
    gen_server:cast(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "127.0.0.1"}),
    {ok, Channel} = 
	amqp_connection:open_channel(Connection),
    State = #state{connection= Connection, channel= Channel},
    {ok, State, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_count, _From, State) ->
    {reply, {ok, State#state.received_count}, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({encode_ok, MetaData, Tag}, #state{channel = Channel} = State ) ->    
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
    io:format("Date: ~p, Time: ~p, Dest: ~p, CID: ~p~n",MetaData),
    {noreply, State};
handle_cast({encode_not_ok, Body, Tag}, #state{retries = N} = State ) when N > 0 ->
    supervisor:start_child(encode_sup, [Body, Tag]),
    NewState = State#state{retries = N -1},
    {noreply, NewState};
handle_cast({encode_not_ok, Body, Tag}, #state{channel = Channel, retries = N} = State ) when N =<0 ->
    io:format("[x] Could not encode source file. Giving up... ~p~n", [Body]),
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
    {noreply, State};
handle_cast(stop, #state{channel = Channel, connection= Connection} = State) ->
    %% Close the channel
    amqp_channel:close(Channel),
    %% Close the connection
    amqp_connection:close(Connection),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Body}},
	    #state{channel = Channel, received_count = ReceivedCount} =State) ->
    %% Here is where I do shit
    case valid_message(Body) of
	{true, no_recording} -> 
	    io:format("[x] No file to encode, moving on~n"),
	    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag});
	{true, valid_filename} -> 
	    case valid_file(Body) of
		true -> 
		    supervisor:start_child(encode_sup, [Body, Tag]);
		false -> 
		    io:format("[x] Cannot access source file giving up~p~n", [Body]),
		    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag})
	    end;
	
	{false, unexpected_message} -> 
	    io:format("[x] No file to encode, unexpected message~p~n", [Body]),
	    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag})
    end,
    
    %%io:format(" [x] Received ~p~n", [Body]),
    {noreply, State#state{received_count = ReceivedCount + 1}};
handle_info(#'basic.consume_ok'{}, State) ->
    io:format(" [*] Subscribed. To exit press CTRL+C~n"),
    {noreply, State};
handle_info(timeout, #state{limit = Limit, channel = Channel} = State) ->
    amqp_channel:call(Channel, #'queue.declare'{queue = <<"recordings">>, durable=true}),
    amqp_channel:call(Channel, #'basic.qos'{prefetch_count = Limit}),
    io:format(" [*] Subscribing for messages...~n"),
    amqp_channel:subscribe(Channel, #'basic.consume'{queue = <<"recordings">>}, self()),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

valid_message(Binary) ->
    Msg = binary_to_list(Binary),
    case Msg of 
	"No_Recording" -> {true, no_recording};
	_ ->   
	    case re:run(Msg, "\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}-\\d{2}_.+_.+") of
		nomatch -> {false, unexpected_message};
		{match, _List} -> {true, valid_filename}
	    end
    end.

valid_file(Filename) ->
    File = binary_to_list(Filename),
    BaseDir = "/usr/local/freeswitch/recordings/",
    case file:read_file_info(BaseDir ++ File ++ ".wav") of
	{ok, F = #file_info{}} ->
	    case F#file_info.type of
		directory ->
		    false;
		regular ->
		    true;
		_Other ->
		    false
	    end;
	{error, Reason} ->
	    io:format("Error al acceder al archivo ~p: ~p",[BaseDir ++ File, Reason]),
	    false
    end.
