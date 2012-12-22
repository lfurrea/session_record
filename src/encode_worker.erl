%%%-------------------------------------------------------------------
%%% @author Luis F Urrea <lfurrea@simplecs.net>
%%% @copyright (C) 2012, Luis F Urrea
%%% @doc
%%%
%%% @end
%%% Created : 19 Dec 2012 by Luis F Urrea <lfurrea@simplecs.net>
%%%-------------------------------------------------------------------
-module(encode_worker).

%%API

-export([start_link/2, init/3]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

start_link(Body, Tag) ->
    proc_lib:start_link(?MODULE, init, [self(), Body, Tag]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes encode execution process
%% @end
%%--------------------------------------------------------------------


init(ParentPid, Body, Tag) ->
    process_flag(trap_exit, true),
    proc_lib:init_ack(ParentPid, {ok, self()}),
    _ = encode(Body, Tag).

%% Internal functions

encode(Filename, Tag) ->
    File = binary_to_list(Filename),
    %% TODO: use get_env to obtain recordings base_dir
    SourceBaseDir = "/usr/local/freeswitch/recordings/",
    EncodeBaseDir = "/usr/local/freeswitch/encodings/",
    OggencOut = os:cmd("oggenc -q -1 --downmix " ++ SourceBaseDir ++ File ++ ".wav" ++ " -o " ++ EncodeBaseDir ++ File ++ ".ogg"),
    case re:run(OggencOut, "ERROR") of
	nomatch ->
	    %%TODO: Handle files that take more than 60 mins
	    case re:run(OggencOut, "\\tFile length:\s+(.*)m\s+(.*)\..s.*\\n",[{capture, all_but_first, list}]) of
		nomatch ->
		    %% We have a valid source file but we failed to properly encode it
		    %% return not_ok so that other worker tries again
		    gen_server:cast(record_core, {encode_not_ok, Filename, Tag});
		{match, [Min,Secs]} ->
		    io:format("[x] File has been encoded, Length: ~pm ~ps~n", [Min, Secs]),
		    {match, MetaData} = re:run(File, "(\\d{4})-(\\d{2})-(\\d{2})-(\\d{2})-(\\d{2})-(\\d{2})_(.+)_(.+)"
				      ,[{capture, all_but_first, list}]),
		    Duration = list_to_integer(Min) * 60 + list_to_integer(Secs),
		    FilePath = EncodeBaseDir ++ File ++ ".ogg",
		    SaveRecord = [{Duration, FilePath}  |MetaData],
		    gen_server:cast(record_core, {encode_ok,SaveRecord, Tag})
		    
	    end;
	{match, _List} ->
	    %% TODO: Return ERROR string only
	    io:format("[x] Failed to encode source recording: ~p", [OggencOut]),
	    gen_server:cast(record_core, {encode_not_ok, Filename, Tag})
    end.

