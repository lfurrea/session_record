-module(hello_qp).

-export([start_link/0, stop/2, open_channel/1, main/1]).

-include_lib("kernel/include/file.hrl").
-include_lib("deps/amqp_client/include/amqp_client.hrl").

start_link() ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "127.0.0.1"}),
    {ok, Connection}.

open_channel(Connection) ->
    {ok, Channel} = amqp_connection:open_channel(Connection),
    {ok, Channel}.


main(Channel) ->
    amqp_channel:call(Channel, #'queue.declare'{queue = <<"hello">>}),
    io:format(" [*] Waiting for messages. To exit press CTRL+C~n"),

    amqp_channel:subscribe(Channel, #'basic.consume'{queue = <<"hello">>,
                                                     no_ack = true}, self()),
    receive
        #'basic.consume_ok'{} -> ok
    end,
    loop(Channel).


loop(Channel) ->
    receive
        {#'basic.deliver'{}, #amqp_msg{payload = Body}} ->
            io:format(" [x] Received ~p~n", [Body]),
            loop(Channel)
    end.

stop(Channel, Connection) ->
    %% Close the channel
    amqp_channel:close(Channel),
    %% Close the connection
    amqp_connection:close(Connection),
    ok.
