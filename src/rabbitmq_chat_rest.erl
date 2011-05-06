-module(rabbitmq_chat_rest).
-export([start_link/1, stop/0]).

-include("amqp_client.hrl").

-record(http_state, {req}).
-record(websocket_state, {ws, conn, exchange, chann, consumer}).

-define(EXCHANGE_NAME, <<"chat_room">>).

%% start misultin http server
start_link(Port) ->
    {ok, Conn} = amqp_connection:start(network, #amqp_params{}),
    {ok, Chann} = amqp_connection:open_channel(Conn),
    amqp_channel:call(Chann,
                      #'exchange.declare'{ exchange = ?EXCHANGE_NAME,
                                           type = <<"x-recent-history">>,
                                           durable = true}),
    amqp_channel:close(Chann),
    misultin:start_link([{port, Port},
                         {loop, fun(Req) -> handle_http(#http_state{req=Req}, Port) end},
                         {ws_loop,
                          fun(Ws) ->
                                  handle_websocket(#websocket_state{ws=Ws, conn=Conn, exchange=?EXCHANGE_NAME})
                          end},
                          {ws_autoexit, false}]).

%% stop misultin
stop() ->
    misultin:stop().

handle_http(#http_state{req=Req}=State, _Port) ->
    handle(Req:get(method), Req:resource([lowercase, urldecode]), State).

handle('HEAD', [], #http_state{req=Req}) ->
    Req:ok("");

handle('GET', [], #http_state{req=Req}) ->
    Req:file("./priv/www/index.html", [{"Content-Type", "text/html"}]);

handle('GET',["favicon.ico"], #http_state{req=Req}) ->
    Req:file("./priv/www/favicon.ico", [{"Content-Type", "image/vnd.microsoft.icon"}]);

handle('GET',["robots.txt"], #http_state{req=Req}) ->
    Req:file("./priv/www/robots.txt", [{"Content-Type", "text/plain"}]);

%% TODO add file exist support, E-TAGS, etc.
handle('GET', ["js", FileName], #http_state{req=Req}) ->
    Req:file(filename:join("./priv/www/js/", FileName), [{"Content-Type", "text/javascript"}]);

handle('GET', ["css", FileName], #http_state{req=Req}) ->
    Req:file(filename:join("./priv/www/css/", FileName), [{"Content-Type", "text/css"}]).

%% callback on received websockets data
handle_websocket(#websocket_state{ws=Ws, conn=Conn, exchange=Exchange} = State) ->
    Chann = get_chann(State#websocket_state.chann, Conn),
    Consumer = maybe_start_consumer(State#websocket_state.consumer, [Chann, Exchange, self()]),
    State2 = State#websocket_state{chann=Chann, consumer=Consumer},
    receive
        {amqp_msgs, Msg} ->
            Ws:send(binary_to_term(Msg)),
            handle_websocket(State2);
        {browser, Data} ->
            amqp_channel:call(Chann, #'basic.publish'{exchange = Exchange},
                                #amqp_msg{payload = term_to_binary(Data)}),
            handle_websocket(State2);
        closed ->
            rabbitmq_chat_consumer:stop(Consumer),
            amqp_channel:close(Chann);
        _Ignore ->
            handle_websocket(State2)
    after 5000 ->
            handle_websocket(State2)
    end.

get_chann(undefined, Conn) ->
    {ok, Chann} = amqp_connection:open_channel(Conn),
    Chann;

get_chann(Chann, _Conn) when is_pid(Chann)->
    Chann.

maybe_start_consumer(undefined, Args) ->
    {ok, Pid} = rabbitmq_chat_consumer:start(Args),
    Pid;
maybe_start_consumer(Consumer, _) when is_pid(Consumer) ->
    Consumer.