-module(rmqchat_utils).

-export([start/1, stop/1, halt_node/1]).

start([Node, App]) ->
    pong = net_adm:ping(Node),
    rpc:call(Node, application, start, [App]).

stop([Node, App]) ->
    pong = net_adm:ping(Node),
    rpc:call(Node, application, stop, [App]).

halt_node([Node]) ->
    pong = net_adm:ping(Node),
    rpc:call(Node, init, stop, []).
