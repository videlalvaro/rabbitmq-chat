-module(rabbitmq_chat).
-export([start/0, stop/0]).

start() ->
    application:start(rabbitmq_chat).

stop() ->
    application:stop(rabbitmq_chat).