-module(rabbitmq_chat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Port} = application:get_env(rabbitmq_chat, port),
    Rest = {rabbitmq_chat_rest, {rabbitmq_chat_rest, start_link, [Port]}, permanent, 5000, worker, [rabbitmq_chat_rest]},
    {ok, { {one_for_one, 5, 10}, [Rest]} }.

