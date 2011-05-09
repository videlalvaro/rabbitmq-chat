-module(browser).

-export([browse/0]).

browse() ->
    rb:start(),
    rb:grep("User connected").