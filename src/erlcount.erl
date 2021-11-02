-module(erlcount).
-behaviour(application).
-export([start/2]).
-export([stop/1]).

start(normal, _StartArgs) ->
    erlcount_sup:start_link().

stop(_State) ->
    ok.

