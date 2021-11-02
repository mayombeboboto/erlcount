-module(erlcount_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    SupFlags = #{ strategy=> one_for_one,
                  intensity=> 5,
                  period=> 100
                },
    ChildSpecs = [#{ id=> dispatch,
                     start=> {erlcount_dispatch, start_link, []},
                     restart=> transient,
                     shutdown=> 60000,
                     type=> worker,
                     modules=> [erlcount_dispatch] }],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
