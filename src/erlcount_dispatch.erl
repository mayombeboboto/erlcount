-module(erlcount_dispatch).
-behaviour(gen_statem).
-export([start_link/0]).
-export([complete/4]).

-export([init/1]).
-export([callback_mode/0]).
-export([dispatching/3]).
-export([listening/3]).
-export([terminate/3]).

-define(POOL, erlcount).

-record(data, { regex=[], refs=[] }).

%%% PUBLIC APIs
start_link() ->
    gen_statem:start_link(?MODULE, [], []).

complete(Pid, Regex, Ref, Count) ->
    gen_statem:cast(Pid, {complete, Regex, Ref, Count}).

%%% CALLBACKs
init([]) ->
    {ok, Re} = application:get_env(regex),
    {ok, Dir} = application:get_env(directory),
    {ok, MaxFiles} = application:get_env(max_files),
    Start = {erlcount_counter, start_link, []},
    ppool:start_ppool(?POOL, MaxFiles, Start),
    case lists:all(fun valid_regex/1, Re) of
        true ->
            self() ! {start, Dir},
            Data = #data{ regex=[{R,0} || R <- Re] },
            {ok, dispatching, Data};
        false ->
            {stop, invalid_regex}
    end.

callback_mode() ->
    state_functions.

dispatching(cast, {continue, File, Continuation}, Data=#data{ regex=Re,
                                                              refs=Refs }) ->
    F = fun({Regex, _Count}, NewRefs) ->
            Ref = make_ref(),
            ppool:async_queue(?POOL, [self(), Ref, File, Regex]),
            [Ref|NewRefs]
        end,
    NewRefs = lists:foldl(F, Refs, Re),
    gen_statem:cast(self(), Continuation()),
    {keep_state, Data#data{ refs=NewRefs }};
dispatching(cast, done, Data) ->
    %% This is a special case. We cannot assume that all messages have NOT
    %% been received by the time we hit 'done'. As such, we directly move to
    %% listening/2 without waiting for an external event.
    listening(cast, done, Data);
dispatching(EventType, EventContent, Data) ->
    handle_common(EventType, EventContent, ?FUNCTION_NAME, Data).

listening(cast, done, #data{ regex=Re, refs=[] }) -> % all received!
    [io:format("Regex ~s has ~p results~n", [R,C]) || {R, C} <- Re],
    {stop, normal, done};
listening(cast, done, _Data) -> % entries still missing
    keep_state_and_data;
listening(EventType, EventContent, Data) ->
    handle_common(EventType, EventContent, ?FUNCTION_NAME, Data).

%%% INTERNAL FUNCTIONs
valid_regex(Re) ->
    try re:run("", Re) of
        _Value -> true
    catch
        error:badarg -> false
    end.

handle_common(cast, {start, Dir}, _State, _Data) ->
    gen_statem:cast(self(), erlcount_lib:find_erl(Dir)),
    keep_state_and_data;
handle_common(_EventType, {complete, Regex, Ref, Count}, State, Data) ->
    #data{ regex=Re, refs=Refs } = Data,
    {Regex, OldCount} = lists:keyfind(Regex, 1, Re),
    NewRe = lists:keyreplace(Regex, 1, Re, {Regex, OldCount+Count}),
    NewData = Data#data{ regex=NewRe, refs=Refs--[Ref] },
    case State of
        dispatching -> {keep_state, NewData};
        listening -> listening(cast, done, NewData)
    end.

terminate(_Reason, _State, _Data) ->
    ok.
