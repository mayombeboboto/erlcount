-module(erlcount_counter).
-behaviour(gen_server).
-export([start_link/4]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-record(state, { dispatcher, ref, file, re }).

start_link(DispatcherPid, Ref, FileName, Regex) ->
    Args = [DispatcherPid, Ref, FileName, Regex],
    gen_server:start_link(?MODULE, Args, []).

init([DispatcherPid, Ref, FileName, Regex]) ->
    self() ! start,
    {ok, #state{ dispatcher=DispatcherPid,
                 ref=Ref,
                 file=FileName,
                 re=Regex }}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start, State=#state{ re=Re, ref=Ref }) ->
    {ok, Bin} = file:read_file(State#state.file),
    Count = erlcount_lib:regex_count(Re, Bin),
    erlcount_dispatch:complete(State#state.dispatcher, Re, Ref, Count),
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.