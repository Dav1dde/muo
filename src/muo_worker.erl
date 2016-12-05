-module(muo_worker).

-export([start_link/0, stop/0]).

%% gen_server
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state, {receivers=#{}}).


start_link() ->
  io:format("start link~n"),
  gen_server:start_link({local, muo_work}, ?MODULE, {}, []).

stop() ->
  io:format("stop~n"),
  gen_server:cast(?MODULE, stop).


init(_) ->
  %io:format("init~n"),
  {ok, #state{}}.


handle_call({Url, Receiver}, From, State) ->
  %io:format("handle check call ~p ~p ~n", [Url, State]),
  {ok, Result} = httpc:request(get, {Url, []}, [], []),
  %Receiver ! Result,
  %Result = 3,
  %io:format("Result ~p~n", [Result]),
  {reply, {Url, Result}, State}.

handle_cast({Url, Receiver}, State) ->
  %io:format("handle check cast~n"),
  {ok, Result} = httpc:request(get, {Url, []}, [], []),
  %NewState = State#state{receivers=maps:put(do_check(Url, async), {Receiver, Url}, State#state.receivers)},
  Receiver ! {Url, Result},
  {noreply, State}.


handle_info({http, {Ref, Result}}, State = #state{receivers = Receivers}) ->
  %io:format("Got http: ~p ~p ~n", [Ref, Receivers]),
  #{Ref := {Receiver, Url}} = Receivers,
  Receiver ! {Url, Result},
  {noreply, State#state{receivers=maps:remove(Ref, Receivers)}};

handle_info(Info, State) ->
  %io:format("Info muo_worker: ~p~n", [Info]),
  {noreply, State}.


terminate(Reason, State) ->
  io:format("terminate~n"),
  ok.


code_change(OldVsn, State, Extra) ->
  io:format("code change~n"),
  {ok, State}.


do_check(Url, snyc) ->
  httpc:request(get, {Url, []}, [], []);

do_check(Url, async) ->
  {ok, Ref} = httpc:request(get, {Url, []}, [], [{receiver, self()}, {sync, false}]),
  Ref.
