-module(muo_main).

%% API
-export([start_link/0, stop/0, check/1, check_file/1, check_file/2, get_failed/0, init/1,
  handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behaviour(gen_server).

-record(state, {failed=[]}).


check_file(Path) ->
  {ok, Device} = file:open(Path, [read]),
  check_file(Device, io:get_line(Device, "")).

check_file(_Device, eof) ->
  ok;

check_file(Device, Line) ->
  Url = string:strip(Line, both, $\n),
  check([Url]),
  check_file(Device, io:get_line(Device, "")).

check(Urls) ->
  gen_server:call(muo_main, {check, Urls}).

get_failed() ->
  gen_server:call(muo_main, {failed}).


start_link() ->
  wpool:start_pool(muo_worker_pool, [{workers, 50}, {worker, {muo_worker, []}}]),
  gen_server:start_link({local, muo_main}, ?MODULE, {}, []).


stop() ->
  wpool:stop(muo_worker_pool),
  gen_server:cast(?MODULE, stop).


init(_Args) ->
  {ok, #state{}}.

handle_call({check, [Url | Tail]}, From, State) ->
  wpool:cast(muo_worker_pool, {Url, self()}, available_worker),
  handle_call({check, Tail}, From, State);

handle_call({check, []}, From, State) ->
  {reply, ok, State};

handle_call({failed}, From, State) ->
  {reply, State#state.failed, State}.

handle_cast(Request, State) ->
  erlang:error(not_implemented_1).

handle_info({Url, {{_, Status, _}, _, _}}, State) when Status =/= 200 ->
  io:format("Failed! ~p~n", [Url]),
  {noreply, State#state{failed=[Url | State#state.failed]}};

handle_info({Url, {{_, Status, _}, _, _}}, State) when Status == 200 ->
  %io:format("Worked! ~p~n", [Url]),
  {noreply, State};

handle_info({Url, {error, Reason}}, State) ->
  %io:format("Error! ~p -> ~p~n", [Url, Reason]),
  {noreply, State}.

%handle_info(Info, State) ->
%  io:format("Info muo_main: ~p~n", [Info]),
%  {noreply, State}.

terminate(Reason, State) ->
  io:format("terminate~n"),
  ok.

code_change(OldVsn, State, Extra) ->
  io:format("code change~n"),
  {ok, State}.
