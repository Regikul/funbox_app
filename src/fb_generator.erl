%%%-------------------------------------------------------------------
%%% @author regikul
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(fb_generator).

-include("funbox.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

-export([start_link/0]).
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(SERVER, ?MODULE).

-define(TIME_NOW, erlang:monotonic_time(millisecond)).

-record(state, {
  redis :: pid(),
  tick_no = 0 :: non_neg_integer(),
  queue :: binary(),
  upper :: pos_integer(),
  last_event :: integer()
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  RedisConfig = case application:get_env(redis) of
                  {ok, Config} -> Config;
                  undefined -> []
                end,
  Host = proplists:get_value(host, RedisConfig, ?REDIS_HOST),
  Port = proplists:get_value(port, RedisConfig, ?REDIS_PORT),
  DB = proplists:get_value(db, RedisConfig, ?REDIS_DB),
  Queue = case application:get_env(queue_key) of
            {ok, Q} -> Q;
            undefined -> ?REDIS_GENERATOR_QUEUE
          end,
  Upper = case application:get_env(n) of
            {ok, X} -> X;
            undefined -> ?UPPER_VALUE
          end,
  {ok, Redis} = eredis:start_link(Host, Port, DB),
  tick(),
  {ok, #state{
    redis = Redis,
    queue = Queue,
    upper = Upper,
    last_event = ?TIME_NOW
  }}.

handle_call(_Request, _From, State = #state{}) ->
  lager:warning("unknown call ~p", [_Request]),
  {reply, ok, State}.

handle_cast({tick}, #state{last_event = LastEvent,
                           redis = Redis,
                           queue = Queue,
                           upper = Upper
                          } = State) ->
  tick(),
  Now = ?TIME_NOW,
  case false of
    true -> _ = numbers(Upper);
    false -> ok
  end,
  Generator = fun () -> numbers(Upper) end,
  Numbers = lists:flatmap(fun (F) -> F() end, lists:duplicate(Now - LastEvent, Generator)),
  push_into_queue(Redis, Queue, Numbers),
  {noreply, State#state{last_event = Now}};
handle_cast(_Request, State = #state{}) ->
  lager:warning("unknown cast ~p", [_Request]),
  {noreply, State}.

handle_info({tick}, State) ->
  lager:info("Got a tick!"),
  {noreply, State};
handle_info(_Info, State = #state{}) ->
  lager:warning("unknown info ~p", [_Info]),
  {noreply, State}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

tick() ->
  timer:apply_after(1, gen_server, cast, [?SERVER, {tick}]).
%%  gen_server:cast(?SERVER, {tick}).

numbers(Upper) when Upper >= 2 ->
  [
    rand:uniform(Upper - 1) + 1,
    rand:uniform(Upper - 1) + 1,
    rand:uniform(Upper - 1) + 1
  ].

push_into_queue(Redis, Queue, Values) ->
  eredis:q(Redis, ["LPUSH", Queue | Values]).

-ifdef(TEST).

numbers_test() ->
  UpTo = 2,
  N = numbers(UpTo),
  ?assert(length(N) =:= 3),
  ?assert(lists:all(fun (X) -> 2 =< X andalso X =< UpTo end, N)).

recv_all() ->
  receive
    {value, Time, Value} -> [{Time, Value} | recv_all()]
  after
    1 -> []
  end.

-record(time_state, {
  count :: non_neg_integer() | undefined,
  min :: float() | undefined,
  max :: float() | undefined,
  first :: integer() | undefined,
  last :: integer() | undefined,
  prev :: integer() | undefined
}).

count_stat({Time, _Value}, #time_state{first = undefined}) ->
  #time_state{count = 1, first = Time, prev = Time, last = Time};
count_stat({Time, _Value}, #time_state{first = First,
                                       count = Count,
                                       prev = Prev,
                                       min = undefined,
                                       max = undefined
                                      }) ->
  #time_state{count = Count + 1,
              prev = Time,
              first = First,
              min = Time - Prev,
              max = Time - Prev,
              last = Time
             };
count_stat({Time, _Value}, #time_state{first = First,
                                       count = Count,
                                       prev = Prev,
                                       min = Min,
                                       max = Max
                                      }) ->
  TDelta = Time - Prev,
  NewMax = case TDelta > Max of
             true -> TDelta;
             false -> Max
           end,
  NewMin = case TDelta < Min of
             true -> TDelta;
             false -> Min
           end,
  #time_state{count = Count + 1,
              prev = Time,
              last = Time,
              min = NewMin,
              max = NewMax,
              first = First
             }.



-define(RATE_PER_SEC, 3000).
-define(RATE_DIFF, 100).

rates_test() ->
  meck:new(eredis, [passthrough]),
  Self = self(),
  Sender = fun (Values) ->
             lists:foreach(fun (X) -> Self ! {value, erlang:monotonic_time(microsecond), X} end, Values)
           end,
  meck:expect(eredis, q, fun (_Redis, [_CMD, _KEY | Values]) -> Sender(Values) end),
  meck:expect(eredis, start_link, fun ([_Host, _Port, _DB]) -> ok end),
  {ok, Pid} = ?MODULE:start_link(),
  Seconds = 3,
  receive
  after
    timer:seconds(Seconds) -> gen_server:stop(Pid)
  end,
  Input = recv_all(),
  TimeState = lists:foldl(fun count_stat/2, #time_state{}, Input),

  ?assert(abs(TimeState#time_state.count - ?RATE_PER_SEC * Seconds) < ?RATE_DIFF),

  FromMicroToMilli = 1000 * 1000,
  Avg = TimeState#time_state.count * FromMicroToMilli / (TimeState#time_state.last - TimeState#time_state.first),
  ?assert(abs(Avg - ?RATE_PER_SEC) < ?RATE_DIFF).

-endif.
