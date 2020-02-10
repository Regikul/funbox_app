%%%-------------------------------------------------------------------
%%% @author regikul
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(fb_generator).

-include("funbox.hrl").

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
  {ok, RedisConfig} = application:get_env(redis),
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
  lager:info("time diff is ~p", [Now - LastEvent]),
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

numbers(Upper) ->
  [
    rand:uniform(Upper),
    rand:uniform(Upper),
    rand:uniform(Upper)
  ].

push_into_queue(Redis, Queue, Values) ->
  eredis:q(Redis, ["LPUSH", Queue | Values]).
