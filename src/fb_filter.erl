%%%-------------------------------------------------------------------
%%% @author regikul
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(fb_filter).

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

-record(state, {
  redis :: pid(),
  queue_in,
  queue_out
}).

-define(READ_DELAY, 100).

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
  InputQueue = case application:get_env(queue_key) of
                 {ok, Generated} -> Generated;
                 undefined -> ?REDIS_GENERATOR_QUEUE
               end,
  OutputQueue = case application:get_env(result_set_key) of
                  {ok, Filtered} -> Filtered;
                  undefined -> ?REDIS_SIMPLE_NUMBERS_SET
                end,
  {ok, Redis} = eredis:start_link(Host, Port, DB),
  read_number(),
  {ok, #state{redis = Redis,
              queue_in = InputQueue,
              queue_out = OutputQueue
             }}.

handle_call(_Request, _From, State = #state{}) ->
  lager:warning("unknown call ~p", [_Request]),
  {reply, ok, State}.

handle_cast({check_prime, Number}, #state{redis = Redis, queue_out = Output} = State) ->
  case primes:check(Number) of
    true -> eredis:q(Redis, ["SADD", Output, integer_to_list(Number)]);
    false -> ok
  end,
  {noreply, State};
handle_cast({read_number}, #state{redis = Redis,
                                  queue_in = Input
                                 } = State) ->
  case eredis:q(Redis, ["BLPOP", Input, 1]) of
    {ok, undefined} -> read_number(?READ_DELAY);
    {ok, [_Queue, BinStr]} ->
      Number = binary_to_integer(BinStr),
      check_prime(Number),
      read_number()
  end,
  {noreply, State};
handle_cast(_Request, State = #state{}) ->
  lager:warning("unknown cast ~p", [_Request]),
  {noreply, State}.

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

read_number() ->
  read_number(now).

read_number(now) ->
  gen_server:cast(?SERVER, {read_number});
read_number(Delay) ->
  timer:apply_after(Delay, gen_server, cast, [{read_number}]).

check_prime(Number) ->
  gen_server:cast(?SERVER, {check_prime, Number}).
