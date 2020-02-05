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

%%-define(TICK_TIME, 1000000 div 3000). %% 3000 times per second

-define(TICK_TIME, 1000000 div 3). %% 3 times per second

-record(state, {
  redis :: pid(),
  timer
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
  {ok, Redis} = eredis:start_link(Host, Port, DB),
  Timer = microtimer:send_interval({tick}, self(), ?TICK_TIME),
  {ok, #state{
    redis = Redis,
    timer = Timer
  }}.

handle_call(_Request, _From, State = #state{}) ->
  lager:warning("unknown call ~p", [_Request]),
  {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
  lager:warning("unknown cast ~p", [_Request]),
  {noreply, State}.

handle_info({tick}, State) ->
  lager:info("Got a tick!"),
  {noreply, State};
handle_info(_Info, State = #state{}) ->
  lager:warning("unknown info ~p", [_Info]),
  {noreply, State}.

terminate(_Reason, _State = #state{timer = Timer}) ->
  microtimer:cancel(Timer).

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
