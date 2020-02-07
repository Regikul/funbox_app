%%%-------------------------------------------------------------------
%%% @author regikul
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Feb 2020 20:13
%%%-------------------------------------------------------------------
-module(primes).
-author("regikul").

%% API
-export([check/1]).
-on_load(init/0).

check(Number) when is_integer(Number) ->
  erlang:nif_error('not implemented').

init() ->
  PrivDir = code:priv_dir(funbox),
  ok = erlang:load_nif(filename:join(PrivDir, "crates/primes/libprimes"), 0).
