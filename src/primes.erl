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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([check/1]).
-on_load(init/0).

check(Number) when is_integer(Number) ->
  erlang:nif_error('not implemented').

init() ->
  PrivDir = code:priv_dir(funbox),
  ok = erlang:load_nif(filename:join(PrivDir, "crates/primes/libprimes"), 0).

-ifdef(TEST).

check_test_() ->
  [
    ?_assert(check(3)),
    ?_assert(check(5)),
    ?_assert(check(7)),
    ?_assertNot(check(8)),
    ?_assertException(error, badarg, check("NAN"))
  ].

-endif.

