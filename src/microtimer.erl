-module(microtimer).
-author("regikul").


%% API
-export([
  send_after/3,
  send_interval/3,
  cancel/1
]).

-on_load(init/0).

send_after(_Time, _Pid, _Message) ->
  erlang:nif_error('not implemented').

send_interval(_Time, _Pid, _Message) ->
  erlang:nif_error('not implemented').

cancel(_Timer) ->
  erlang:nif_error('not implemented').

init() ->
  PrivDir = code:priv_dir(funbox),
  ok = erlang:load_nif(filename:join(PrivDir, "crates/microtimer/libmicrotimer"), 0).