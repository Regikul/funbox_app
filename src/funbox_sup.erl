%%%-------------------------------------------------------------------
%% @doc funbox_app top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(funbox_sup).

-include("funbox.hrl").

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
      strategy => one_for_all,
      intensity => 0,
      period => 1
    },
    ChildSpecs = [
      ?WORKER(fb_filter),
      ?WORKER(fb_generator)
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
