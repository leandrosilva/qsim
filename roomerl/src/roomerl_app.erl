%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Callbacks for the roomerl application.

-module(roomerl_app).
-author('Leandro Silva <leandrodoze@gmail.com>').

-behaviour(application).

% admin api
-export([start/2, stop/1, upgrade/0]).

%%
%% Admin API --------------------------------------------------------------------------------------
%%

%% @spec start(_Type, _StartArgs) -> {ok, SupervisorPid} | ignore | {error, Error}
%% @doc application start callback for roomerl.
start(_Type, _StartArgs) ->
  roomerl_deps:ensure(),
  roomerl_sup:start_link().

%% @spec stop(_State) -> ok
%% @doc application stop callback for roomerl.
stop(_State) ->
  ok.

%% @spec upgrade() -> ok
%% @doc Upgrade the roomerl application code.
upgrade() ->
  roomerl_sup:upgrade().
  