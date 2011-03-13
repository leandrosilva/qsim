%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Supervisor for the roomerl application.

-module(roomerl_sup).
-author('Leandro Silva <leandrodoze@gmail.com>').

-behaviour(supervisor).

% admin api
-export([start_link/0, upgrade/0]).
% supervisor callback
-export([init/1]).

%%
%% Admin API --------------------------------------------------------------------------------------
%%

%% @spec start_link() -> Result = {ok, Pid} | ignore | {error, Error}
%% @doc API for starting the supervisor.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
  {ok, {_, Specs}} = init([]),

  Old = sets:from_list([Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
  New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),

  % kill children processes that doesn't exists anymore
  % I mean, they exist in the Old spec but no longer in the New spec
  Kill = sets:subtract(Old, New),

  sets:fold(fun (Id, ok) ->
              supervisor:terminate_child(?MODULE, Id),
              supervisor:delete_child(?MODULE, Id),
              ok
            end, ok, Kill),

  [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
  ok.

%%
%% Supervisor Callback ----------------------------------------------------------------------------
%%

%% @spec init([]) -> SupervisorTree = {ok, {{RestartStrategy, MaxR, MaxT}, [ChildSpec]}} | ignore
%% @doc supervisor callback.
init([]) ->
  WebConfig = roomerl:get_web_config(),
  Web = {roomerl_web, {roomerl_web, start_link, [WebConfig]}, permanent, 5000, worker, dynamic},

  Processes = [Web],
  {ok, {{one_for_one, 10, 10}, Processes}}.
