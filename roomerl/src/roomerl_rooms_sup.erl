%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Supervisor for the rooms domain of the roomerl application.

-module(roomerl_rooms_sup).
-author('Leandro Silva <leandrodoze@gmail.com>').

-behaviour(supervisor).

% admin api
-export([start_link/0, upgrade/0]).
% public api
-export([start_child/1, stop_child/1]).
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
%% @doc Remove and add processes if necessary.
upgrade() ->
  supervisor_utility:upgrade(?MODULE).

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec start_child(RoomId) -> {ok, ChildPid} | {ok, ChildPid, Info} | {error, Error}
%% @dynamic Start a roomerl_rooms process to serve a room given.
start_child(RoomId) ->
  RoomName = roomerl_rooms:get_name(RoomId),

  RoomSpec = {RoomName, {roomerl_rooms, start_link, [RoomId]}, permanent, 5000, worker, dynamic},
  supervisor:start_child(roomerl_rooms_sup, RoomSpec).

%% @spec stop_child(RoomId) -> ok | {error, Error}
%% @dynamic Stop a roomerl_rooms process to serve a room given.
stop_child(RoomId) ->
  RoomName = roomerl_rooms:get_name(RoomId),
  
  supervisor:terminate_child(roomerl_rooms_sup, RoomName),
  supervisor:delete_child(roomerl_rooms_sup, RoomName).

%%
%% Supervisor Callback ----------------------------------------------------------------------------
%%

%% @spec init([]) -> SupervisorTree = {ok, {SupervisorSpec, [ChildSpec]}} | ignore
%%
%% Types:
%%
%%     SupervisorSpec = {RestartStrategy, AllowedRestarts, MaxSeconds}
%%     ChildSpec = {Id, {Module, Function, Arguments}, Restart, Shutdown, Type, ModuleList}
%%
%% @doc supervisor callback.
init([]) ->
  {ok, {{one_for_one, 10, 10}, []}}.
