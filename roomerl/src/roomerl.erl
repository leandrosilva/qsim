%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Startup for roomerl application.

-module(roomerl).
-author('Leandro Silva <leandrodoze@gmail.com>').

-export([start/0, stop/0, upgrade/0]).
-export([get_basedir/0, get_host/0, get_port/0, get_backlog/0, get_docroot/0]).

%%
%% Admin API --------------------------------------------------------------------------------------
%%
        
%% @spec start() -> ok
%% @doc Start the roomerl server.
start() ->
  roomerl_deps:ensure(),
  application:start(?MODULE).

%% @spec stop() -> ok
%% @doc Stop the roomerl server.
stop() ->
  _Res = application:stop(?MODULE).

%% @spec upgrade() -> ok
%% @doc Upgrade the roomerl server code.
upgrade() ->
  upgrade_code(),
  upgrade_app().

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec get_basedir() -> string()
%% @doc Return the application directory for the roomerl server.
get_basedir() ->
  {file, Here} = code:is_loaded(?MODULE),
  filename:dirname(filename:dirname(Here)).

%% @spec get_host() -> string()
%% @doc The roomerl server IP.
get_host() ->
  get_env(host).

%% @spec get_port() -> int()
%% @doc The roomerl server HTTP port.
get_port() ->
  get_env(port).

%% @spec get_backlog() -> int()
%% @doc The maximum length that the queue of pending connections to roomerl
%%      server may grow to.
get_backlog() ->
  get_env(backlog).

%% @spec get_docroot() -> string()
%% @doc The relative path to documents in the roomerl server.
get_docroot() ->
  get_env(docroot).  

%%
%% Internal API -----------------------------------------------------------------------------------
%%

%% @spec get_env(Name) -> Val | undefined
%% @doc Get an configuration parameter of this application.
get_env(Name) ->
  case application:get_env(?MODULE, Name) of
    {ok, Value} ->
      Value;
    undefined ->
      undefined
  end.

%% @spec upgrade_code() -> [{module, Module}]
%% @doc Upgrade the roomerl server code.
upgrade_code() ->
  {ok, LoadedModules} = application:get_key(roomerl, modules),
  
  [code:purge(Module) || Module <- LoadedModules],
  [code:load_file(Module) || Module <- LoadedModules].
  
%% @spec upgrade_app() -> [{module, Module}]
%% @doc Upgrade the roomerl server application.
upgrade_app() ->
  {ok, {AppName, _}} = application:get_key(?MODULE, mod),
  AppName:upgrade().
  