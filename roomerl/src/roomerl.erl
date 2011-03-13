%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Startup for roomerl application.

-module(roomerl).
-author('Leandro Silva <leandrodoze@gmail.com>').

-export([start/0, stop/0, upgrade/0]).
-export([get_basedir/0, get_web_config/0]).

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

%% @spec get_web_config() -> string()
%% @doc The roomerl web server configuration.
get_web_config() ->
  get_env(web).

%%
%% Internal API -----------------------------------------------------------------------------------
%%

%% @spec get_env(Name) -> Val | undefined
%% @doc Get a configuration parameter of this application.
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
  