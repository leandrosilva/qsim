%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Functional test module for roomerl.
%%      The reference module is roomerl. It's the main module.

-module(roomerl_test).
-author('Leandro Silva <leandrodoze@gmail.com>').

-include_lib("eunit/include/eunit.hrl").

%%
%% Describing roomerl module ----------------------------------------------------------------------
%%
describe_roomerl_test_() ->
  {"roomerl",
    [
      {"before all tests",
        {setup, fun before_all/0, []}},
        
      {"having a public API",
        [
          {"should has get_basedir()",
            fun should_has_get_basedir/0},
              
          {"should has get_host()",
            fun should_has_get_host/0},
              
          {"should has get_port()",
            fun should_has_get_port/0},

          {"should has get_backlog()",
            fun should_has_get_backlog/0},

          {"should has get_docroot",
            fun should_has_get_docroot/0}
        ]},

      {"after all tests",
        {setup, fun after_all/0, []}}
    ]}.

%%
%% Setup ------------------------------------------------------------------------------------------
%%

before_all() ->
  ok.

after_all() ->
  ok.

%%
%% Scenary: having a public API -------------------------------------------------------------------
%%

should_has_get_basedir() ->
  ?assertMatch("roomerl", lists:last(string:tokens(roomerl:get_basedir(), "/"))).

should_has_get_host() ->
  ?assertMatch("127.0.0.1", roomerl:get_host()).

should_has_get_port() ->
  ?assertMatch(8008, roomerl:get_port()).

should_has_get_backlog() ->
  ?assertMatch(111, roomerl:get_backlog()).

should_has_get_docroot() ->
  ?assertMatch("priv/www", roomerl:get_docroot()).
