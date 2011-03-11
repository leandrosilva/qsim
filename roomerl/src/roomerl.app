%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

{application, roomerl,
 [{description, "roomerl"},
  {vsn, "0.01"},
  {modules, [
    roomerl,
    roomerl_app,
    roomerl_sup,
    roomerl_web,
    roomerl_deps
  ]},
  {registered, []},
  {mod, {roomerl_app, []}},
  {env, [{host, "0.0.0.0"},
         {port, 8080},
         {backlog, 128},
         {docroot, "priv/www"}]},
  {applications, [kernel, stdlib]}]}.
