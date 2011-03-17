%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

{application, roomerl,
 [{description, "roomerl"},
  {vsn, "0.01"},
  {modules, [
    roomerl,
    roomerl_app,
    roomerl_sup,
    roomerl_web_server_sup,
    roomerl_web_server,
    roomerl_deps,
    roomerl_rooms_web_handler,
    roomerl_rooms_sup,
    roomerl_rooms_manager,
    roomerl_room,
    supervisor_utility
  ]},
  {registered, []},
  {mod, {roomerl_app, []}},
  {env, [{web_server, [{host, "0.0.0.0"},
                       {port, 8080},
                       {backlog, 128},
                       {docroot, "priv/www"}]}]},
  {applications, [kernel, stdlib]}]}.
