%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

-module(roomerl_web).
-export([start/1, stop/0]).

%%
%% Admin API --------------------------------------------------------------------------------------
%%

% start misultin http server
start(Options) ->
  [{host, Host},
   {port, Port},
   {backlog, BackLog},
   {docroot, _DocRoot}] = Options,
  
	misultin:start_link([{host, Host},
	                     {port, Port},
	                     {backlog, BackLog},
	                     {loop, fun(Req) -> handle_http(Req) end},
                       {ws_loop, fun(Ws) -> handle_websocket(Ws) end},
                       {ws_autoexit, false}]).

% stop misultin
stop() ->
	misultin:stop().

%%
%% Misultin Callback ------------------------------------------------------------------------------
%%

% callback on request received
handle_http(Req) ->
  % dispatch to routes
  Method = Req:get(method),
  Resource = Req:resource([lowercase, urldecode]),
  handle(Method, Resource, Req).
  
% callback on received websockets data
handle_websocket(Ws) ->
  % dispatch to routes
  Path = string:tokens(Ws:get(path), "/"),
	handle_websocket(Path, Ws).

% --- HTTP Routes ---------------------------------------------------------------------------------

% handle a GET on /
handle('GET', [], Req) ->
	Req:ok([{"Content-Type", "text/plain"}], "qsin:roomerl");

% handle a GET on /rooms
handle('GET', ["rooms"], Req) ->
	Req:ok([{"Content-Type", "text/plain"}], "The Rooms page.");

% handle a GET on /rooms/{RoomId}
handle('GET', ["rooms", RoomId], Req) ->
	Req:ok([{"Content-Type", "text/plain"}], "This is the ~s room page.", [RoomId]);

% handle a GET on /rooms/{RoomId}/getin
handle('GET', ["rooms", _RoomId, "getin"], Req) ->
	Req:file(path_to_doc("room.html"));

% handle a GET on /rooms/{RoomId}/users
handle('GET', ["rooms", RoomId, "users"], Req) ->
	Req:ok([{"Content-Type", "text/plain"}], "This is ~s's users page.", [RoomId]);

% handle the 404 page not found
handle(_, _, Req) ->
	Req:ok([{"Content-Type", "text/plain"}], "Page not found.").

% --- WebSockets Routes ---------------------------------------------------------------------------

% handle /chat PATH
handle_websocket(["rooms", RoomId], Ws) ->
	receive
		{browser, Data} ->
      io:format("[websocket_handler = ~w, RoomId = ~p] received ~p~n", [self(), RoomId, Data]),

			Ws:send(["received '", Data, "'"]),
			handle_websocket(["rooms", RoomId], Ws);
		closed ->
			io:format("[websocket_handler = ~w, RoomId = ~p] The WebSocket was CLOSED!~n", [self(), RoomId]),

			closed;
		_Ignore ->
			handle_websocket(["rooms", RoomId], Ws)
	after 5000 ->
		io:format("[websocket_handler = ~p, RoomId = ~p] pushing~n", [self(), RoomId]),

		Ws:send("pushing!"),
		handle_websocket(["rooms", RoomId], Ws)
	end.

%%
%% Internal API -----------------------------------------------------------------------------------
%%

path_to_doc(File) ->
  [_, _, _, {docroot, DocRoot}] = roomerl:get_web_config(),
  DocRoot ++ "/" ++ File.
