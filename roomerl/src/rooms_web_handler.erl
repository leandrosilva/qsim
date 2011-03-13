%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc The misultin-based web handler module for handle HTTP and WebSocket requests on the roomerl
%%      application's rooms.

-module(rooms_web_handler, [DocRoot]).
-author('Leandro Silva <leandrodoze@gmail.com>').

% misultin web handler callbacks
-export([handle_http/3, handle_websocket/2]).

%%
%% Misultin-based Callbacks for roomerl_web -------------------------------------------------------
%%

% --- HTTP Routes to support handle_http callback -------------------------------------------------

% handle a GET on /rooms
handle_http('GET', ["rooms"], Req) ->
  Req:ok([{"Content-Type", "text/plain"}], "The Rooms page.");

% handle a GET on /rooms/{RoomId}
handle_http('GET', ["rooms", RoomId], Req) ->
  Req:ok([{"Content-Type", "text/plain"}], "This is the ~s room page.", [RoomId]);

% handle a GET on /rooms/{RoomId}/open
handle_http('GET', ["rooms", RoomId, "open"], Req) ->
  Req:ok([{"Content-Type", "text/plain"}], "Opening room ~s.", [RoomId]);

% handle a GET on /rooms/{RoomId}/close
handle_http('GET', ["rooms", RoomId, "close"], Req) ->
  Req:ok([{"Content-Type", "text/plain"}], "Closing room ~s.", [RoomId]);

% handle a GET on /rooms/{RoomId}/enter
handle_http('GET', ["rooms", _RoomId, "enter"], Req) ->
  Req:file(path_to_doc("room.html"));

% handle a GET on /rooms/{RoomId}/users
handle_http('GET', ["rooms", RoomId, "users"], Req) ->
  Req:ok([{"Content-Type", "text/plain"}], "This is ~s's users page.", [RoomId]);

% handle the 404 page not found
handle_http(_, _, Req) ->
  Req:ok([{"Content-Type", "text/plain"}], "Page not found.").

% --- WebSockets Routes to support handle_websocket callback --------------------------------------

% handle /chat PATH
handle_websocket(["rooms", RoomId], Ws) ->
  receive
    {browser, Data} ->
      % io:format("[websocket_handler = ~w, RoomId = ~p] received ~p~n", [self(), RoomId, Data]),

      Ws:send(["received '", Data, "'"]),
      handle_websocket(["rooms", RoomId], Ws);
    closed ->
      % io:format("[websocket_handler = ~w, RoomId = ~p] The WebSocket was CLOSED!~n", [self(), RoomId]),

      closed;
    _Ignore ->
      handle_websocket(["rooms", RoomId], Ws)
  after 5000 ->
    % io:format("[websocket_handler = ~p, RoomId = ~p] pushing~n", [self(), RoomId]),

    Ws:send("pushing!"),
    handle_websocket(["rooms", RoomId], Ws)
  end.

%%
%% Internal API -----------------------------------------------------------------------------------
%%

path_to_doc(File) ->
  DocRoot ++ "/" ++ File.
