%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc The rooms manager of the roomerl application.

-module(rooms).
-author('Leandro Silva <leandrodoze@gmail.com>').

-behaviour(gen_server).

% admin api
-export([start_link/1, stop/0]).
% public api
-export([open_room/1, close_room/1, get_room/1, get_open_rooms/0, is_open_room/1, close_all_rooms/0]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {rooms}).

%%
%% Admin API --------------------------------------------------------------------------------------
%%

%% @spec start_link(_Options) -> {ok, Pid} | ignore | {error, Error}
%% @doc Start rooms manager.
start_link(_Options) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec stop() -> ok
%% @doc Manually stops the server.
stop() ->
  gen_server:cast(?MODULE, stop).

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec open_room(RoomId) -> {ok, RoomId}
%% @doc Opens a room given.
open_room(RoomId) ->
  gen_server:call(?MODULE, {do, open_room, RoomId}).

%% @spec close_room(RoomId) -> {ok, RoomId}
%% @doc Closes a room given.
close_room(RoomId) ->
  gen_server:call(?MODULE, {do, close_room, RoomId}).

%% @spec get_room(RoomId) -> atom()
%% @doc Returns the registered name to a room given.
get_room(RoomId) ->
  gen_server:call(?MODULE, {get, room, RoomId}).

%% @spec get_open_rooms() -> [Room]
%% @doc List currently open rooms.
get_open_rooms() ->
  gen_server:call(?MODULE, {get, open_rooms}).

%% @spec is_open_room(RoomId) -> yes | no
%% @doc Is room open?
is_open_room(RoomId) ->
  gen_server:call(?MODULE, {get, is_open_room, RoomId}).

%% @spec close_all_rooms() -> ok
%% @doc Close all open rooms.
close_all_rooms() ->
  gen_server:call(?MODULE, {do, close_all_rooms}).

%%
%% Gen_Server Callbacks ---------------------------------------------------------------------------
%%

%% @spec init(_Options) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
%% @doc Initiates the server.
init(_Options) ->
  process_flag(trap_exit, true),
  erlang:monitor(process, rooms),
  
  {ok, #state{rooms = []}}.

%% @spec handle_call(Request, From, State) ->
%%                  {reply, Reply, State} | {reply, Reply, State, Timeout} | {noreply, State} |
%%                  {noreply, State, Timeout} | {stop, Reason, Reply, State} | {stop, Reason, State}
%% @doc Handling call messages.

% opens a room given
handle_call({do, open_room, RoomId}, _From, State) ->
  OpenRooms = lists:reverse([RoomId | lists:reverse(State#state.rooms)]),
  NewState = State#state{rooms = OpenRooms},
  
  {reply, {ok, RoomId}, NewState};

% opens a room given
handle_call({do, close_room, RoomId}, _From, State) ->
  OpenRooms = lists:delete(RoomId, State#state.rooms),
  NewState = State#state{rooms = OpenRooms},
  
  {reply, {ok, RoomId}, NewState};

% get a room given
handle_call({get, room, RoomId}, _From, State) ->
  OpenRooms = State#state.rooms,

  case lists:member(RoomId, OpenRooms) of
    true -> Name = list_to_atom("room_" ++ RoomId);
    false -> Name = unknow
  end,
  
  {reply, Name, State};

% return currently open rooms
handle_call({get, open_rooms}, _From, State) ->
  {reply, State#state.rooms, State};

% return whether a room is open
handle_call({get, is_open_room, RoomId}, _From, State) ->
  OpenRooms = State#state.rooms,

  case lists:member(RoomId, OpenRooms) of
    true -> Found = yes;
    false -> Found = no
  end,

  {reply, Found, State};

% close all open rooms
handle_call({do, close_all_rooms}, _From, State) ->
  NewState = State#state{rooms = []},

  {reply, [], NewState};

% handle_call generic fallback
handle_call(_Request, _From, State) ->
  {reply, undefined, State}.

%% @spec handle_cast(Msg, State) ->
%%                  {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
%% @doc Handling cast messages.

% manual shutdown
handle_cast(stop, State) ->
  {stop, normal, State};

% handle_cast generic fallback (ignore)
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @spec handle_info(Info, State) ->
%%                  {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
%% @doc Handling all non call/cast messages.

% handle info when misultin server goes down -> take down misultin_gen_server too [the supervisor
% will take everything up again]
handle_info({'DOWN', _, _, {misultin, _}, _}, State) ->
  {stop, normal, State};

% handle_info generic fallback (ignore)
handle_info(_Info, State) ->
  {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to terminate. When it returns,
%%      the gen_server terminates with Reason. The return value is ignored.
terminate(_Reason, _State) ->
  misultin:stop(),
  terminated.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%
%% Internal API -----------------------------------------------------------------------------------
%%
