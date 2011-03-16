%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc The room abstraction for the roomerl application.

-module(room).
-author('Leandro Silva <leandrodoze@gmail.com>').

-behaviour(gen_server).

% public api
-export([get_name/1, is_open/1, open/1, close/1, welcome_student/2, goodbye_student/2, has_student/2]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {id, name, students}).

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec get_name(RoomId) -> atom()
%% @doc Name of room by its id.
get_name(RoomId) ->
  ModuleName = atom_to_list(?MODULE),
  RoomName = ModuleName ++ "_" ++ RoomId,
  
  list_to_atom(RoomName).

%% @spec is_open(RoomId) -> yes | no
%% @doc The room is open or no?
is_open(RoomId) ->
  case whereis(get_name(RoomId)) of
    undefined -> no;
    _ -> yes
  end.

%% @spec open(RoomId) -> {ok, Pid} | ignore | {error, Error}
%% @doc Opens a room given. It's equivalent to start_link in a typicall gen_server implementation.
open(RoomId) ->
  RoomName = get_name(RoomId),
  
  case gen_server:start_link({local, RoomName}, ?MODULE, RoomId, []) of
    {error, {already_started, Pid}} ->
      {error, already_open, Pid};
    Result ->
      Result
  end.

%% @spec stop(RoomId) -> ok
%% @doc Manually stops the server. It's equivalent to stop in a typicall gen_server implementation.
close(RoomId) ->
  RoomName = get_name(RoomId),
  gen_server:cast(RoomName, stop).

%% @spec welcome_student(Student, RoomId) -> ok | {error, closed_room} | {error, Error}
%% @doc Receives a student given.
welcome_student(Student, RoomId) ->
  RoomName = get_name(RoomId),
  gen_server:cast(RoomName, {do, welcome_student, Student}).

%% @spec goodbye_student(Student, RoomId) -> ok | {error, closed_room} | {error, Error}
%% @doc Say bye-bye to a student given.
goodbye_student(Student, RoomId) ->
  RoomName = get_name(RoomId),
  gen_server:cast(RoomName, {do, goodbye_student, Student}).

%% @spec has_student(Student, RoomId) -> yes | no | {error, closed_room} | {error, Error}
%% @doc Verify whether a student given is present.
has_student(Student, RoomId) ->
  RoomName = get_name(RoomId),
  gen_server:cast(RoomName, {do, has_student, Student}).

%%
%% Gen_Server Callbacks ---------------------------------------------------------------------------
%%

%% @spec init(RoomId) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
%% @doc Initiates the server.
init(RoomId) ->
  RoomName = get_name(RoomId),
  
  process_flag(trap_exit, true),
  erlang:monitor(process, RoomName),
  
  {ok, #state{id = RoomId, name = RoomName, students = []}}.

%% @spec handle_call(Request, From, State) ->
%%                  {reply, Reply, State} | {reply, Reply, State, Timeout} | {noreply, State} |
%%                  {noreply, State, Timeout} | {stop, Reason, Reply, State} | {stop, Reason, State}
%% @doc Handling call messages.

% say welcome to a student given
handle_call({do, welcome_student, _Student}, _From, State) ->
  {reply, _Student, State};

% say bye-bye to a student given
handle_call({do, goodbye_student, _Student}, _From, State) ->
  {reply, _Student, State};

% this student is present here?
handle_call({do, has_student, _Student}, _From, State) ->
  {reply, yes_or_no, State};

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
