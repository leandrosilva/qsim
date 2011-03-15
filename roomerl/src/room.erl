%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc The room abstraction for the roomerl application.

-module(room, [Id]).
-author('Leandro Silva <leandrodoze@gmail.com>').

-behaviour(gen_server).

% public api
-export([open/0, close/0, welcome_student/1, goodbye_student/1, is_present_student/1]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(ROOM_ID, list_to_atom(?MODULE ++ "_" ++ Id)).
-record(state, {students}).

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec open() -> {ok, Pid} | ignore | {error, Error}
%% @doc Opens a room given. It's equivalent to start_link in a typicall gen_server implementation.
open() ->
  gen_server:start_link({local, ?ROOM_ID}, ?MODULE, [], []).

%% @spec stop() -> ok
%% @doc Manually stops the server. It's equivalent to stop in a typicall gen_server implementation.
close() ->
  gen_server:cast(?ROOM_ID, stop).

%% @spec welcome_student(Student) -> ok | {error, closed_room} | {error, Error}
%% @doc Receives a student given.
welcome_student(Student) ->
  gen_server:call(?MODULE, {do, welcome_student, Student}).

%% @spec goodbye_student(Student) -> ok | {error, closed_room} | {error, Error}
%% @doc Say bye-bye to a student given.
goodbye_student(Student) ->
  gen_server:call(?MODULE, {do, goodbye_student, Student}).

%% @spec is_present_student(Student) -> yes | no | {error, closed_room} | {error, Error}
%% @doc Verify whether a student given is present.
is_present_student(Student) ->
  gen_server:call(?MODULE, {do, is_present_student, Student}).

%%
%% Gen_Server Callbacks ---------------------------------------------------------------------------
%%

%% @spec init(_Options) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
%% @doc Initiates the server.
init(_Options) ->
  process_flag(trap_exit, true),
  erlang:monitor(process, room),
  
  {ok, #state{students = []}}.

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
handle_call({do, is_present_student, _Student}, _From, State) ->
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
