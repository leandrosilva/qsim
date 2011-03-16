%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Functional test module for room.

-module(room_test).
-author('Leandro Silva <leandrodoze@gmail.com>').

-include_lib("eunit/include/eunit.hrl").

%%
%% Describing room module -------------------------------------------------------------------------
%%
describe_test_() ->
  {"room",
    [
      {"before all tests",
        {setup, fun before_all/0, []}},
        
      {"as a room",
        [
          {"should have a name",
            fun should_have_a_name/0},

          {"should know whether is open or no",
            fun should_know_whether_is_open_or_no/0},

          {"should can be open",
            fun should_can_ben_open/0},

          {"should can be close",
            fun should_can_be_close/0},
          
          {"should say welcome to a student",
            fun should_say_welcome_to_a_student/0},

          {"should say goodbye to a student",
            fun should_say_goodbye_to_a_student/0},

          {"should know whether a student is present or no",
            fun should_know_whether_a_student_is_present_or_no/0}
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
%% Scenary: as a room manager ---------------------------------------------------------------------
%%

should_have_a_name() ->
  Room = room:new("123"),
  
  ?assertMatch(room_123, Room:get_name()).

should_know_whether_is_open_or_no() ->
  Room = room:new("123"),
  ensure_room_is_close(Room),
  
  ?assertMatch(no, Room:is_open()),
  ?assertMatch({ok, _}, Room:open()),
  ?assertMatch(yes, Room:is_open()).
  
should_can_ben_open() ->
  Room = room:new("123"),
  ensure_room_is_close(Room),
  
  ?assertMatch({ok, _}, Room:open()).

should_can_be_close() ->
  Room = room:new("123"),
  ensure_room_is_close(Room),
  
  ?assertMatch({ok, _}, Room:open()),
  ?assertMatch(ok, Room:close()).

should_say_welcome_to_a_student() ->
  ?assertMatch(yes, no).

should_say_goodbye_to_a_student() ->
  ?assertMatch(yes, no).

should_know_whether_a_student_is_present_or_no() ->
  ?assertMatch(yes, no).
  
%%
%% Helper functions -------------------------------------------------------------------------------
%%

ensure_room_is_close(Room) ->
  ?assertMatch(ok, Room:close()),
  
  timer:sleep(1),
  ok.
