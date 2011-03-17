%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Functional test module for room.

-module(roomerl_rooms_test).
-author('Leandro Silva <leandrodoze@gmail.com>').

-include_lib("eunit/include/eunit.hrl").

%%
%% Describing room module -------------------------------------------------------------------------
%%
describe_test_() ->
  {"roomerl_rooms",
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
          
          {"should say welcome to a user",
            fun should_say_welcome_to_a_user/0},

          {"should say goodbye to a user",
            fun should_say_goodbye_to_a_user/0},

          {"should know whether a user is present or no",
            fun should_know_whether_a_user_is_present_or_no/0},
          
          {"should publish messages",
            fun should_publish_messages/0}
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
  ?assertMatch(roomerl_rooms_123, roomerl_rooms:get_name("123")).

should_know_whether_is_open_or_no() ->
  ensure_room_is_close("123"),
  
  ?assertMatch(no, roomerl_rooms:is_open("123")),
  ?assertMatch({ok, roomerl_rooms_123}, roomerl_rooms:open("123")),
  ?assertMatch(yes, roomerl_rooms:is_open("123")).
  
should_can_ben_open() ->
  ensure_room_is_close("123"),
  
  ?assertMatch({ok, roomerl_rooms_123}, roomerl_rooms:open("123")).

should_can_be_close() ->
  ensure_room_is_close("123"),
  
  ?assertMatch({ok, roomerl_rooms_123}, roomerl_rooms:open("123")),
  ?assertMatch(ok, roomerl_rooms:close("123")).

should_say_welcome_to_a_user() ->
  ?assertMatch(yes, no).

should_say_goodbye_to_a_user() ->
  ?assertMatch(yes, no).

should_know_whether_a_user_is_present_or_no() ->
  ?assertMatch(yes, no).

should_publish_messages() ->
  ?assertMatch(yes, no).
    
%%
%% Helper functions -------------------------------------------------------------------------------
%%

ensure_room_is_close(RoomId) ->
  ?assertMatch(ok, roomerl_rooms:close(RoomId)),
  
  timer:sleep(1),
  ok.
