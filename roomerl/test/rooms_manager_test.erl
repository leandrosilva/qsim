%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Functional test module for rooms.

-module(rooms_manager_test).
-author('Leandro Silva <leandrodoze@gmail.com>').

-include_lib("eunit/include/eunit.hrl").

%%
%% Describing rooms module ------------------------------------------------------------------------
%%
describe_test_() ->
  {"rooms_manager",
    [
      {"before all tests",
        {setup, fun before_all/0, []}},
        
      {"as a room manager",
        [
          {"should open a room given",
            fun should_open_a_room_given/0},

          {"should close a room given",
            fun should_close_a_room_given/0},
          
          {"should get a room given",
            fun should_get_a_room_given/0},

          {"should list all open rooms",
            fun should_list_all_open_rooms/0},

          {"should know whether a room is open or no",
            fun should_know_whether_a_room_is_open_or_no/0},

          {"should close all open rooms",
            fun should_close_all_open_rooms/0}
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

should_open_a_room_given() ->
  ensure_there_is_no_room(),
  
  ?assertMatch({ok, "123"}, rooms_manager:open_room("123")),
  ?assertMatch(["123"], rooms_manager:get_open_rooms()).

should_close_a_room_given() ->
  ensure_there_is_no_room(),
  
  ?assertMatch({ok, "123"}, rooms_manager:open_room("123")),
  ?assertMatch({ok, "456"}, rooms_manager:open_room("456")),
  
  ?assertMatch({ok, "123"}, rooms_manager:close_room("123")),
  ?assertMatch(["456"], rooms_manager:get_open_rooms()).

should_get_a_room_given() ->
  ensure_there_is_no_room(),

  ?assertMatch(unknow, rooms_manager:get_room("123")),
  ?assertMatch({ok, "123"}, rooms_manager:open_room("123")),
  ?assertMatch(room_123, rooms_manager:get_room("123")).

should_list_all_open_rooms() ->
  ensure_there_is_no_room(),
  
  rooms_manager:open_room("123"),
  rooms_manager:open_room("456"),
  rooms_manager:open_room("789"),
  
  ?assertMatch(["123", "456", "789"], rooms_manager:get_open_rooms()).

should_know_whether_a_room_is_open_or_no() ->
  ensure_there_is_no_room(),

  rooms_manager:open_room("123"),
  rooms_manager:open_room("456"),
  rooms_manager:open_room("789"),

  ?assertMatch(yes, rooms_manager:is_open_room("123")),
  ?assertMatch(yes, rooms_manager:is_open_room("456")),
  ?assertMatch(yes, rooms_manager:is_open_room("789")),
  ?assertMatch(no, rooms_manager:is_open_room("000")).
    
should_close_all_open_rooms() ->
  ensure_there_is_no_room(),

  rooms_manager:open_room("123"),
  rooms_manager:open_room("456"),
  rooms_manager:open_room("789"),

  ?assertMatch([], rooms_manager:close_all_rooms()),
  ?assertMatch([], rooms_manager:get_open_rooms()).
  
%%
%% Helper functions -------------------------------------------------------------------------------
%%

ensure_there_is_no_room() ->
  ?assertMatch([], rooms_manager:close_all_rooms()),
  ?assertMatch([], rooms_manager:get_open_rooms()),
  ok.
