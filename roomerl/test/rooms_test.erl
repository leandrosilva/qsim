%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Functional test module for rooms.

-module(rooms_test).
-author('Leandro Silva <leandrodoze@gmail.com>').

-include_lib("eunit/include/eunit.hrl").

%%
%% Describing rooms module ----------------------------------------------------------------------
%%
describe_test_() ->
  {"rooms",
    [
      {"before all tests",
        {setup, fun before_all/0, []}},
        
      {"as a room manager",
        [
          {"should open a room given",
            fun should_open_a_room_given/0},

          {"should close a room given",
            fun should_close_a_room_given/0},
              
          {"should list all open rooms",
            fun should_list_all_open_rooms/0},

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
  
  ?assertMatch(["123"], rooms:open_room("123")),
  ?assertMatch(["123"], rooms:get_open_rooms()).

should_close_a_room_given() ->
  ensure_there_is_no_room(),
  
  ?assertMatch(["123"], rooms:open_room("123")),
  ?assertMatch(["123", "456"], rooms:open_room("456")),
  
  ?assertMatch(["456"], rooms:close_room("123")),
  ?assertMatch(["456"], rooms:get_open_rooms()).

should_list_all_open_rooms() ->
  ensure_there_is_no_room(),
  
  rooms:open_room("123"),
  rooms:open_room("456"),
  rooms:open_room("789"),
  
  ?assertMatch(["123", "456", "789"], rooms:get_open_rooms()).

should_close_all_open_rooms() ->
  ensure_there_is_no_room(),

  rooms:open_room("123"),
  rooms:open_room("456"),
  rooms:open_room("789"),

  ?assertMatch([], rooms:close_all_rooms()),
  ?assertMatch([], rooms:get_open_rooms()).

%%
%% Helper functions -------------------------------------------------------------------------------
%%

ensure_there_is_no_room() ->
  ?assertMatch([], rooms:close_all_rooms()),
  ?assertMatch([], rooms:get_open_rooms()),
  ok.
