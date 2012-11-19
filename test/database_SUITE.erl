-module(database_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("flow.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([create_float/1, find_float/1, find_or_create_float/1, merge_floats/1]).
-export([create_drop/1, find_drop/1, find_drops/1]).
-export([create_flow/1, change_title/1, add_floats/1, delete_floats/1, find_flow/1, find_flows/1, find_drop_of/1]).

all() -> [
    create_float, find_float, find_or_create_float, merge_floats,
    create_drop, find_drop, find_drops,
    create_flow, change_title, add_floats, delete_floats, find_flow, find_flows, find_drop_of
  ].

init_per_suite(Config) ->
  application:set_env(mnesia, dir, ?config(priv_dir, Config)),
  ok = mnesia:create_schema([node()]),
  ok = mnesia:start(),
  ok = flow_database:create(),
  flow_database:wait_for_tables(),

  Config.

end_per_suite(_Config) ->
  ok = flow_database:delete(),
  stopped = mnesia:stop(),
  mnesia:delete_schema([node()]),

  ok.

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, _Config) ->
  ok.

create_float(_Config) ->
  {atomic, #flow_float{name = "wat", properties = []}}
    = flow_database:create_float("wat"),

  {atomic, #flow_float{name = "omg", properties = [announcement]}}
    = flow_database:create_float("omg", [announcement]),

  ok.

find_float(_Config) ->
  {atomic, #flow_float{name = "wat"}}
    = flow_database:find_float("wat"),

  {atomic, #flow_float{name = "omg", properties = [announcement]}}
    = flow_database:find_float("omg"),

  {atomic, undefined}
    = flow_database:find_float("lol"),

  ok.

find_or_create_float(_Config) ->
  {atomic, #flow_float{name = "wat"}}
    = flow_database:find_or_create_float("wat"),

  {atomic, #flow_float{name = "omg", properties = [announcement]}}
    = flow_database:find_or_create_float("omg"),

  {atomic, #flow_float{name = "lol"}}
    = flow_database:find_or_create_float("lol"),

  ok.

merge_floats(_Config) ->
  {skip, not_implemented}.

create_drop(_Config) ->
  Now = calendar:now_to_universal_time(now()),

  {atomic, #flow_drop{id = 1, date = Now, parent = undefined}}
    = flow_database:create_drop("konnichiwa"),

  {atomic, #flow_drop{id = 2, date = Now, parent = 1}}
    = flow_database:create_drop(1, "o hai"),

  ok.

find_drop(_Config) ->
  {atomic, #flow_drop{id = 1, parent = undefined, children = [2]}}
    = flow_database:find_drop(1),

  {atomic, #flow_drop{id = 2, parent = 1}}
    = flow_database:find_drop(2),

  {atomic, undefined}
    = flow_database:find_drop(3),

  ok.

find_drops(_Config) ->
  {atomic, [#flow_drop{id = 1, parent = undefined, children = [2]}, #flow_drop{id = 2, parent = 1} | _]}
    = flow_database:find_drops([1, 2, 3]),

  ok.

create_flow(_Config) ->
  {aborted, no_floats}
    = flow_database:create_flow("lol", "wut", []),

  {atomic, #flow_flow{id = 1, title = "lol", drop = 1, floats = ["a", "b"]}}
    = flow_database:create_flow("lol", 1, ["a", "b"]),

  {atomic, #flow_flow{id = 2, title = "lol", drop = 3, floats = ["a", "b"]}}
    = flow_database:create_flow("lol", "Don't lose your dinosaur", ["a", "b"]),

  ok.

change_title(_Config) ->
  {atomic, #flow_flow{id = 1, title = "omg", drop = 1, floats = ["a", "b"]}}
    = flow_database:change_title(1, "omg"),

  ok.

add_floats(_Config) ->
  {atomic, #flow_flow{id = 1, title = "omg", drop = 1, floats = ["a", "b", "c"]}}
    = flow_database:add_floats(1, ["b", "c"]),

  ok.

delete_floats(_Config) ->
  {atomic, #flow_flow{id = 1, title = "omg", drop = 1, floats = ["a", "c"]}}
    = flow_database:delete_floats(1, ["b"]),

  ok.

find_flow(_Config) ->
  {atomic, #flow_flow{id = 1, title = "omg", drop = 1, floats = ["a", "c"]}}
    = flow_database:find_flow(1),

  {atomic, #flow_flow{id = 2, title = "lol", drop = 3, floats = ["a", "b"]}}
    = flow_database:find_flow(2),

  ok.

find_flows(_Config) ->
  {atomic, [1, 2]}
    = flow_database:find_flows("b || c"),

  {atomic, [2]}
    = flow_database:find_flows("a && !c"),

  ok.

find_drop_of(_Config) ->
  {atomic, #flow_drop{id = 1}}
    = flow_database:find_drop_of(1),

  {atomic, #flow_drop{id = 3}}
    = flow_database:find_drop_of(2),

  ok.
