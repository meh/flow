-module(database_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("flow.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([create_float/1, find_float/1, find_or_create_float/1, merge_floats/1]).
-export([create_drop/1, find_drop/1, find_drops/1]).
-export([create_flow/1, change_title/1, add_floats/1, delete_floats/1, find_flow/1, find_flows/1, find_drop_of/1, fetch_tree/1, sort_flows_by/1, find_last_update/1]).
-export([create_moderator/1, delete_moderator/1, is_moderator/1]).

all() -> [
    create_float, find_float, find_or_create_float, merge_floats,
    create_drop, find_drop, find_drops,
    create_flow, change_title, add_floats, delete_floats, find_flow, find_flows, find_drop_of, fetch_tree, sort_flows_by, find_last_update,
    create_moderator, delete_moderator, is_moderator
  ].

init_per_suite(Config) ->
  application:set_env(mnesia, dir, ?config(priv_dir, Config)),
  ok = mnesia:create_schema([node()]),
  ok = application:start(mnesia),

  Config.

end_per_suite(_Config) ->
  ok = flow_database:delete(),
  ok = application:stop(mnesia),
  mnesia:delete_schema([node()]),

  ok.

start(Config) ->
  ok = flow_database:create([node()]),
  flow_database:wait_for_tables(),

  Config.

stop(Config) ->
  ok = flow_database:delete(),

  Config.

init_per_testcase(find_float, Config) ->
  start(Config),

  {atomic, _} = flow_database:create_float("wat"),
  {atomic, _} = flow_database:create_float("omg", [announcement]),
  {atomic, _} = flow_database:create_float("lol", [announcement, {color, '#ff0000'}]),

  Config;

init_per_testcase(find_or_create_float, Config) ->
  start(Config),

  {atomic, _} = flow_database:create_float("wat"),
  {atomic, _} = flow_database:create_float("omg", [announcement]),

  Config;

init_per_testcase(find_drop, Config) ->
  start(Config),

  {atomic, _} = flow_database:create_drop("konnichiwa"),
  {atomic, _} = flow_database:create_drop(1, "o hai"),

  Config;

init_per_testcase(find_drops, Config) ->
  start(Config),

  {atomic, _} = flow_database:create_drop("konnichiwa"),
  {atomic, _} = flow_database:create_drop(1, "o hai"),

  Config;

init_per_testcase(create_flow, Config) ->
  start(Config),

  {atomic, _} = flow_database:create_drop("konnichiwa"),
  {atomic, _} = flow_database:create_drop(1, "o hai"),

  Config;

init_per_testcase(change_title, Config) ->
  start(Config),

  {atomic, _} = flow_database:create_flow("lol", "Don't lose your dinosaur", ["a", "b"]),

  Config;

init_per_testcase(add_floats, Config) ->
  start(Config),

  {atomic, _} = flow_database:create_flow("omg", "Don't lose your dinosaur", ["a", "b"]),

  Config;

init_per_testcase(delete_floats, Config) ->
  start(Config),

  {atomic, _} = flow_database:create_flow("omg", "Don't lose your dinosaur", ["a", "b", "c"]),

  Config;

init_per_testcase(find_flow, Config) ->
  start(Config),

  {atomic, _} = flow_database:create_drop("konnichiwa"),
  {atomic, _} = flow_database:create_drop(1, "o hai"),

  {atomic, _} = flow_database:create_flow("omg", 1, ["a", "c"]),
  {atomic, _} = flow_database:create_flow("lol", "Don't lose your dinosaur", ["a", "b"]),

  Config;

init_per_testcase(find_flows, Config) ->
  start(Config),

  {atomic, _} = flow_database:create_drop("konnichiwa"),
  {atomic, _} = flow_database:create_drop(1, "o hai"),

  {atomic, _} = flow_database:create_flow("omg", 1, ["a", "c"]),
  {atomic, _} = flow_database:create_flow("lol", "Don't lose your dinosaur", ["a", "b"]),

  Config;

init_per_testcase(find_drop_of, Config) ->
  start(Config),

  {atomic, _} = flow_database:create_drop("konnichiwa"),
  {atomic, _} = flow_database:create_drop(1, "o hai"),

  {atomic, _} = flow_database:create_flow("omg", 1, ["a", "c"]),
  {atomic, _} = flow_database:create_flow("lol", "Don't lose your dinosaur", ["a", "b"]),

  Config;

init_per_testcase(fetch_tree, Config) ->
  start(Config),

  {atomic, _} = flow_database:create_drop("konnichiwa"),
  {atomic, _} = flow_database:create_drop(1, "o hai"),
  {atomic, _} = flow_database:create_drop(1, "sup"),
  {atomic, _} = flow_database:create_drop(2, "no u"),

  {atomic, _} = flow_database:create_flow("omg", 1, ["a", "c"]),

  Config;

init_per_testcase(sort_flows_by, Config) ->
  start(Config),

  {atomic, _} = flow_database:create_drop("konnichiwa"),
  {atomic, _} = flow_database:create_drop(1, "o hai"),
  {atomic, _} = flow_database:create_drop(1, "sup"),
  {atomic, _} = flow_database:create_flow("omg", 1, ["a", "c"]),

  timer:sleep(timer:seconds(2)),

  {atomic, _} = flow_database:create_drop(2, "no u"),
  {atomic, _} = flow_database:create_flow("lol", "wut", ["a", "c"]),

  Config;

init_per_testcase(find_last_update, Config) ->
  start(Config),

  First = calendar:now_to_universal_time(now()),

  {atomic, _} = flow_database:create_drop("konnichiwa"),
  {atomic, _} = flow_database:create_drop(1, "o hai"),
  {atomic, _} = flow_database:create_drop(1, "sup"),

  timer:sleep(timer:seconds(2)),

  Last = calendar:now_to_universal_time(now()),

  {atomic, _} = flow_database:create_drop(2, "no u"),

  {atomic, _} = flow_database:create_flow("omg", 1, ["a", "c"]),

  [{times, {First, Last}} | Config];

init_per_testcase(delete_moderator, Config) ->
  start(Config),

  {atomic, _} = flow_database:create_moderator("lol@wut.it"),
  {atomic, _} = flow_database:create_moderator("omg@wut.it", [admin]),

  Config;

init_per_testcase(is_moderator, Config) ->
  start(Config),

  {atomic, _} = flow_database:create_moderator("lol@wut.it"),

  Config;

init_per_testcase(_, Config) ->
  start(Config).

end_per_testcase(_, Config) ->
  stop(Config).

create_float(_Config) ->
  {atomic, #flow_float{name = "wat", properties = []}}
    = flow_database:create_float("wat"),

  {atomic, #flow_float{name = "omg", properties = [announcement]}}
    = flow_database:create_float("omg", [announcement]),

  {atomic, #flow_float{name = "lol", properties = [announcement, {color, '#ff0000'}]}}
    = flow_database:create_float("lol", [announcement, {color, '#ff0000'}]),

  ok.

find_float(_Config) ->
  {atomic, #flow_float{name = "wat", properties = []}}
    = flow_database:find_float("wat"),

  {atomic, #flow_float{name = "omg", properties = [announcement]}}
    = flow_database:find_float("omg"),

  {atomic, #flow_float{name = "lol", properties = [announcement, {color, '#ff0000'}]}}
    = flow_database:find_float("lol"),

  {atomic, undefined}
    = flow_database:find_float("win"),

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
  {skip, "too lazy to implement this"}.

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

fetch_tree(_Config) ->
  {atomic, #flow_drop{id = 1, children = [#flow_drop{id = 2, children = [#flow_drop{id = 4}]}, #flow_drop{id = 3}]}}
    = flow_database:fetch_tree({flow, 1}),

  {atomic, #flow_drop{id = 1, children = [#flow_drop{id = 2, children = [#flow_drop{id = 4}]}, #flow_drop{id = 3}]}}
    = flow_database:fetch_tree({drop, 1}),

  ok.

sort_flows_by(_Config) ->
  [1, 2] = flow_database:sort_flows_by(update, [1, 2]),
  [2, 1] = flow_database:sort_flows_by(creation, [1, 2]),

  ok.

find_last_update(Config) ->
  {_, Last} = ?config(times, Config),

  Last = flow_database:find_last_update({flow, 1}),

  ok.

create_moderator(_Config) ->
  {atomic, #flow_moderator{email = "lol@wut.it", token = Token}}
    = flow_database:create_moderator("lol@wut.it"),

  {atomic, #flow_moderator{email = "lol@wut.it", token = Token}}
    = flow_database:create_moderator("lol@wut.it"),

  {atomic, #flow_moderator{email = "omg@wut.it", properties = [admin]}}
    = flow_database:create_moderator("omg@wut.it", [admin]),

  ok.

delete_moderator(_Config) ->
  true
    = flow_database:is_moderator({email, "omg@wut.it"}),

  {atomic, ok}
    = flow_database:delete_moderator("omg@wut.it"),

  false
    = flow_database:is_moderator({email, "omg@wut.it"}),

  ok.

is_moderator(_Config) ->
  true  = flow_database:is_moderator({email, "lol@wut.it"}),
  false = flow_database:is_moderator({email, "omg@wut.it"}),

  ok.
