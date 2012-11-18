-module(database_SUITE).

-include_lib("common_test/include/ct.hrl").
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2, all/0]).
-export([create_float/1]).

init_per_suite(Config) ->
  Priv = ?config(priv_dir, Config),
  application:set_env(mnesia, dir, Priv),
  mnesia:create_schema([node()]),
  mnesia:start(),
  flow_database:create(),
  Config.

end_per_suite(_Config) ->
  mnesia:stop(),
  ok.

all() ->
  [create_float].

init_per_testcase(create_float, Config) ->
  Config.

end_per_testcase(_, _Config) ->
  ok.

create_float(_Config) ->
  ok.
