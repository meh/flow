% Copyleft (ɔ) meh. - http://meh.schizofreni.co
%
% This file is part of flow - https://github.com/meh/flow
%
% flow is free software: you can redistribute it and/or modify
% it under the terms of the GNU Affero General Public License as published
% by the Free Software Foundation, either version 3 of the License,
% or (at your option) any later version.
%
% flow is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Affero General Public License for more details.
%
% You should have received a copy of the GNU Affero General Public License
% along with flow. If not, see <http://www.gnu.org/licenses/>.

-module(flow).
-author('meh. <meh@schizofreni.co>').

-export([install/0, install/1, uninstall/0, start/0, start/1, stop/0]).

install() ->
  install([node()]).

install(Nodes) ->
  ok = application:start(mnesia),
  flow_database:create(Nodes),
  flow_database:wait_for_tables(),

  stopped = application:stop(mnesia),

  ok.

uninstall() ->
  ok = application:start(mnesia),
  flow_database:delete(),

  stopped = application:stop(mnesia),

  ok.

start() ->
  start(58008).

start(Port) ->
  ok = application:start(mnesia),
  flow_database:wait_for_tables(),

  ok = application:start(misultin),
  flow_rest:start(Port),

  ok.

stop() ->
  stopped = application:stop(mnesia),
  stopped = application:stop(misultin),

  ok.
