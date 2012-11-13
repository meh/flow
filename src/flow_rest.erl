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

-module(flow_rest).
-export([start/0, start/1, stop/0]).

start() ->
  start(58008).

start(Port) ->
  misultin:start_link([{port, Port}, {loop, fun handle_http/1}]).

stop() ->
  misultin:stop().

handle_http(Req) ->
  handle(Req:get(method), Req:resource([lowercase, urldecode]), Req).

handle(_, _, Req) ->
  Req:respond(404, [{"Content-Type", "text/plain"}], "What is love?").
