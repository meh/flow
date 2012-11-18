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

-include("flow.hrl").
-compile({no_auto_import, [get/1, get/0, put/2]}).
-export([start/0, start/1, stop/0]).

start() ->
  start(58008).

start(Port) ->
  mnesia:start(),
  misultin:start_link([{port, Port}, {loop, fun handle_http/1}]),
  flow_database:wait_for_tables().

stop() ->
  misultin:stop(),
  mnesia:stop().

handle_http(Req) ->
  Resource = Req:resource([lowercase, urldecode]),

  case Req:get(method) of
    'GET'    -> get(Resource, Req:parse_qs(), Req);
    'POST'   -> post(Resource, Req:parse_qs(), rfc4627:decode(Req:get(body)), Req);
    'PUT'    -> put(Resource, Req:parse_qs(), rfc4627:decode(Req:get(body)), Req);
    'DELETE' -> delete(Resource, Req:parse_qs(), rfc4627:decode(Req:get(body)), Req)
  end.

get(["flow", Id], _, Req) ->
  case flow_database:find_flow(list_to_integer(Id)) of
    {atomic, undefined} -> respond(null, Req);
    {atomic, Flow}      -> respond({obj, [
            {title, list_to_binary(Flow#flow_flow.title)},
            {floats, lists:map(fun erlang:list_to_binary/1, Flow#flow_flow.floats)},
            {drop, Flow#flow_flow.drop}]}, Req)
  end;

get(["flows", Expression], Query, Req) ->
  Flows = flow_database:find_flows(Expression),

  respond(Flows, Req);

get(_, _, Req) ->
  Req:respond(404, [{"Content-Type", "text/plain"}], "What is love?").

post(_, _, _, Req) ->
  Req:respond(404, [{"Content-Type", "text/plain"}], "Baby don't hurt me.").

put(_, _, _, Req) ->
  Req:respond(404, [{"Content-Type", "text/plain"}], "Don't hurt me.").

delete(_, _, _, Req) ->
  Req:respond(404, [{"Content-Type", "text/plain"}], "No more.").

respond(Data, Req) ->
  Req:ok([{"Content-Type", rfc4627:mime_type()}], rfc4627:encode(Data)).
