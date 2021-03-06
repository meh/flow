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
-author('meh. <meh@schizofreni.co>').

-include("flow.hrl").
-compile({no_auto_import, [get/1, get/0, put/2, error/1, error/2]}).
-export([start/1]).

start(Port) ->
  brokeback:start_link([{port, Port}, {loop, fun handle/1}]).

handle(Req) ->
  Resource = Req:resource([lowercase, urldecode]),
  Data     = case flow_json:decode(Req:get(body)) of
    {ok, Value, []} -> Value;
    {error, _}      -> null
  end,

  case Req:get(method) of
    'GET'    -> get(Resource, Req:parse_qs(), Req);
    'POST'   -> post(Resource, Req:parse_qs(), Data, Req);
    'PUT'    -> put(Resource, Req:parse_qs(), Data, Req);
    'DELETE' -> delete(Resource, Req:parse_qs(), Data, Req)
  end.

get(["drop", Id], Query, Req) ->
  {atomic, Drop} = case Req:get_variable("depth", Query) of
    undefined -> flow_database:find_drop(list_to_integer(Id));
    Depth     -> flow_database:fetch_tree({drop, list_to_integer(Id)}, list_to_integer(Depth))
  end,

  respond(flow_json:from_drop(Drop), Req);

get(["flow", Id], Query, Req) ->
  {atomic, Flow} = flow_database:find_flow(list_to_integer(Id)),

  case Req:get_variable("depth", Query) of
    undefined ->
      respond(flow_json:from_flow(Flow), Req);

    Depth ->
      {atomic, Drop} = flow_database:fetch_tree({drop, Flow#flow_flow.drop}, list_to_integer(Depth)),

      respond(flow_json:from_flow(Flow#flow_flow{drop = Drop}), Req)
  end;

get(["flows", Expression], Query, Req) ->
  {atomic, Flows} = flow_database:find_flows(Expression),
  Sorted          = case Req:get_variable("by", Query) of
    undefined  -> Flows;
    "creation" -> flow_database:sort_flows_by(creation, Flows);
    "update"   -> flow_database:sort_flows_by(update, Flows)
  end,

  respond(case Req:get_variable("full", Query) of
      undefined -> Sorted;
      []        -> lists:map(fun(Id) ->
              {atomic, Flow} = flow_database:find_flow(Id),
              flow_json:from_flow(Flow)
          end, Sorted)
    end, Req);

get(_, _, Req) ->
  not_found("What is love?", Req).

post(["flow"], _, {obj, Data}, Req) ->
  {atomic, Flow} = flow_database:create_flow(
      flow_json:to_string(Req:get_variable("title", Data)),
      flow_json:to_string(Req:get_variable("content", Data)),
      flow_json:to_string(Req:get_variable("floats", Data))),

  respond(flow_json:from_flow(Flow), Req);

post(_, _, _, Req) ->
  not_found("Baby don't hurt me.", Req).

put(_, _, _, Req) ->
  not_found("Don't hurt me.", Req).

delete(_, _, _, Req) ->
  not_found("No more.", Req).

respond(Data, Req) ->
  Req:ok(cross_domain([{"Content-Type", flow_json:mime_type()}]), flow_json:encode(Data)).

not_found(Reason, Req) ->
  Req:respond(404, cross_domain([{"Content-Type", "text/plain"}]), Reason).

cross_domain(Headers) ->
  case application:get_env(flow, domains) of
    {ok, Domains} -> [
        {"Access-Control-Expose-Headers", "*"},
        {"Access-Control-Allow-Credentials", "true"},
        {"Access-Control-Allow-Origin", string:join(Domains, ",")},
        {"Access-Control-Allow-Methods", string:join(Domains, ",")},
        {"Access-Control-Allow-Headers", string:join(Domains, ",")} | Headers];

    _ -> Headers
  end.
