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

-module(flow_json).
-author('meh. <meh@schizofreni.co>').

-include("flow.hrl").

-export([mime_type/0, encode/1, decode/1]).
-export([from_string/1, from_datetime/1, from_drop/1, from_flow/1]).

mime_type() ->
  rfc4627:mime_type().

encode(Data) ->
  rfc4627:encode(Data).

decode(Data) ->
  rfc4627:decode(Data).

from_string(String) ->
  list_to_binary(String).

from_datetime({{Year, Month, Day}, {Hour, Minute, Second}}) ->
  from_string(io_lib:format("~w-~w-~wT~w:~w:~w+00:00", [Year, Month, Day, Hour, Minute, Second])).

from_drop(Id) when is_integer(Id) ->
  Id;

from_drop(Drop = #flow_drop{}) ->
  {obj, orddict:filter(fun(_, Value) -> Value /= undefined end, [
        {id, Drop#flow_drop.id},
        {flow, Drop#flow_drop.flow},
        {parent, Drop#flow_drop.parent},
        {date, from_datetime(Drop#flow_drop.date)},
        {content, from_string(Drop#flow_drop.content)},
        {children, lists:map(fun from_drop/1, Drop#flow_drop.children)}])}.

from_flow(undefined) ->
  null;

from_flow(Flow) ->
  {obj, [
      {id, Flow#flow_flow.id},
      {title, from_string(Flow#flow_flow.title)},
      {floats, lists:map(fun from_string/1, Flow#flow_flow.floats)},
      {drop, from_drop(Flow#flow_flow.drop)}]}.
