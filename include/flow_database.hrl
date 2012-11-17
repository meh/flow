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

-author('meh. <meh@schizofreni.co>').

-record(flow_id, {
    type :: atom(),
    id   :: integer() }).

-record(flow_float, {
    name            :: string(),
    attributes = [] :: list(),
    flows      = [] :: [integer()] }).

-record(flow_drop, {
    id                   :: integer(),
    date                 :: calendar:datetime(),
    content              :: string(),
    flow     = undefined :: 'undefined' | integer(),
    parent   = undefined :: 'undefined' | integer(),
    children = []        :: [integer() | #flow_drop{}] }).

-record(flow_flow, {
    id          :: integer(),
    title       :: string(),
    floats = [] :: [string()],
    drop        :: integer() }).

-record(flow_moderator, {
    email :: string(),
    token :: string() }).

-spec flow_database:create(Node :: [node()]) ->
  'ok'.

-spec flow_database:wait_for_tables() ->
  'ok' | {'timeout', list()} | {'error', any()}.

-spec flow_database:wait_for_tables(Timeout :: timeout()) ->
  'ok' | {'timeout', list()} | {'error', any()}.

-spec flow_database:delete() ->
  'ok'.

-spec flow_database:create_float(Name :: string()) ->
  {'atomic', #flow_float{}} | {'aborted', any()}.

-spec flow_database:create_float(Name :: string(), Attributes :: list()) ->
  {'atomic', #flow_float{}} | {'aborted', any()}.

-spec flow_database:find_float(Name :: string()) ->
  {'atomic', #flow_float{}} | {'aborted', any()}.

-spec flow_database:find_or_create_float(Name :: string()) ->
  {'atomic', #flow_float{}} | {'aborted', any()}.

-spec flow_database:create_drop(Content :: string()) ->
  {'atomic', #flow_drop{}} | {'aborted', any()}.

-spec flow_database:create_drop(Parent :: integer(), Content :: string()) ->
  {'atomic', #flow_drop{}} | {'aborted', any()}.

-spec flow_database:find_drop(Id :: integer()) ->
  {'atomic', #flow_drop{}} | {'aborted', any()}.

-spec flow_databases:find_drops(Ids :: [integer()]) ->
  {'atomic', [#flow_drop{}]} | {'aborted', any()}.

-spec flow_database:create_flow(Title :: string(), IdOrContent :: integer() | string(), Floats :: [string()]) ->
  {'atomic', #flow_flow{}} | {'aborted', any()}.

-spec flow_database:change_title(Id :: integer(), Title :: string()) ->
  {'atomic', #flow_flow{}} | {'aborted', any()}.

-spec flow_database:add_floats(Id :: integer(), Floats :: [string()]) ->
  {'atomic', #flow_flow{}} | {'aborted', any()}.

-spec flow_database:delete_floats(Id :: integer(), Floats :: [string()]) ->
  {'atomic', #flow_flow{}} | {'aborted', any()}.

-spec flow_database:find_flow(Id :: integer()) ->
  {'atomic', #flow_flow{} | 'undefined'} | {'aborted', any()}.

-spec flow_database:find_flows(Expression :: string()) ->
  {'atomic', [integer()]} | {'aborted', any()}.

-spec flow_database:find_drop_of(Ids :: [integer()] | integer()) ->
  {'atomic', [{integer(), #flow_drop{}}] | #flow_drop{}} | {'aborted', any()}.

-spec flow_database:fetch_tree(Id :: {'drop' | 'flow', integer()}) ->
  {'atomic', #flow_drop{}} | {'aborted', any()}.

-spec flow_database:fetch_tree(Id :: {'drop' | 'flow', integer()}, Depth :: integer()) ->
  {'atomic', #flow_drop{} | integer()} | {'aborted', any()}.

-spec flow_database:sort_flows_by(By :: 'creation' | 'update', Flows :: [integer()]) ->
  [integer()].

-spec flow_database:find_last_update(Id :: {'drop' | 'flow', integer()}) ->
  calendar:datetime().

-spec flow_database:create_moderator(Email :: string()) ->
  {'atomic', #flow_moderator{}} | {'aborted', any()}.

-spec flow_database:delete_moderator(Email :: string()) ->
  {'atomic', 'ok'} | {'aborted', any()}.

-spec flow_database:is_moderator(What :: {'email', string()} | {'token', string()}) ->
  boolean().
