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

-module(flow_database).
-author('meh. <meh@schizofreni.co>').

-include("flow.hrl").

-record(flow_id, { type :: atom(), id :: integer() }).

-export([create/1, wait_for_tables/0, wait_for_tables/1, delete/0]).
-export([create_float/1, create_float/2, find_float/1, find_or_create_float/1, merge_floats/2]).
-export([create_drop/1, create_drop/2, find_drop/1, find_drops/1]).
-export([create_flow/3, change_title/2, add_floats/2, delete_floats/2, find_flow/1, find_flows/1, find_drop_of/1, fetch_tree/1, fetch_tree/2, sort_flows_by/2, find_last_update/1]).
-export([create_moderator/1, create_moderator/2, delete_moderator/1, is_moderator/1]).

create(Nodes) ->
  {_, ok} = mnesia:create_table(flow_id,
                      [{attributes, record_info(fields, flow_id)},
                       {disc_copies, Nodes}]),

  {_, ok} = mnesia:create_table(flow_float,
                      [{attributes, record_info(fields, flow_float)},
                       {disc_copies, Nodes}]),

  {_, ok} = mnesia:create_table(flow_drop,
                      [{attributes, record_info(fields, flow_drop)},
                       {disc_copies, Nodes}]),

  {_, ok} = mnesia:create_table(flow_flow,
                      [{attributes, record_info(fields, flow_flow)},
                       {disc_copies, Nodes}]),

  {_, ok} = mnesia:create_table(flow_moderator,
                      [{attributes, record_info(fields, flow_moderator)},
                       {index, [#flow_moderator.token]},
                       {disc_copies, Nodes}]),

  ok.

wait_for_tables() ->
  wait_for_tables(infinity).

wait_for_tables(Timeout) ->
  mnesia:wait_for_tables([flow_id, flow_float, flow_drop, flow_flow, flow_moderator], Timeout).

delete() ->
  mnesia:delete_table(flow_id),
  mnesia:delete_table(flow_float),
  mnesia:delete_table(flow_drop),
  mnesia:delete_table(flow_flow),
  mnesia:delete_table(flow_moderator),

  ok.

auto_increment(Name) ->
  mnesia:dirty_update_counter(flow_id, Name, 1).

create_float(Name) ->
  create_float(Name, []).

create_float(Name, Properties) ->
  mnesia:transaction(fun() ->
        Float = #flow_float{
            name       = boolean_parser:normalize(Name),
            properties = Properties },

        mnesia:write(Float),

        Float
    end).

find_float(Name) ->
  mnesia:transaction(fun() ->
        case mnesia:wread({flow_float, boolean_parser:normalize(Name)}) of
          [Float] -> Float;
          _       -> undefined
        end
    end).

find_or_create_float(Name) ->
  mnesia:transaction(fun() ->
        case find_float(Name) of
          {atomic, undefined} -> {atomic, Float} = create_float(Name), Float;
          {atomic, Float}     -> Float
        end
    end).

merge_floats(Float, Into) when Float == Into ->
  ok;

merge_floats(Float, Into) ->
  case mnesia:transaction(fun() ->
          [First]  = mnesia:wread({flow_float, Float}),
          [Second] = mnesia:wread({flow_float, Into}),

          lists:foreach(fun(Id) ->
                [Flow] = mnesia:wread({flow_flow, Id}),

                mnesia:write(Flow#flow_flow{
                    floats = lists:usort([Into | lists:delete(Float, Flow#flow_flow.floats)]) })
            end, lists:usort(First#flow_float.flows ++ Second#flow_float.flows)),

          mnesia:write(Second#flow_float{flows = lists:usort(Second#flow_float.flows ++ First#flow_float.flows)}),
          mnesia:delete({flow_float, Float})
      end) of
    Error = {aborted, _} -> Error;
    _                    -> ok
  end.

create_drop(Content) ->
  mnesia:transaction(fun() ->
        Drop = #flow_drop{
            id      = auto_increment(flow_drop),
            date    = calendar:now_to_universal_time(now()),
            content = Content },

        mnesia:write(Drop),

        Drop
    end).

create_drop(Parent, Content) ->
  mnesia:transaction(fun() ->
        Drop = #flow_drop{
            id      = auto_increment(flow_drop),
            parent  = Parent,
            date    = calendar:now_to_universal_time(now()),
            content = Content },

        [ParentDrop] = mnesia:wread({flow_drop, Parent}),

        mnesia:write(Drop),
        mnesia:write(ParentDrop#flow_drop{
            children = ParentDrop#flow_drop.children ++ [Drop#flow_drop.id] }),

        Drop
    end).

find_drop(Id) ->
  mnesia:transaction(fun() ->
        case mnesia:wread({flow_drop, Id}) of
          [Drop] -> Drop;
          _      -> undefined
        end
    end).

find_drops(Ids) ->
  MatchSpec = match_all(#flow_drop{id = '$1', _ = '_'}, Ids, '$_'),

  mnesia:transaction(fun() ->
          mnesia:select(flow_drop, MatchSpec)
      end).

create_flow(_, _, []) ->
  {aborted, no_floats};

create_flow(Title, IdOrContent, Floats) ->
  Id = case is_list(IdOrContent) of
    true  -> {atomic, Created} = create_drop(IdOrContent), Created#flow_drop.id;
    false -> IdOrContent
  end,

  mnesia:transaction(fun() ->
        case mnesia:wread({flow_drop, Id}) of
          [Drop = #flow_drop{flow = undefined, _ = _}] ->
            NewFlow = #flow_flow{
                id    = auto_increment(flow_flow),
                title = Title,
                drop  = Id },

            mnesia:write(NewFlow),
            mnesia:write(Drop#flow_drop{flow = NewFlow#flow_flow.id}),

            {atomic, Flow} = add_floats(NewFlow#flow_flow.id, Floats), Flow;

          [_] -> mnesia:abort(drop_already_in_flow);
          _   -> mnesia:abort(drop_not_found)
        end
    end).

change_title(Id, Title) ->
  mnesia:transaction(fun() ->
        [Flow]  = mnesia:wread({flow_flow, Id}),
        NewFlow = Flow#flow_flow{title = Title},

        mnesia:write(NewFlow),

        NewFlow
    end).

add_floats(Id, Floats) ->
  mnesia:transaction(fun() ->
        [Flow]        = mnesia:wread({flow_flow, Id}),
        MissingFloats = lists:filter(fun(Elem) ->
                not lists:member(Elem, Flow#flow_flow.floats)
            end, lists:usort(Floats)),

        case length(MissingFloats) of
          0 -> Flow;
          _ ->
            NewFlow = Flow#flow_flow{ floats = Flow#flow_flow.floats ++ lists:map(fun(Name) ->
                      {atomic, Float} = find_or_create_float(Name),

                      mnesia:write(Float#flow_float{flows = [Id | Float#flow_float.flows]}),

                      Float#flow_float.name
                  end, MissingFloats)},

            mnesia:write(NewFlow),

            NewFlow
        end
    end).

delete_floats(Id, Floats) ->
  mnesia:transaction(fun() ->
        {atomic, Flow} = find_flow(Id),
        Length         = length(Flow#flow_flow.floats),
        PresentFloats  = lists:filter(fun(Elem) ->
                lists:member(Elem, Flow#flow_flow.floats)
            end, lists:usort(Floats)),

        case length(PresentFloats) of
          Length -> mnesia:abort(no_floats);
          0      -> Flow;
          _      ->
            lists:foreach(fun(Name) ->
                  {atomic, Float} = find_float(Name),

                  mnesia:write(Float#flow_float{flows = lists:delete(Id, Float#flow_float.flows)})
              end, PresentFloats),

            NewFlow = Flow#flow_flow{ floats = lists:filter(fun(Float) ->
                      not lists:member(Float, PresentFloats)
                  end, Flow#flow_flow.floats) },

            mnesia:write(NewFlow),

            NewFlow
        end
    end).

find_flow(Id) ->
  mnesia:transaction(fun() ->
        case mnesia:wread({flow_flow, Id}) of
          [Flow] -> Flow;
          _      -> undefined
        end
    end).

find_flows(Expression) ->
  MatchSpec = match_all(#flow_float{name = '$1', flows = '$2', _ = '_'},
                        boolean_parser:elements(Expression), {{'$1', '$2'}}),

  case mnesia:transaction(fun() -> mnesia:select(flow_float, MatchSpec) end) of
    {atomic, Floats} -> {atomic, filter_flows(Expression, orddict:from_list(Floats))};
    Error            -> Error
  end.

filter_flows(Expression, Floats) ->
  {ok, ParsedExpression} = boolean_parser:expression(Expression),
  Flows                  = lists:usort(orddict:fold(fun(_, Value, Acc) -> Value ++ Acc end, [], Floats)),

  filter_flows(ParsedExpression, Floats, Flows).

% the Expression was just a single float, return early
filter_flows(ParsedExpression, _, Flows) when is_list(ParsedExpression) ->
  Flows;

filter_flows(ParsedExpression, Floats, Flows) ->
  lists:filter(fun(Flow) -> in_expression(Flow, ParsedExpression, Floats) end, Flows).

in_expression(Flow, {'not', What}, Floats) ->
  not in_expression(Flow, What, Floats);

in_expression(Flow, {'and', Left, Right}, Floats) ->
  in_expression(Flow, Left, Floats) andalso in_expression(Flow, Right, Floats);

in_expression(Flow, {'or', Left, Right}, Floats) ->
  in_expression(Flow, Left, Floats) orelse in_expression(Flow, Right, Floats);

in_expression(Flow, {'xor', Left, Right}, Floats) ->
  in_expression(Flow, Left, Floats) xor in_expression(Flow, Right, Floats);

% Term is a string, check if the Flow has that float
in_expression(Flow, Term, Floats) when is_list(Term) ->
  lists:member(Flow, orddict:fetch(Term, Floats)).

find_drop_of(Id) when is_integer(Id) ->
  mnesia:transaction(fun() ->
        [Flow] = mnesia:read({flow_flow, Id}),
        [Drop] = mnesia:read({flow_drop, Flow#flow_flow.drop}),

        Drop
    end);

find_drop_of(Ids) when is_list(Ids) ->
  mnesia:transaction(fun() ->
        mnesia:select(flow_drop, match_all(#flow_drop{flow = '$1', _ = '_'}, Ids, {{'$1', '$_'}}))
    end).

fetch_tree(Id) ->
  fetch_tree(Id, -1).

fetch_tree({flow, Id}, 0) ->
  {atomic, Drop} = find_drop_of(Id), {atomic, Drop#flow_drop.id};

fetch_tree({drop, Id}, 0) ->
  {atomic, Id};

fetch_tree({flow, Id}, Depth) ->
  mnesia:transaction(fun() ->
        [Flow] = mnesia:read({flow_flow, Id}),

        {atomic, Result} = fetch_tree({drop, Flow#flow_flow.drop}, Depth), Result
    end);

fetch_tree({drop, Id}, Depth) ->
  mnesia:transaction(fun() ->
        [Drop] = mnesia:read({flow_drop, Id}),

        Drop#flow_drop{children = lists:map(fun(CurrentId) ->
                {atomic, CurrentDrop} = fetch_tree({drop, CurrentId}, Depth - 1), CurrentDrop
            end, Drop#flow_drop.children)}
    end).

sort_flows_by(creation, Flows) ->
  {atomic, Drops} = find_drop_of(Flows),

  lists:map(fun({Flow, _}) -> Flow end, lists:sort(fun({_, #flow_drop{date = A}}, {_, #flow_drop{date = B}}) ->
          B =< A end, Drops));

sort_flows_by(update, Flows) ->
  {atomic, Drops} = find_drop_of(Flows),

  lists:map(fun({Flow, _}) -> Flow end, lists:sort(fun({A, _}, {B, _}) ->
          find_last_update({flow, A}) =< find_last_update({flow, B}) end, Drops)).

find_last_update(Id) ->
  {atomic, Drop} = fetch_tree(Id),

  find_last_update(Drop#flow_drop{flow = undefined}, 0).

find_last_update([], Max) ->
  Max;

find_last_update([Drop | Rest], Max) ->
  First  = find_last_update(Drop, Max),
  Second = find_last_update(Rest, Max),

  case First =< Second of
    true  -> Second;
    false -> First
  end;

find_last_update(Drop = #flow_drop{children = []}, Max) when Max =< Drop#flow_drop.date ->
  Drop#flow_drop.date;

find_last_update(#flow_drop{children = []}, Max) ->
  Max;

find_last_update(Drop = #flow_drop{flow = undefined}, Max) when Max =< Drop#flow_drop.date ->
  find_last_update(Drop#flow_drop.children, Drop#flow_drop.date);

find_last_update(Drop = #flow_drop{}, Max) when Max =< Drop#flow_drop.date ->
  Drop#flow_drop.date;

find_last_update(#flow_drop{}, Max) ->
  Max.

create_moderator(Email) ->
  create_moderator(Email, []).

create_moderator(Email, Properties) ->
  mnesia:transaction(fun() ->
        case mnesia:read({flow_moderator, Email}) of
          [Moderator] -> Moderator;
          _           ->
            Moderator = #flow_moderator{
                email      = Email,
                properties = Properties,
                token      = generate_token() },

            mnesia:write(Moderator),

            Moderator
        end
    end).

delete_moderator(Email) ->
  mnesia:transaction(fun() ->
        mnesia:delete({flow_moderator, Email})
    end).

is_moderator(What) ->
  case mnesia:transaction(fun() ->
          mnesia:match_object(case What of
              {token, Token} -> #flow_moderator{token = Token, _ = '_'};
              {email, Email} -> #flow_moderator{email = Email, _ = '_'}
            end)
      end) of
    {atomic, [_]} -> true;
    {atomic, []}  -> false
  end.

generate_token() ->
  generate_token(32).

generate_token(Length) ->
  generate_token(Length, "abcdefghijklmnopqrstuvwxyz0123456789").

generate_token(Length, Charset) ->
  Token = [lists:nth(random:uniform(length(Charset)), Charset) || _ <- lists:seq(1, Length)],

  case is_moderator({token, Token}) of
    false -> Token;
    true  -> generate_token(Length, Charset)
  end.

match_all(Head, [Key], Result) ->
  [{Head, [{'==', '$1', Key}], [Result]}];

match_all(Head, Keys, Result) ->
  [{Head, [match_all(Keys)], [Result]}].

match_all([First, Second | []]) ->
  {'orelse', {'==', '$1', First}, {'==', '$1', Second}};

match_all([First | Rest]) ->
  {'orelse', {'==', '$1', First}, match_all(Rest)}.
