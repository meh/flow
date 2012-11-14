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

-include("flow_database.hrl").

-export([install/1, wait_for_tables/0, wait_for_tables/1, uninstall/0]).
-export([create_float/1, create_float/2, find_float/1, find_or_create_float/1]).
-export([create_drop/1, create_drop/2]).
-export([create_flow/3, add_floats/2, delete_floats/2, find_flow/1, find_flows/1]).
-export([create_moderator/1, delete_moderator/1, is_moderator/1]).

install(Nodes) ->
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

uninstall() ->
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

create_float(Name, Attributes) ->
  mnesia:transaction(fun() ->
        Float = #flow_float{
            name       = boolean_parser:normalize(Name),
            attributes = Attributes },

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

create_drop(Content) ->
  create_drop(undefined, Content).

create_drop(Parent, Content) ->
  mnesia:transaction(fun() ->
        Drop = #flow_drop{
            id      = auto_increment(flow_drop),
            date    = calendar:now_to_universal_time(now()),
            parent  = Parent,
            content = Content },

        mnesia:write(Drop),

        Drop
    end).

create_flow(_, _, []) ->
  {aborted, no_floats};

create_flow(Title, Drop, Floats) ->
  Id = case is_list(Drop) of
    true  -> {atomic, Created} = create_drop(Drop), Created#flow_drop.id;
    false -> Drop
  end,

  mnesia:transaction(fun() ->
        NewFlow = #flow_flow{
            id    = auto_increment(flow_flow),
            title = Title,
            drop  = Id },

        mnesia:write(NewFlow),

        {atomic, Flow} = add_floats(NewFlow#flow_flow.id, Floats),
        Flow
    end).

add_floats(Id, Floats) ->
  mnesia:transaction(fun() ->
        [Flow] = mnesia:wread({flow_flow, Id}),
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
        Length = length(Flow#flow_flow.floats),
        PresentFloats = lists:filter(fun(Elem) ->
                lists:member(Elem, Flow#flow_flow.floats)
            end, lists:usort(Floats)),

        case length(PresentFloats) of
          Length -> mnesia:abort(no_floats);
          0 -> Flow;
          _ ->
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

  {atomic, Floats} = mnesia:transaction(fun() ->
        mnesia:select(flow_float, MatchSpec)
    end),

  filter_flows(Expression, dict:from_list(Floats)).


filter_flows(Expression, Floats) ->
  {ok, ParsedExpression} = boolean_parser:expression(Expression),
  Flows                  = lists:usort(dict:fold(fun(_, Value, Acc) -> Value ++ Acc end, [], Floats)),

  filter_flows(ParsedExpression, Floats, Flows).

filter_flows(ParsedExpression, _, Flows) when is_list(ParsedExpression) ->
  Flows;

filter_flows(ParsedExpression, Floats, Flows) ->
  lists:filter(fun(Flow) -> in_expression(Flow, ParsedExpression, Floats) end, Flows).

in_expression(Flow, {'not', What}, Floats) ->
  not in_expression(Flow, What, Floats);

in_expression(Flow, {'and', Left, Right}, Floats) ->
  in_expression(Flow, Left, Floats) and in_expression(Flow, Right, Floats);

in_expression(Flow, {'or', Left, Right}, Floats) ->
  in_expression(Flow, Left, Floats) or in_expression(Flow, Right, Floats);

in_expression(Flow, Term, Floats) when is_list(Term) ->
  lists:member(Flow, dict:fetch(Term, Floats)).

create_moderator(Email) ->
  mnesia:transaction(fun() ->
        case mnesia:read({flow_moderator, Email}) of
          [Moderator] -> Moderator;
          _ ->
            Moderator = #flow_moderator{
                email = Email,
                token = generate_token() },

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
