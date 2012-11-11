-module(flow_database).
-export([
  install/1, create_float/2, create_drop/1, create_drop/2, create_flow/3, add_floats/2,
  delete_floats/2, find_flows/1]).

-record(flow_ids, {type, id}).

-record(flow_float, {name,
                     attributes=[],
                     flows=[]
                    }).

-record(flow_drop, {id,
                    content,
                    parent=undefined,
                    children=[]
                   }).

-record(flow_flow, {id,
                    title,
                    floats=[],
                    drop
                   }).

install(Nodes) ->
  ok = mnesia:create_schema(Nodes),
  application:start(mnesia),

  mnesia:create_table(flow_ids,
                      [{attributes, record_info(fields, flow_ids)},
                       {index, [#flow_ids.type]},
                       {disc_copies, Nodes}]),

  mnesia:create_table(flow_float,
                      [{attributes, record_info(fields, flow_float)},
                       {index, [#flow_float.name]},
                       {disc_copies, Nodes}]),

  mnesia:create_table(flow_drop,
                      [{attributes, record_info(fields, flow_drop)},
                       {index, [#flow_drop.id]},
                       {disc_copies, Nodes}]),

  mnesia:create_table(flow_flow,
                      [{attributes, record_info(fields, flow_flow)},
                       {index, [#flow_flow.id]},
                       {disc_copies, Nodes}]),

  application:stop(mnesia).

auto_increment(Name) ->
  mnesia:dirty_update_counter(flow_ids, Name, 1).

create_float(Name, Attributes) ->
  mnesia:transaction(fun() ->
        Float = #flow_float{ name = Name, attributes = Attributes },
        mnesia:write(Float),
        Float
    end).

create_drop(Content) ->
  create_drop(undefined, Content).

create_drop(Parent, Content) ->
  mnesia:transaction(fun() ->
        Drop = #flow_drop{ id = auto_increment(flow_drop), parent = Parent, content = Content },
        mnesia:write(Drop),
        Drop
    end).

create_flow(Title, Drop, Floats) ->
  Id = if is_list(Drop) -> (create_drop(Drop))#flow_drop.id;
          true          -> Drop
  end,

  mnesia:transaction(fun() ->
        Flow = #flow_flow{ id = auto_increment(flow_flow), title = Title, floats = Floats, drop = Id },
        mnesia:write(Flow),
        add_floats(Flow#flow_flow.id, Floats),
        Flow
    end).

add_floats(Id, Floats) ->
  mnesia:transaction(fun() ->
        [Flow] = mnesia:wread({flow_flow, Id}),
        MissingFloats = lists:filter(fun(Elem) ->
                not lists:member(Elem, Flow#flow_flow.floats)
            end, lists:usort(Floats)),

        if length(MissingFloats) > 0 ->
            lists:foreach(MissingFloats, fun(Name) ->
                  [Float] = mnesia:wread({flow_float, Name}),
                  mnesia:write(Float#flow_float{ flows = [Id | Float#flow_float.flows] })
              end),

            NewFlow = Flow#flow_flow{ floats = Flow#flow_flow.floats ++ MissingFloats },
            mnesia:write(NewFlow),
            NewFlow;

          true -> Flow
        end
    end).

delete_floats(Id, Floats) ->
  mnesia:transaction(fun() ->
        [Flow] = mnesia:wread({flow_flow, Id}),
        PresentFloats = lists:filter(fun(Elem) ->
                lists:member(Elem, Flow#flow_flow.floats)
            end, lists:usort(Floats)),

        if length(PresentFloats) > 0 ->
            lists:foreach(PresentFloats, fun(Name) ->
                  [Float] = mnesia:wread({flow_float, Name}),
                  mnesia:write(Float#flow_float{ flows = lists:delete(Float#flow_float.flows, Id) })
              end),

            NewFlow = Flow#flow_flow{ floats = lists:filter(fun() ->
                      not lists:member(PresentFloats)
                  end, Flow#flow_flow.floats) },
            mnesia:write(NewFlow),
            NewFlow;

          true -> Flow
        end
    end).

find_flows(Expression) ->
  {ok, Tokens, _} = booerlang_lexer:string(Expression),
  Tokens, [].
