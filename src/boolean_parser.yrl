%          DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
%                  Version 2, December 2004
%
%          DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
% TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION 
%
% 0. You just DO WHAT THE FUCK YOU WANT TO.

Nonterminals
  expressions element.

Terminals
  '(' ')' 'not' 'and' 'or' 'xor' tag.

Rootsymbol
  expressions.

expressions -> element : '$1'.
expressions -> '(' expressions ')' : '$2'.
expressions -> 'not' expressions : {'not', '$2'}.
expressions -> expressions 'and' expressions : {'and', '$1', '$3'}.
expressions -> expressions 'or' expressions : {'or', '$1', '$3'}.
expressions -> expressions 'xor' expressions : {'xor', '$1', '$3'}.

element -> tag : normalize(element(3, '$1')).

Erlang code.

-export_type([expression/0]).

-type expression() :: string() |
  {'not', expression()} |
  {'and', expression(), expression()} |
  {'or', expression(), expression()} |
  {'xor', expression(), expression()}.

-export([normalize/1, expression/1, elements/1]).

% XXX: replace [\-_] with -?
normalize(String) ->
  Options = [global, {return, list}, unicode],

  string:to_lower(re:replace(re:replace(String, "(\\A\\s+)|(\\s+\\Z)", "", Options),
                             "\\s+", " ", Options)).

expression(Expression) ->
  parse(element(2, boolean_lexer:string(Expression))).

elements(Expression) ->
  {ok, Tokens, _} = boolean_lexer:string(Expression),

  lists:usort(lists:map(fun(Token) ->
          {tag, _, Element} = Token, normalize(Element)
        end, lists:filter(fun(Token) ->
            element(1, Token) == tag end, Tokens))).
