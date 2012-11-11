Definitions.

WS = [\000-\s]
C  = [a-zA-Z0-9]
CS = [a-zA-Z0-9\s]

Rules.

!            : {token, {'not', TokenLine}}.
[Nn][Oo][Tt] : {token, {'not', TokenLine}}.

&&?          : {token, {'and', TokenLine}}.
[Aa][Nn][Dd] : {token, {'and', TokenLine}}.

\|\|?    : {token, {'or', TokenLine}}.
[Oo][Rr] : {token, {'or', TokenLine}}.

[()] : {token, {list_to_atom(TokenChars), TokenLine}}.

{C}+    : {token, {tag, TokenLine, TokenChars}}.
"{CS}+" : {token, {tag, TokenLine, string:sub_string(TokenChars, 2, TokenLen - 1)}}.
'{CS}+' : {token, {tag, TokenLine, string:sub_string(TokenChars, 2, TokenLen - 1)}}.

{WS}+   : skip_token.

Erlang code.

-export([elements/1]).

elements(Expression) ->
  {ok, Tokens, _} = string(Expression),

  lists:usort(lists:map(fun(Token) ->
          {tag, _, Element} = Token, Element
        end, lists:filter(fun(Token) ->
            element(1, Token) == tag end, Tokens))).
