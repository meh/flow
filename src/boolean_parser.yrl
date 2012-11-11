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
  '(' ')' 'and' 'or' 'not' tag.

Rootsymbol
  expressions.

expressions -> element : '$1'.
expressions -> '(' expressions ')' : '$2'.
expressions -> 'not' expressions : {'not', '$2'}.
expressions -> expressions 'and' expressions : {'and', '$1', '$3'}.
expressions -> expressions 'or' expressions : {'or', '$1', '$3'}.

element -> tag : trans('$1').

Erlang code.

trans({ tag, _, Tag }) ->
  Tag.
