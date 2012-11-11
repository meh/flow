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
