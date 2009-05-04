divert(-1)

define(`BEGIN_SYNTAX', `dnl
define(`CURRENT_MACHINE', `')
define(`DONE_STATE', `')
divert(1)dnl
digraph {
divert(-1)
')

define(`END_SYNTAX', `dnl
divert(1)dnl
}
divert(-1)
')

define(`BEGIN_MACHINE', `
define(`CURRENT_MACHINE', `$1')
define(`DONE_STATE', `')
divert(-1)dnl
  subgraph "cluster-$1" {
divert(1)
    concentrate = true;
    pad = 0.25;
    fontsize = 20;
    fontname = Helvetica;
    labeljust = l;
    labelloc = t;
    label = "[$1] $3`'dnl
divert(-1)
')

define(`END_MACHINE', `
divert(1)dnl
    fontsize = 20;
    fontname = Helvetica;
divert(-1)
  }
divert(-1)
')
')

define(`PARAMETER', `
divert(1)dnl
 $1`'dnl
divert(-1)
')
')

define(`NO_PARAMETERS', `
divert(1)dnl
";
divert(-1)
')

define(`END_PARAMETERS', `
divert(1)dnl
";
divert(-1)
')

define(`BEGIN_STATE', `
define(`CURRENT_STATE', `$1')
divert(1)dnl
    "CURRENT_MACHINE-$1" [ shape = record, fontname = Helvetica, label = "$1dnl
divert(-1)
')

define(`NOP_COLOR', `#DFDFDF')
define(`FAILURE_COLOR', `#FF9999')
define(`TOKEN_COLOR', `#99FF99')
define(`CHOICE_COLOR', `#9999FF')
define(`STATE_COLOR', `#99EEEE')
define(`CHAR_COLOR', `#EEEE99')
define(`COUNTER_COLOR', `#EE99EE')

define(`NO_ACTION', `
define(`FILL_COLOR', `NOP_COLOR')
divert(1)dnl
 | Nop`'dnl
divert(-1)
')

define(`NEXT_CHAR', `
define(`FILL_COLOR', `CHAR_COLOR')
divert(1)dnl
 | NextChar`'dnl
divert(-1)
')

define(`PREV_CHAR', `
define(`FILL_COLOR', `CHAR_COLOR')
divert(1)dnl
 | PrevChar`'dnl
divert(-1)
')

define(`NEXT_LINE', `
define(`FILL_COLOR', `CHAR_COLOR')
divert(1)dnl
 | NextLine`'dnl
divert(-1)
')

define(`BEGIN_TOKEN', `
define(`FILL_COLOR', `TOKEN_COLOR')
divert(1)dnl
 | BeginToken $1`'dnl
divert(-1)
')

define(`END_TOKEN', `
define(`FILL_COLOR', `TOKEN_COLOR')
divert(1)dnl
 | EndToken $1`'dnl
divert(-1)
')

define(`EMPTY_TOKEN', `
define(`FILL_COLOR', `TOKEN_COLOR')
divert(1)dnl
 | EmptyToken $1`'dnl
divert(-1)
')

define(`BEGIN_CHOICE', `
define(`FILL_COLOR', `CHOICE_COLOR')
divert(1)dnl
 | BeginChoice $1`'dnl
divert(-1)
')

define(`END_CHOICE', `
define(`FILL_COLOR', `CHOICE_COLOR')
divert(1)dnl
 | EndChoice $1`'dnl
divert(-1)
')

define(`COMMIT', `
define(`FILL_COLOR', `CHOICE_COLOR')
divert(1)dnl
 | Commit $1`'dnl
divert(-1)
')

define(`PUSH_STATE', `
define(`FILL_COLOR', `STATE_COLOR')
divert(1)dnl
 | PushState`'dnl
divert(-1)
')

define(`SET_STATE', `
define(`FILL_COLOR', `STATE_COLOR')
divert(1)dnl
 | SetState`'dnl
divert(-1)
')

define(`RESET_STATE', `
define(`FILL_COLOR', `STATE_COLOR')
divert(1)dnl
 | ResetState`'dnl
divert(-1)
')

define(`POP_STATE', `
define(`FILL_COLOR', `STATE_COLOR')
divert(1)dnl
 | PopState`'dnl
divert(-1)
')

define(`RESET_COUNTER', `
define(`FILL_COLOR', `COUNTER_COLOR')
divert(1)dnl
 | ResetCounter`'dnl
divert(-1)
')

define(`INCREMENT_COUNTER', `
define(`FILL_COLOR', `COUNTER_COLOR')
divert(1)dnl
 | IncrementCounter`'dnl
divert(-1)
')

define(`UNEXPECTED', `
define(`FILL_COLOR', `FAILURE_COLOR')
divert(1)dnl
 | Unexpected`'dnl
divert(-1)
')

define(`BEGIN_TRANSITIONS', `
divert(1)dnl
", style = "filled", fillcolor = "FILL_COLOR" ];
divert(-1)
')

define(`BEGIN_TRANSITION', `
define(`LABEL_PREFIX', `dnl
    "CURRENT_MACHINE-CURRENT_STATE-$1" [ label = "dnl
')
divert(1)dnl
divert(-1)
')

define(`END_TRANSITION', `
divert(1)dnl
ifelse(LABEL_PREFIX, ` | ', `dnl
", fontname = Helvetica, shape = plaintext ];
ifelse(CURRENT_STATE, `$2', `
    "CURRENT_MACHINE-CURRENT_STATE" -> "CURRENT_MACHINE-CURRENT_STATE-$1" [ headport = ne ];
    "CURRENT_MACHINE-CURRENT_STATE-$1" -> "CURRENT_MACHINE-$2" [ tailport = nw ]
', `
    "CURRENT_MACHINE-CURRENT_STATE" -> "CURRENT_MACHINE-CURRENT_STATE-$1";
    "CURRENT_MACHINE-CURRENT_STATE-$1" -> "CURRENT_MACHINE-$2";
')
', `0', `$1', `dnl
    "CURRENT_MACHINE-CURRENT_STATE" -> "CURRENT_MACHINE-$2";
', `dnl
    "CURRENT_MACHINE-CURRENT_STATE" -> "CURRENT_MACHINE-$2" [ style = dashed ];
')
divert(-1)
')

define(`COUNTER_LESS_THAN_N', `
divert(1)dnl
LABEL_PREFIX`'CounterLessThanN`'dnl
divert(-1)
define(`LABEL_PREFIX', ` | ')
')

define(`COUNTER_LESS_EQUAL_N', `
divert(1)dnl
LABEL_PREFIX`'CounterLessEqualN`'dnl
divert(-1)
define(`LABEL_PREFIX', ` | ')
')

define(`START_OF_LINE', `
divert(1)dnl
LABEL_PREFIX`'StartOfLine`'dnl
divert(-1)
define(`LABEL_PREFIX', ` | ')
')

define(`BLOCK_OUT', `
divert(1)dnl
LABEL_PREFIX`'BlockOut`'dnl
divert(-1)
define(`LABEL_PREFIX', ` | ')
')

define(`BLOCK_IN', `
divert(1)dnl
LABEL_PREFIX`'BlockIn`'dnl
divert(-1)
define(`LABEL_PREFIX', ` | ')
')

define(`BLOCK_KEY', `
divert(1)dnl
LABEL_PREFIX`'BlockKey`'dnl
divert(-1)
define(`LABEL_PREFIX', ` | ')
')

define(`FLOW_OUT', `
divert(1)dnl
LABEL_PREFIX`'FlowOut`'dnl
divert(-1)
define(`LABEL_PREFIX', ` | ')
')

define(`FLOW_IN', `
divert(1)dnl
LABEL_PREFIX`'FlowIn`'dnl
divert(-1)
define(`LABEL_PREFIX', ` | ')
')

define(`FLOW_KEY', `
divert(1)dnl
LABEL_PREFIX`'FlowKey`'dnl
divert(-1)
define(`LABEL_PREFIX', ` | ')
')

define(`CLASS', `
divert(1)dnl
LABEL_PREFIX`'$1`'dnl
divert(-1)
define(`LABEL_PREFIX', ` | ')
')
