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
divert(1)dnl
  subgraph "cluster-$1" {
divert(-1)
')

define(`END_MACHINE', `
divert(1)dnl
    label = "[$1] $3`'dnl
undivert(2)`'dnl
";
  }
divert(-1)
')
')

define(`PARAMETER', `
divert(2)dnl
 $1`'dnl
divert(-1)
')
')

define(`BEGIN_STATE', `
define(`CURRENT_STATE', `$1')
divert(1)dnl
    "CURRENT_MACHINE-$1" [ shape = record, label = "$1dnl
divert(-1)
')

define(`NO_ACTION', `
divert(1)dnl
 | Nop`'dnl
divert(-1)
')

define(`NEXT_CHAR', `
divert(1)dnl
 | NextChar`'dnl
divert(-1)
')

define(`PREV_CHAR', `
divert(1)dnl
 | PrevChar`'dnl
divert(-1)
')

define(`NEXT_LINE', `
divert(1)dnl
 | NextLine`'dnl
divert(-1)
')

define(`BEGIN_TOKEN', `
divert(1)dnl
 | BeginToken $1`'dnl
divert(-1)
')

define(`END_TOKEN', `
divert(1)dnl
 | EndToken $1`'dnl
divert(-1)
')

define(`EMPTY_TOKEN', `
divert(1)dnl
 | EmptyToken $1`'dnl
divert(-1)
')

define(`BEGIN_CHOICE', `
divert(1)dnl
 | BeginChoice $1`'dnl
divert(-1)
')

define(`END_CHOICE', `
divert(1)dnl
 | EndChoice $1`'dnl
divert(-1)
')

define(`COMMIT', `
divert(1)dnl
 | Commit $1`'dnl
divert(-1)
')

define(`PUSH_STATE', `
divert(1)dnl
 | PushState`'dnl
divert(-1)
')

define(`RESET_STATE', `
divert(1)dnl
 | ResetState`'dnl
divert(-1)
')

define(`POP_STATE', `
divert(1)dnl
 | PopState`'dnl
divert(-1)
')

define(`RESET_COUNTER', `
divert(1)dnl
 | ResetCounter`'dnl
divert(-1)
')

define(`INCREMENT_COUNTER', `
divert(1)dnl
 | IncrementCounter`'dnl
divert(-1)
')

define(`NON_POSITIVE_N', `
divert(1)dnl
 | NonPositiveN`'dnl
divert(-1)
')

define(`BAD_CONTEXT', `
divert(1)dnl
 | BadContext`'dnl
divert(-1)
')


define(`SUCCESS', `
divert(1)dnl
 | Success`'dnl
divert(-1)
')

define(`FAILURE', `
divert(1)dnl
 | Failure`'dnl
divert(-1)
')

define(`BEGIN_TRANSITIONS', `
divert(1)dnl
" ];
divert(-1)
')

define(`BEGIN_TRANSITION', `
define(`LABEL_PREFIX', `label = "[$1] ')
divert(1)dnl
    "CURRENT_MACHINE-CURRENT_STATE" -> "CURRENT_MACHINE-CURRENT_STATE-$2";
    "CURRENT_MACHINE-CURRENT_STATE-$2" -> "CURRENT_MACHINE-$2";
    "CURRENT_MACHINE-CURRENT_STATE-$2" [ dnl
divert(-1)
')

define(`END_TRANSITION', `
divert(1)dnl
ifelse(LABEL_PREFIX, ` | ', `dnl
", shape = plaintext ];
', `dnl
shape = point ];
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
