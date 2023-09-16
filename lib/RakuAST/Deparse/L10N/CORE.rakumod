# This file contains that "null" translation of features of the Raku
# Programming Language, and as such only has meaning as a template for
# other translations.  It is **not** installed by default, as it serves
# no purpose other then by being a source code example.
#
# Please note that as the Raku Programming Language evolves, further
# elements may be added, so any translations will probably need to be
# updated by then as well.
#
# The keys consist of 2 parts: before the first hyphen is a type indication
# of the syntax, followed by the name of the feature in the syntax.  The
# following type indications exist so far:
#
#   block        syntax involving a block
#   core         sub/method names that are part of the Raku core
#   constraint   related to (ad-hoc) constraints
#   infix        infix operators consisting of alphanumeric characters
#   modifier     statement modifier syntax
#   multi        types of multi syntax
#   package      package declarators
#   prefix       prefix operators consisting of alphanumeric characters
#   phaser       types of phasers ("BEGIN","END","CHECK", etc.)
#   routine      types of named code blocks
#   scope        types of scope ("my","our","state", etc.)
#   stmt-prefix  statement prefixes ("do","eager","lazy", etc.)
#   trait        types of traits ("is","does","returns", etc.)
#   typer        type constructors ("enum","subset")
#   use          use related ("use","no","require", etc.)
#
# If you call the .DEPARSE method with a role compatible with this role,
# it will automatically mix it in with the default RakuAST::Deparse class.

use v6.e.PREVIEW;

unit role RakuAST::Deparse::L10N::CORE;

my constant %translation =
  block-default => 'default',
  block-else    => 'else',
  block-elsif   => 'elsif',
  block-for     => 'for',
  block-given   => 'given',
  block-if      => 'if',
  block-loop    => 'loop',
  block-orwith  => 'orwith',
  block-repeat  => 'repeat',
  block-unless  => 'unless',
  block-until   => 'until',
  block-when    => 'when',
  block-while   => 'while',
  block-with    => 'with',
  block-without => 'without',

  constraint-where => 'where',

  core-abs              => 'abs',
  core-all              => 'all',
  core-any              => 'any',
  core-append           => 'append',
  core-ast              => 'ast',
  core-atomic-add-fetch => 'atomic-add-fetch',
  core-atomic-assign    => 'atomic-assign',
  core-atomic-dec-fetch => 'atomic-dec-fetch',
  core-atomic-fetch     => 'atomic-fetch',
  core-atomic-fetch-add => 'atomic-fetch-add',
  core-atomic-fetch-dec => 'atomic-fetch-dec',
  core-atomic-fetch-inc => 'atomic-fetch-inc',
  core-atomic-fetch-sub => 'atomic-fetch-sub',
  core-atomic-inc-fetch => 'atomic-inc-fetch',
  core-atomic-sub-fetch => 'atomic-sub-fetch',
  core-await            => 'await',

  core-bag => 'bag',

  core-callframe    => 'callframe',
  core-callsame     => 'callsame',
  core-callwith     => 'callwith',
  core-cas          => 'cas',
  core-categorize   => 'categorize',
  core-ceiling      => 'ceiling',
  core-chars        => 'chars',
  core-chdir        => 'chdir',
  core-chmod        => 'chmod',
  core-chomp        => 'chomp',
  core-chop         => 'chop',
  core-chown        => 'chown',
  core-chr          => 'chr',
  core-chrs         => 'chrs',
  core-classify     => 'classify',
  core-close        => 'close',
  core-comb         => 'comb',
  core-combinations => 'combinations',

  core-deepmap => 'deepmap',
  core-defined => 'defined',
  core-die     => 'die',
  core-dir     => 'dir',
  core-done    => 'done',
  core-duckmap => 'duckmap',

  core-elems  => 'elems',
  core-emit   => 'emit',
  core-end    => 'end',
  core-exit   => 'exit',
  core-exp    => 'exp',
  core-expmod => 'expmod',

  core-fail         => 'fail',
  core-fc           => 'fc',
  core-first        => 'first',
  core-flat         => 'flat',
  core-flip         => 'flip',
  core-floor        => 'floor',
  core-full-barrier => 'full-barrier',

  core-get  => 'get',
  core-getc => 'getc',
  core-gist => 'gist',
  core-grep => 'grep',

  core-hash => 'hash',
  core-head => 'head',

  core-indent  => 'indent',
  core-index   => 'index',
  core-indices => 'indices',
  core-indir   => 'indir',
  core-item    => 'item',

  core-join => 'join',

  core-key  => 'key',
  core-keys => 'keys',
  core-kv   => 'kv',

  core-last     => 'last',
  core-lastcall => 'lastcall',
  core-lc       => 'lc',
  core-lines    => 'lines',
  core-link     => 'link',
  core-list     => 'list',
  core-lsb      => 'lsb',

  core-make   => 'make',
  core-map    => 'map',
  core-max    => 'max',
  core-min    => 'min',
  core-minmax => 'minmax',
  core-mix    => 'mix',
  core-mkdir  => 'mkdir',
  core-move   => 'move',
  core-msb    => 'msb',

  core-next       => 'next',
  core-nextcallee => 'nextcallee',
  core-nextsame   => 'nextsame',
  core-nextwith   => 'nextwith',
  core-none       => 'none',
  core-not        => 'not',
  core-note       => 'note',

  core-one  => 'one',
  core-open => 'open',
  core-ord  => 'ord',
  core-ords => 'ords',

  core-pair         => 'pair',
  core-pairs        => 'pairs',
  core-parse-base   => 'parse-base',
  core-permutations => 'permutations',
  core-pick         => 'pick',
  core-pop          => 'pop',
  core-prepend      => 'prepend',
  core-print        => 'print',
  core-printf       => 'printf',
  core-proceed      => 'proceed',
  core-prompt       => 'prompt',
  core-push         => 'push',
  core-put          => 'put',

  core-rand       => 'rand',
  core-redo       => 'redo',
  core-reduce     => 'reduce',
  core-repeated   => 'repeated',
  core-repl       => 'repl',
  core-return     => 'return',
  core-return-rw  => 'return-rw',
  core-reverse    => 'reverse',
  core-rindex     => 'rindex',
  core-rmdir      => 'rmdir',
  core-roll       => 'roll',
  core-rotate     => 'rotate',
  core-round      => 'round',
  core-roundrobin => 'roundrobin',
  core-run        => 'run',

  core-samecase    => 'samecase',
  core-samemark    => 'samemark',
  core-samewith    => 'samewith',
  core-say         => 'say',
  core-set         => 'set',
  core-shell       => 'shell',
  core-shift       => 'shift',
  core-sign        => 'sign',
  core-signal      => 'signal',
  core-skip        => 'skip',
  core-sleep       => 'sleep',
  core-sleep-timer => 'sleep-timer',
  core-sleep-until => 'sleep-until',
  core-slip        => 'slip',
  core-slurp       => 'slurp',
  core-snip        => 'snip',
  core-snitch      => 'snitch',
  core-so          => 'so',
  core-sort        => 'sort',
  core-splice      => 'splice',
  core-split       => 'split',
  core-sprintf     => 'sprintf',
  core-spurt       => 'spurt',
  core-sqrt        => 'sqrt',
  core-squish      => 'squish',
  core-srand       => 'srand',
  core-subbuf      => 'subbuf',
  core-subbuf-rw   => 'subbuf-rw',
  core-succeed     => 'succeed',
  core-sum         => 'sum',
  core-symlink     => 'symlink',

  core-tail          => 'tail',
  core-take          => 'take',
  core-take-rw       => 'take-rw',
  core-tc            => 'tc',
  core-tclc          => 'tclc',
  core-trim          => 'trim',
  core-trim-leading  => 'trim-leading',
  core-trim-trailing => 'trim-trailing',
  core-truncate      => 'truncate',

  core-uc       => 'uc',
  core-unimatch => 'unimatch',
  core-uniname  => 'uniname',
  core-uninames => 'uninames',
  core-uniparse => 'uniparse',
  core-uniprop  => 'uniprop',
  core-uniprops => 'uniprops',
  core-unique   => 'unique',
  core-unival   => 'unival',
  core-univals  => 'univals',
  core-unlink   => 'unlink',
  core-unshift  => 'unshift',

  core-val    => 'val',
  core-value  => 'value',
  core-values => 'values',

  core-warn     => 'warn',
  core-wordcase => 'wordcase',
  core-words    => 'words',

  infix-after      => 'after',
  infix-and        => 'and',
  infix-andthen    => 'andthen',
  infix-before     => 'before',
  infix-but        => 'but',
  infix-cmp        => 'cmp',
  infix-coll       => 'coll',
  infix-div        => 'div',
  infix-does       => 'does',
  infix-eq         => 'eq',
  infix-ff         => 'ff',
  infix-fff        => 'fff',
  infix-gcd        => 'gcd',
  infix-ge         => 'ge',
  infix-gt         => 'gt',
  infix-le         => 'le',
  infix-lcm        => 'lcm',
  infix-leg        => 'leg',
  infix-lt         => 'lt',
  infix-max        => 'max',
  infix-min        => 'min',
  infix-minmax     => 'minmax',
  infix-mod        => 'mod',
  infix-ne         => 'ne',
  infix-notandthen => 'notandthen',
  infix-o          => 'o',
  infix-or         => 'or',
  infix-orelse     => 'orelse',
  infix-unicmp     => 'unicmp',
  infix-x          => 'x',
  infix-X          => 'X',
  infix-xx         => 'xx',
  infix-Z          => 'Z',
  'infix-ff^'      => 'ff^',
  'infix-fff^'     => 'fff^',
  'infix-(cont)'   => '(cont)',
  'infix-(elem)'   => '(elem)',
  'infix-^ff'      => '^ff',
  'infix-^fff'     => '^fff',
  'infix-^ff^'     => '^ff^',
  'infix-^fff^'    => '^fff^',

  modifier-for     => 'for',
  modifier-given   => 'given',
  modifier-if      => 'if',
  modifier-unless  => 'unless',
  modifier-until   => 'until',
  modifier-when    => 'when',
  modifier-while   => 'while',
  modifier-with    => 'with',
  modifier-without => 'without',

  multi-multi => 'multi',
  multi-only  => 'only',
  multi-proto => 'proto',

  package-class   => 'class',
  package-grammar => 'grammar',
  package-module  => 'module',
  package-package => 'package',
  package-role    => 'role',

  phaser-BEGIN   => 'BEGIN',
  phaser-CATCH   => 'CATCH',
  phaser-CHECK   => 'CHECK',
  phaser-CLOSE   => 'CLOSE',
  phaser-CONTROL => 'CONTROL',
  phaser-DOC     => 'DOC',
  phaser-END     => 'END',
  phaser-ENTER   => 'ENTER',
  phaser-FIRST   => 'FIRST',
  phaser-INIT    => 'INIT',
  phaser-KEEP    => 'KEEP',
  phaser-LAST    => 'LAST',
  phaser-LEAVE   => 'LEAVE',
  phaser-NEXT    => 'NEXT',
  phaser-PRE     => 'PRE',
  phaser-POST    => 'POST',
  phaser-QUIT    => 'QUIT',
  phaser-UNDO    => 'UNDO',

  prefix-not => 'not',
  prefix-so  => 'so',

  routine-method    => 'method',
  routine-sub       => 'sub',
  routine-regex     => 'regex',
  routine-rule      => 'rule',
  routine-submethod => 'submethod',
  routine-token     => 'token',

  scope-anon     => 'anon',
  scope-constant => 'constant',
  scope-has      => 'has',
  scope-HAS      => 'HAS',
  scope-my       => 'my',
  scope-our      => 'our',
  scope-state    => 'state',
  scope-unit     => 'unit',

  stmt-prefix-do       => 'do',
  stmt-prefix-eager    => 'eager',
  stmt-prefix-gather   => 'gather',
  stmt-prefix-hyper    => 'hyper',
  stmt-prefix-lazy     => 'lazy',
  stmt-prefix-quietly  => 'quietly',
  stmt-prefix-race     => 'race',
  stmt-prefix-sink     => 'sink',
  stmt-prefix-start    => 'start',
  stmt-prefix-try      => 'try',
  stmt-prefix-react    => 'react',
  stmt-prefix-whenever => 'whenever',

  trait-does    => 'does',
  trait-hides   => 'hides',
  trait-is      => 'is',
  trait-of      => 'of',
  trait-returns => 'returns',

  typer-enum   => 'enum',
  typer-subset => 'subset',

  use-import  => 'import',
  use-need    => 'need',
  use-no      => 'no',
  use-require => 'require',
  use-use     => 'use',
;

# This is an example implementation
method xsyn(str $prefix, str $key) {
    %translation{"$prefix-$key"} // $key
}

# vim: expandtab shiftwidth=4
