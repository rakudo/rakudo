use lib <t/packages/>;
use Test;
use Test::Helpers;
plan 187;

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester <pos>
          
            <pos>    help1
        §usage-msg
    'unchanged usage for MAIN($pos)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $opt-pos?, #= help2
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [<opt-pos>]
          
            [<opt-pos>]    help2
        §usage-msg
    'unchanged usage for MAIN($opt-pos?)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Any]]
          
            --named[=Any]    help3
        §usage-msg
    'unchanged usage for MAIN(:$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any]
          
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(:$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester literal -- help5
        §usage-msg
    'unchanged usage for MAIN(‘literal)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester <Int> -- help6
        §usage-msg
    'unchanged usage for MAIN(In)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        $opt-pos?, #= help2
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester <pos> [<opt-pos>]
          
            <pos>          help1
            [<opt-pos>]    help2
        §usage-msg
    'unchanged usage for MAIN($pos, $opt-pos?)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Any]] <pos>
          
            <pos>            help1
            --named[=Any]    help3
        §usage-msg
    'unchanged usage for MAIN($pos, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] <pos>
          
            <pos>                help1
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        ‘literal’ #= help5
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester <pos> literal
          
            <pos>    help1 help5
        §usage-msg
    'unchanged usage for MAIN($pos, ‘literal)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester <pos> -- help5
          
            <pos>    help1
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        Int #= help6
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester <pos> <Int>
          
            <pos>    help1 help6
        §usage-msg
    'unchanged usage for MAIN($pos, In)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester <pos> -- help6
          
            <pos>    help1
        §usage-msg
    'unchanged usage for MAIN(Int $pos)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $opt-pos?, #= help2
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Any]] [<opt-pos>]
          
            [<opt-pos>]      help2
            --named[=Any]    help3
        §usage-msg
    'unchanged usage for MAIN($opt-pos?, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [<opt-pos>]
          
            [<opt-pos>]          help2
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN($opt-pos?, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [<opt-pos>] -- help5
          
            [<opt-pos>]    help2
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $opt-pos?)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $opt-pos?, #= help2
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [<opt-pos>] -- help6
          
            [<opt-pos>]    help2
        §usage-msg
    'unchanged usage for MAIN(Int $opt-pos?)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]]
          
            --named[=Any]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(:$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]]
          
            --req-named[=Any]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN(:$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named=<literal>] -- help5
          
            --named=<literal>    help3
        §usage-msg
    'unchanged usage for MAIN(‘literal’ :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Int]] -- help6
          
            --named[=Int]    help3
        §usage-msg
    'unchanged usage for MAIN(Int :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> -- help5
          
            --req-named=<literal>    help4
        §usage-msg
    'unchanged usage for MAIN(‘literal’ :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] -- help6
          
            --req-named[=Int]    help4
        §usage-msg
    'unchanged usage for MAIN(Int :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        $opt-pos?, #= help2
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Any]] <pos> [<opt-pos>]
          
            <pos>            help1
            [<opt-pos>]      help2
            --named[=Any]    help3
        §usage-msg
    'unchanged usage for MAIN($pos, $opt-pos?, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] <pos> [<opt-pos>]
          
            <pos>                help1
            [<opt-pos>]          help2
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, $opt-pos?, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester <pos> [<opt-pos>]
          
            <pos>          help1 help5
            [<opt-pos>]    help2
        §usage-msg
    'unchanged usage for MAIN($pos, ‘literal’ $opt-pos?)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        $opt-pos?, #= help2
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester <pos> [<opt-pos>] -- help5
          
            <pos>          help1
            [<opt-pos>]    help2
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, $opt-pos?)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        Int #= help6
                        $opt-pos?, #= help2
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester <pos> [<opt-pos>]
          
            <pos>          help1 help6
            [<opt-pos>]    help2
        §usage-msg
    'unchanged usage for MAIN($pos, Int $opt-pos?)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        $opt-pos?, #= help2
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester <pos> [<opt-pos>] -- help6
          
            <pos>          help1
            [<opt-pos>]    help2
        §usage-msg
    'unchanged usage for MAIN(Int $pos, $opt-pos?)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] <pos>
          
            <pos>                help1
            --named[=Any]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] <pos>
          
            <pos>                help1
            --req-named[=Any]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN($pos, :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        ‘literal’ #= help5
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named=<literal>] <pos>
          
            <pos>                help1 help5
            --named=<literal>    help3
        §usage-msg
    'unchanged usage for MAIN($pos, ‘literal’ :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Any]] <pos> -- help5
          
            <pos>            help1
            --named[=Any]    help3
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        Int #= help6
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Int]] <pos>
          
            <pos>            help1 help6
            --named[=Int]    help3
        §usage-msg
    'unchanged usage for MAIN($pos, Int :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Any]] <pos> -- help6
          
            <pos>            help1
            --named[=Any]    help3
        §usage-msg
    'unchanged usage for MAIN(Int $pos, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> <pos>
          
            <pos>                    help1 help5
            --req-named=<literal>    help4
        §usage-msg
    'unchanged usage for MAIN($pos, ‘literal’ :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] <pos> -- help5
          
            <pos>                help1
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        Int #= help6
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] <pos>
          
            <pos>                help1 help6
            --req-named[=Int]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, Int :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] <pos> -- help6
          
            <pos>                help1
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(Int $pos, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        Int #= help6
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester <pos> <Int> -- help5
          
            <pos>    help1 help6
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, In)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        ‘literal’ #= help5
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester <pos> literal -- help6
          
            <pos>    help1 help5
        §usage-msg
    'unchanged usage for MAIN(Int $pos, ‘literal)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $opt-pos?, #= help2
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] [<opt-pos>]
          
            [<opt-pos>]          help2
            --named[=Any]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN($opt-pos?, :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] [<opt-pos>]
          
            [<opt-pos>]          help2
            --req-named[=Any]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN($opt-pos?, :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named=<literal>] [<opt-pos>]
          
            [<opt-pos>]          help2 help5
            --named=<literal>    help3
        §usage-msg
    'unchanged usage for MAIN($opt-pos?, ‘literal’ :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Any]] [<opt-pos>] -- help5
          
            [<opt-pos>]      help2
            --named[=Any]    help3
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $opt-pos?, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $opt-pos?, #= help2
                        Int #= help6
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Int]] [<opt-pos>]
          
            [<opt-pos>]      help2 help6
            --named[=Int]    help3
        §usage-msg
    'unchanged usage for MAIN($opt-pos?, Int :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $opt-pos?, #= help2
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Any]] [<opt-pos>] -- help6
          
            [<opt-pos>]      help2
            --named[=Any]    help3
        §usage-msg
    'unchanged usage for MAIN(Int $opt-pos?, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [<opt-pos>]
          
            [<opt-pos>]              help2 help5
            --req-named=<literal>    help4
        §usage-msg
    'unchanged usage for MAIN($opt-pos?, ‘literal’ :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [<opt-pos>] -- help5
          
            [<opt-pos>]          help2
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $opt-pos?, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $opt-pos?, #= help2
                        Int #= help6
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [<opt-pos>]
          
            [<opt-pos>]          help2 help6
            --req-named[=Int]    help4
        §usage-msg
    'unchanged usage for MAIN($opt-pos?, Int :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [<opt-pos>] -- help6
          
            [<opt-pos>]          help2
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(Int $opt-pos?, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        :$named, #= help3
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Any]]
          
            --named[=Any]            help3 help5
            --req-named=<literal>    help4
        §usage-msg
    'unchanged usage for MAIN(:$named, ‘literal’ :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        :$req-named!, #= help4
                        ‘literal’ #= help5
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named=<literal>]
          
            --req-named[=Any]    help4 help5
            --named=<literal>    help3
        §usage-msg
    'unchanged usage for MAIN(:$req-named!, ‘literal’ :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named=<literal>] -- help5
          
            --named=<literal>    help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(‘literal’ :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Any]] -- help5
          
            --req-named=<literal>    help4
            --named[=Any]            help3
        §usage-msg
    'unchanged usage for MAIN(‘literal’ :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        :$named, #= help3
                        Int #= help6
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named[=Any]]
          
            --named[=Any]        help3 help6
            --req-named[=Int]    help4
        §usage-msg
    'unchanged usage for MAIN(:$named, Int :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        :$req-named!, #= help4
                        Int #= help6
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Int]]
          
            --req-named[=Any]    help4 help6
            --named[=Int]        help3
        §usage-msg
    'unchanged usage for MAIN(:$req-named!, Int :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Int]] -- help6
          
            --named[=Int]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(Int :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named[=Any]] -- help6
          
            --req-named[=Int]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN(Int :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        $opt-pos?, #= help2
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] <pos> [<opt-pos>]
          
            <pos>                help1
            [<opt-pos>]          help2
            --named[=Any]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, $opt-pos?, :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] <pos> [<opt-pos>]
          
            <pos>                help1
            [<opt-pos>]          help2
            --req-named[=Any]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN($pos, $opt-pos?, :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named=<literal>] <pos> [<opt-pos>]
          
            <pos>                help1
            [<opt-pos>]          help2 help5
            --named=<literal>    help3
        §usage-msg
    'unchanged usage for MAIN($pos, $opt-pos?, ‘literal’ :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Any]] <pos> [<opt-pos>]
          
            <pos>            help1 help5
            [<opt-pos>]      help2
            --named[=Any]    help3
        §usage-msg
    'unchanged usage for MAIN($pos, ‘literal’ $opt-pos?, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        $opt-pos?, #= help2
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Any]] <pos> [<opt-pos>] -- help5
          
            <pos>            help1
            [<opt-pos>]      help2
            --named[=Any]    help3
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, $opt-pos?, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        $opt-pos?, #= help2
                        Int #= help6
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Int]] <pos> [<opt-pos>]
          
            <pos>            help1
            [<opt-pos>]      help2 help6
            --named[=Int]    help3
        §usage-msg
    'unchanged usage for MAIN($pos, $opt-pos?, Int :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        Int #= help6
                        $opt-pos?, #= help2
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Any]] <pos> [<opt-pos>]
          
            <pos>            help1 help6
            [<opt-pos>]      help2
            --named[=Any]    help3
        §usage-msg
    'unchanged usage for MAIN($pos, Int $opt-pos?, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        $opt-pos?, #= help2
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Any]] <pos> [<opt-pos>] -- help6
          
            <pos>            help1
            [<opt-pos>]      help2
            --named[=Any]    help3
        §usage-msg
    'unchanged usage for MAIN(Int $pos, $opt-pos?, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> <pos> [<opt-pos>]
          
            <pos>                    help1
            [<opt-pos>]              help2 help5
            --req-named=<literal>    help4
        §usage-msg
    'unchanged usage for MAIN($pos, $opt-pos?, ‘literal’ :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] <pos> [<opt-pos>]
          
            <pos>                help1 help5
            [<opt-pos>]          help2
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, ‘literal’ $opt-pos?, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] <pos> [<opt-pos>] -- help5
          
            <pos>                help1
            [<opt-pos>]          help2
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, $opt-pos?, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        $opt-pos?, #= help2
                        Int #= help6
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] <pos> [<opt-pos>]
          
            <pos>                help1
            [<opt-pos>]          help2 help6
            --req-named[=Int]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, $opt-pos?, Int :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        Int #= help6
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] <pos> [<opt-pos>]
          
            <pos>                help1 help6
            [<opt-pos>]          help2
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, Int $opt-pos?, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] <pos> [<opt-pos>] -- help6
          
            <pos>                help1
            [<opt-pos>]          help2
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(Int $pos, $opt-pos?, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        Int #= help6
                        $opt-pos?, #= help2
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester <pos> [<opt-pos>] -- help5
          
            <pos>          help1 help6
            [<opt-pos>]    help2
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, Int $opt-pos?)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester <pos> [<opt-pos>] -- help6
          
            <pos>          help1 help5
            [<opt-pos>]    help2
        §usage-msg
    'unchanged usage for MAIN(Int $pos, ‘literal’ $opt-pos?)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        :$named, #= help3
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Any]] <pos>
          
            <pos>                    help1
            --named[=Any]            help3 help5
            --req-named=<literal>    help4
        §usage-msg
    'unchanged usage for MAIN($pos, :$named, ‘literal’ :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        :$req-named!, #= help4
                        ‘literal’ #= help5
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named=<literal>] <pos>
          
            <pos>                help1
            --req-named[=Any]    help4 help5
            --named=<literal>    help3
        §usage-msg
    'unchanged usage for MAIN($pos, :$req-named!, ‘literal’ :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        ‘literal’ #= help5
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named=<literal>] <pos>
          
            <pos>                help1 help5
            --named=<literal>    help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, ‘literal’ :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Any]] <pos>
          
            <pos>                    help1 help5
            --req-named=<literal>    help4
            --named[=Any]            help3
        §usage-msg
    'unchanged usage for MAIN($pos, ‘literal’ :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] <pos> -- help5
          
            <pos>                help1
            --named[=Any]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] <pos> -- help5
          
            <pos>                help1
            --req-named[=Any]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        :$named, #= help3
                        Int #= help6
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named[=Any]] <pos>
          
            <pos>                help1
            --named[=Any]        help3 help6
            --req-named[=Int]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, :$named, Int :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        :$req-named!, #= help4
                        Int #= help6
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Int]] <pos>
          
            <pos>                help1
            --req-named[=Any]    help4 help6
            --named[=Int]        help3
        §usage-msg
    'unchanged usage for MAIN($pos, :$req-named!, Int :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        Int #= help6
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Int]] <pos>
          
            <pos>                help1 help6
            --named[=Int]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, Int :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        Int #= help6
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named[=Any]] <pos>
          
            <pos>                help1 help6
            --req-named[=Int]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN($pos, Int :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] <pos> -- help6
          
            <pos>                help1
            --named[=Any]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(Int $pos, :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] <pos> -- help6
          
            <pos>                help1
            --req-named[=Any]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN(Int $pos, :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        Int #= help6
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Int]] <pos> -- help5
          
            <pos>            help1 help6
            --named[=Int]    help3
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, Int :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        ‘literal’ #= help5
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named=<literal>] <pos> -- help6
          
            <pos>                help1 help5
            --named=<literal>    help3
        §usage-msg
    'unchanged usage for MAIN(Int $pos, ‘literal’ :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        Int #= help6
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] <pos> -- help5
          
            <pos>                help1 help6
            --req-named[=Int]    help4
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, Int :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> <pos> -- help6
          
            <pos>                    help1 help5
            --req-named=<literal>    help4
        §usage-msg
    'unchanged usage for MAIN(Int $pos, ‘literal’ :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $opt-pos?, #= help2
                        :$named, #= help3
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Any]] [<opt-pos>]
          
            [<opt-pos>]              help2
            --named[=Any]            help3 help5
            --req-named=<literal>    help4
        §usage-msg
    'unchanged usage for MAIN($opt-pos?, :$named, ‘literal’ :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                        ‘literal’ #= help5
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named=<literal>] [<opt-pos>]
          
            [<opt-pos>]          help2
            --req-named[=Any]    help4 help5
            --named=<literal>    help3
        §usage-msg
    'unchanged usage for MAIN($opt-pos?, :$req-named!, ‘literal’ :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named=<literal>] [<opt-pos>]
          
            [<opt-pos>]          help2 help5
            --named=<literal>    help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN($opt-pos?, ‘literal’ :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Any]] [<opt-pos>]
          
            [<opt-pos>]              help2 help5
            --req-named=<literal>    help4
            --named[=Any]            help3
        §usage-msg
    'unchanged usage for MAIN($opt-pos?, ‘literal’ :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] [<opt-pos>] -- help5
          
            [<opt-pos>]          help2
            --named[=Any]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $opt-pos?, :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] [<opt-pos>] -- help5
          
            [<opt-pos>]          help2
            --req-named[=Any]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $opt-pos?, :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $opt-pos?, #= help2
                        :$named, #= help3
                        Int #= help6
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named[=Any]] [<opt-pos>]
          
            [<opt-pos>]          help2
            --named[=Any]        help3 help6
            --req-named[=Int]    help4
        §usage-msg
    'unchanged usage for MAIN($opt-pos?, :$named, Int :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                        Int #= help6
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Int]] [<opt-pos>]
          
            [<opt-pos>]          help2
            --req-named[=Any]    help4 help6
            --named[=Int]        help3
        §usage-msg
    'unchanged usage for MAIN($opt-pos?, :$req-named!, Int :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $opt-pos?, #= help2
                        Int #= help6
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Int]] [<opt-pos>]
          
            [<opt-pos>]          help2 help6
            --named[=Int]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN($opt-pos?, Int :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $opt-pos?, #= help2
                        Int #= help6
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named[=Any]] [<opt-pos>]
          
            [<opt-pos>]          help2 help6
            --req-named[=Int]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN($opt-pos?, Int :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $opt-pos?, #= help2
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] [<opt-pos>] -- help6
          
            [<opt-pos>]          help2
            --named[=Any]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(Int $opt-pos?, :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] [<opt-pos>] -- help6
          
            [<opt-pos>]          help2
            --req-named[=Any]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN(Int $opt-pos?, :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        Int #= help6
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Int]] [<opt-pos>] -- help5
          
            [<opt-pos>]      help2 help6
            --named[=Int]    help3
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $opt-pos?, Int :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named=<literal>] [<opt-pos>] -- help6
          
            [<opt-pos>]          help2 help5
            --named=<literal>    help3
        §usage-msg
    'unchanged usage for MAIN(Int $opt-pos?, ‘literal’ :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        Int #= help6
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [<opt-pos>] -- help5
          
            [<opt-pos>]          help2 help6
            --req-named[=Int]    help4
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $opt-pos?, Int :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [<opt-pos>] -- help6
          
            [<opt-pos>]              help2 help5
            --req-named=<literal>    help4
        §usage-msg
    'unchanged usage for MAIN(Int $opt-pos?, ‘literal’ :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        :$named, #= help3
                        Int #= help6
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named=<literal>] -- help5
          
            --named=<literal>    help3 help6
            --req-named[=Int]    help4
        §usage-msg
    'unchanged usage for MAIN(‘literal’ :$named, Int :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                        Int #= help6
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Int]] -- help5
          
            --req-named=<literal>    help4 help6
            --named[=Int]            help3
        §usage-msg
    'unchanged usage for MAIN(‘literal’ :$req-named!, Int :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        :$named, #= help3
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Int]] -- help6
          
            --named[=Int]            help3 help5
            --req-named=<literal>    help4
        §usage-msg
    'unchanged usage for MAIN(Int :$named, ‘literal’ :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        :$req-named!, #= help4
                        ‘literal’ #= help5
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named=<literal>] -- help6
          
            --req-named[=Int]    help4 help5
            --named=<literal>    help3
        §usage-msg
    'unchanged usage for MAIN(Int :$req-named!, ‘literal’ :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        $opt-pos?, #= help2
                        :$named, #= help3
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Any]] <pos> [<opt-pos>]
          
            <pos>                    help1
            [<opt-pos>]              help2
            --named[=Any]            help3 help5
            --req-named=<literal>    help4
        §usage-msg
    'unchanged usage for MAIN($pos, $opt-pos?, :$named, ‘literal’ :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                        ‘literal’ #= help5
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named=<literal>] <pos> [<opt-pos>]
          
            <pos>                help1
            [<opt-pos>]          help2
            --req-named[=Any]    help4 help5
            --named=<literal>    help3
        §usage-msg
    'unchanged usage for MAIN($pos, $opt-pos?, :$req-named!, ‘literal’ :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named=<literal>] <pos> [<opt-pos>]
          
            <pos>                help1
            [<opt-pos>]          help2 help5
            --named=<literal>    help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, $opt-pos?, ‘literal’ :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Any]] <pos> [<opt-pos>]
          
            <pos>                    help1
            [<opt-pos>]              help2 help5
            --req-named=<literal>    help4
            --named[=Any]            help3
        §usage-msg
    'unchanged usage for MAIN($pos, $opt-pos?, ‘literal’ :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] <pos> [<opt-pos>]
          
            <pos>                help1 help5
            [<opt-pos>]          help2
            --named[=Any]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, ‘literal’ $opt-pos?, :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] <pos> [<opt-pos>]
          
            <pos>                help1 help5
            [<opt-pos>]          help2
            --req-named[=Any]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN($pos, ‘literal’ $opt-pos?, :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        $opt-pos?, #= help2
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] <pos> [<opt-pos>] -- help5
          
            <pos>                help1
            [<opt-pos>]          help2
            --named[=Any]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, $opt-pos?, :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] <pos> [<opt-pos>] -- help5
          
            <pos>                help1
            [<opt-pos>]          help2
            --req-named[=Any]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, $opt-pos?, :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        $opt-pos?, #= help2
                        :$named, #= help3
                        Int #= help6
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named[=Any]] <pos> [<opt-pos>]
          
            <pos>                help1
            [<opt-pos>]          help2
            --named[=Any]        help3 help6
            --req-named[=Int]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, $opt-pos?, :$named, Int :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                        Int #= help6
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Int]] <pos> [<opt-pos>]
          
            <pos>                help1
            [<opt-pos>]          help2
            --req-named[=Any]    help4 help6
            --named[=Int]        help3
        §usage-msg
    'unchanged usage for MAIN($pos, $opt-pos?, :$req-named!, Int :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        $opt-pos?, #= help2
                        Int #= help6
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Int]] <pos> [<opt-pos>]
          
            <pos>                help1
            [<opt-pos>]          help2 help6
            --named[=Int]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, $opt-pos?, Int :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        $opt-pos?, #= help2
                        Int #= help6
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named[=Any]] <pos> [<opt-pos>]
          
            <pos>                help1
            [<opt-pos>]          help2 help6
            --req-named[=Int]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN($pos, $opt-pos?, Int :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        Int #= help6
                        $opt-pos?, #= help2
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] <pos> [<opt-pos>]
          
            <pos>                help1 help6
            [<opt-pos>]          help2
            --named[=Any]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, Int $opt-pos?, :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        Int #= help6
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] <pos> [<opt-pos>]
          
            <pos>                help1 help6
            [<opt-pos>]          help2
            --req-named[=Any]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN($pos, Int $opt-pos?, :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        $opt-pos?, #= help2
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] <pos> [<opt-pos>] -- help6
          
            <pos>                help1
            [<opt-pos>]          help2
            --named[=Any]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(Int $pos, $opt-pos?, :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] <pos> [<opt-pos>] -- help6
          
            <pos>                help1
            [<opt-pos>]          help2
            --req-named[=Any]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN(Int $pos, $opt-pos?, :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        Int #= help6
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Int]] <pos> [<opt-pos>]
          
            <pos>            help1 help5
            [<opt-pos>]      help2 help6
            --named[=Int]    help3
        §usage-msg
    'unchanged usage for MAIN($pos, ‘literal’ $opt-pos?, Int :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        Int #= help6
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named=<literal>] <pos> [<opt-pos>]
          
            <pos>                help1 help6
            [<opt-pos>]          help2 help5
            --named=<literal>    help3
        §usage-msg
    'unchanged usage for MAIN($pos, Int $opt-pos?, ‘literal’ :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        $opt-pos?, #= help2
                        Int #= help6
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Int]] <pos> [<opt-pos>] -- help5
          
            <pos>            help1
            [<opt-pos>]      help2 help6
            --named[=Int]    help3
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, $opt-pos?, Int :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        Int #= help6
                        $opt-pos?, #= help2
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Any]] <pos> [<opt-pos>] -- help5
          
            <pos>            help1 help6
            [<opt-pos>]      help2
            --named[=Any]    help3
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, Int $opt-pos?, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named=<literal>] <pos> [<opt-pos>] -- help6
          
            <pos>                help1
            [<opt-pos>]          help2 help5
            --named=<literal>    help3
        §usage-msg
    'unchanged usage for MAIN(Int $pos, $opt-pos?, ‘literal’ :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester [--named[=Any]] <pos> [<opt-pos>] -- help6
          
            <pos>            help1 help5
            [<opt-pos>]      help2
            --named[=Any]    help3
        §usage-msg
    'unchanged usage for MAIN(Int $pos, ‘literal’ $opt-pos?, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        Int #= help6
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] <pos> [<opt-pos>]
          
            <pos>                help1 help5
            [<opt-pos>]          help2 help6
            --req-named[=Int]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, ‘literal’ $opt-pos?, Int :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        Int #= help6
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> <pos> [<opt-pos>]
          
            <pos>                    help1 help6
            [<opt-pos>]              help2 help5
            --req-named=<literal>    help4
        §usage-msg
    'unchanged usage for MAIN($pos, Int $opt-pos?, ‘literal’ :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        $opt-pos?, #= help2
                        Int #= help6
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] <pos> [<opt-pos>] -- help5
          
            <pos>                help1
            [<opt-pos>]          help2 help6
            --req-named[=Int]    help4
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, $opt-pos?, Int :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        Int #= help6
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] <pos> [<opt-pos>] -- help5
          
            <pos>                help1 help6
            [<opt-pos>]          help2
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, Int $opt-pos?, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> <pos> [<opt-pos>] -- help6
          
            <pos>                    help1
            [<opt-pos>]              help2 help5
            --req-named=<literal>    help4
        §usage-msg
    'unchanged usage for MAIN(Int $pos, $opt-pos?, ‘literal’ :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] <pos> [<opt-pos>] -- help6
          
            <pos>                help1 help5
            [<opt-pos>]          help2
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(Int $pos, ‘literal’ $opt-pos?, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        ‘literal’ #= help5
                        :$named, #= help3
                        Int #= help6
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named=<literal>] <pos>
          
            <pos>                help1 help5
            --named=<literal>    help3 help6
            --req-named[=Int]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, ‘literal’ :$named, Int :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                        Int #= help6
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Int]] <pos>
          
            <pos>                    help1 help5
            --req-named=<literal>    help4 help6
            --named[=Int]            help3
        §usage-msg
    'unchanged usage for MAIN($pos, ‘literal’ :$req-named!, Int :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        Int #= help6
                        :$named, #= help3
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Int]] <pos>
          
            <pos>                    help1 help6
            --named[=Int]            help3 help5
            --req-named=<literal>    help4
        §usage-msg
    'unchanged usage for MAIN($pos, Int :$named, ‘literal’ :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        Int #= help6
                        :$req-named!, #= help4
                        ‘literal’ #= help5
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named=<literal>] <pos>
          
            <pos>                help1 help6
            --req-named[=Int]    help4 help5
            --named=<literal>    help3
        §usage-msg
    'unchanged usage for MAIN($pos, Int :$req-named!, ‘literal’ :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        :$named, #= help3
                        Int #= help6
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named[=Any]] <pos> -- help5
          
            <pos>                help1
            --named[=Any]        help3 help6
            --req-named[=Int]    help4
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, :$named, Int :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        :$req-named!, #= help4
                        Int #= help6
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Int]] <pos> -- help5
          
            <pos>                help1
            --req-named[=Any]    help4 help6
            --named[=Int]        help3
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, :$req-named!, Int :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        Int #= help6
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Int]] <pos> -- help5
          
            <pos>                help1 help6
            --named[=Int]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, Int :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        Int #= help6
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named[=Any]] <pos> -- help5
          
            <pos>                help1 help6
            --req-named[=Int]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, Int :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        :$named, #= help3
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Any]] <pos> -- help6
          
            <pos>                    help1
            --named[=Any]            help3 help5
            --req-named=<literal>    help4
        §usage-msg
    'unchanged usage for MAIN(Int $pos, :$named, ‘literal’ :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        :$req-named!, #= help4
                        ‘literal’ #= help5
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named=<literal>] <pos> -- help6
          
            <pos>                help1
            --req-named[=Any]    help4 help5
            --named=<literal>    help3
        §usage-msg
    'unchanged usage for MAIN(Int $pos, :$req-named!, ‘literal’ :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        ‘literal’ #= help5
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named=<literal>] <pos> -- help6
          
            <pos>                help1 help5
            --named=<literal>    help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(Int $pos, ‘literal’ :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Any]] <pos> -- help6
          
            <pos>                    help1 help5
            --req-named=<literal>    help4
            --named[=Any]            help3
        §usage-msg
    'unchanged usage for MAIN(Int $pos, ‘literal’ :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$named, #= help3
                        Int #= help6
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named=<literal>] [<opt-pos>]
          
            [<opt-pos>]          help2 help5
            --named=<literal>    help3 help6
            --req-named[=Int]    help4
        §usage-msg
    'unchanged usage for MAIN($opt-pos?, ‘literal’ :$named, Int :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                        Int #= help6
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Int]] [<opt-pos>]
          
            [<opt-pos>]              help2 help5
            --req-named=<literal>    help4 help6
            --named[=Int]            help3
        §usage-msg
    'unchanged usage for MAIN($opt-pos?, ‘literal’ :$req-named!, Int :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $opt-pos?, #= help2
                        Int #= help6
                        :$named, #= help3
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Int]] [<opt-pos>]
          
            [<opt-pos>]              help2 help6
            --named[=Int]            help3 help5
            --req-named=<literal>    help4
        §usage-msg
    'unchanged usage for MAIN($opt-pos?, Int :$named, ‘literal’ :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $opt-pos?, #= help2
                        Int #= help6
                        :$req-named!, #= help4
                        ‘literal’ #= help5
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named=<literal>] [<opt-pos>]
          
            [<opt-pos>]          help2 help6
            --req-named[=Int]    help4 help5
            --named=<literal>    help3
        §usage-msg
    'unchanged usage for MAIN($opt-pos?, Int :$req-named!, ‘literal’ :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        :$named, #= help3
                        Int #= help6
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named[=Any]] [<opt-pos>] -- help5
          
            [<opt-pos>]          help2
            --named[=Any]        help3 help6
            --req-named[=Int]    help4
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $opt-pos?, :$named, Int :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                        Int #= help6
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Int]] [<opt-pos>] -- help5
          
            [<opt-pos>]          help2
            --req-named[=Any]    help4 help6
            --named[=Int]        help3
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $opt-pos?, :$req-named!, Int :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        Int #= help6
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Int]] [<opt-pos>] -- help5
          
            [<opt-pos>]          help2 help6
            --named[=Int]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $opt-pos?, Int :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        Int #= help6
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named[=Any]] [<opt-pos>] -- help5
          
            [<opt-pos>]          help2 help6
            --req-named[=Int]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $opt-pos?, Int :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $opt-pos?, #= help2
                        :$named, #= help3
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Any]] [<opt-pos>] -- help6
          
            [<opt-pos>]              help2
            --named[=Any]            help3 help5
            --req-named=<literal>    help4
        §usage-msg
    'unchanged usage for MAIN(Int $opt-pos?, :$named, ‘literal’ :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                        ‘literal’ #= help5
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named=<literal>] [<opt-pos>] -- help6
          
            [<opt-pos>]          help2
            --req-named[=Any]    help4 help5
            --named=<literal>    help3
        §usage-msg
    'unchanged usage for MAIN(Int $opt-pos?, :$req-named!, ‘literal’ :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named=<literal>] [<opt-pos>] -- help6
          
            [<opt-pos>]          help2 help5
            --named=<literal>    help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(Int $opt-pos?, ‘literal’ :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Any]] [<opt-pos>] -- help6
          
            [<opt-pos>]              help2 help5
            --req-named=<literal>    help4
            --named[=Any]            help3
        §usage-msg
    'unchanged usage for MAIN(Int $opt-pos?, ‘literal’ :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$named, #= help3
                        Int #= help6
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named=<literal>] <pos> [<opt-pos>]
          
            <pos>                help1
            [<opt-pos>]          help2 help5
            --named=<literal>    help3 help6
            --req-named[=Int]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, $opt-pos?, ‘literal’ :$named, Int :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                        Int #= help6
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Int]] <pos> [<opt-pos>]
          
            <pos>                    help1
            [<opt-pos>]              help2 help5
            --req-named=<literal>    help4 help6
            --named[=Int]            help3
        §usage-msg
    'unchanged usage for MAIN($pos, $opt-pos?, ‘literal’ :$req-named!, Int :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        $opt-pos?, #= help2
                        Int #= help6
                        :$named, #= help3
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Int]] <pos> [<opt-pos>]
          
            <pos>                    help1
            [<opt-pos>]              help2 help6
            --named[=Int]            help3 help5
            --req-named=<literal>    help4
        §usage-msg
    'unchanged usage for MAIN($pos, $opt-pos?, Int :$named, ‘literal’ :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        $opt-pos?, #= help2
                        Int #= help6
                        :$req-named!, #= help4
                        ‘literal’ #= help5
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named=<literal>] <pos> [<opt-pos>]
          
            <pos>                help1
            [<opt-pos>]          help2 help6
            --req-named[=Int]    help4 help5
            --named=<literal>    help3
        §usage-msg
    'unchanged usage for MAIN($pos, $opt-pos?, Int :$req-named!, ‘literal’ :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        :$named, #= help3
                        Int #= help6
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named[=Any]] <pos> [<opt-pos>]
          
            <pos>                help1 help5
            [<opt-pos>]          help2
            --named[=Any]        help3 help6
            --req-named[=Int]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, ‘literal’ $opt-pos?, :$named, Int :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                        Int #= help6
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Int]] <pos> [<opt-pos>]
          
            <pos>                help1 help5
            [<opt-pos>]          help2
            --req-named[=Any]    help4 help6
            --named[=Int]        help3
        §usage-msg
    'unchanged usage for MAIN($pos, ‘literal’ $opt-pos?, :$req-named!, Int :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        Int #= help6
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Int]] <pos> [<opt-pos>]
          
            <pos>                help1 help5
            [<opt-pos>]          help2 help6
            --named[=Int]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, ‘literal’ $opt-pos?, Int :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        Int #= help6
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named[=Any]] <pos> [<opt-pos>]
          
            <pos>                help1 help5
            [<opt-pos>]          help2 help6
            --req-named[=Int]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN($pos, ‘literal’ $opt-pos?, Int :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        Int #= help6
                        $opt-pos?, #= help2
                        :$named, #= help3
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Any]] <pos> [<opt-pos>]
          
            <pos>                    help1 help6
            [<opt-pos>]              help2
            --named[=Any]            help3 help5
            --req-named=<literal>    help4
        §usage-msg
    'unchanged usage for MAIN($pos, Int $opt-pos?, :$named, ‘literal’ :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        Int #= help6
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                        ‘literal’ #= help5
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named=<literal>] <pos> [<opt-pos>]
          
            <pos>                help1 help6
            [<opt-pos>]          help2
            --req-named[=Any]    help4 help5
            --named=<literal>    help3
        §usage-msg
    'unchanged usage for MAIN($pos, Int $opt-pos?, :$req-named!, ‘literal’ :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        Int #= help6
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named=<literal>] <pos> [<opt-pos>]
          
            <pos>                help1 help6
            [<opt-pos>]          help2 help5
            --named=<literal>    help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN($pos, Int $opt-pos?, ‘literal’ :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        $pos, #= help1
                        Int #= help6
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Any]] <pos> [<opt-pos>]
          
            <pos>                    help1 help6
            [<opt-pos>]              help2 help5
            --req-named=<literal>    help4
            --named[=Any]            help3
        §usage-msg
    'unchanged usage for MAIN($pos, Int $opt-pos?, ‘literal’ :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        $opt-pos?, #= help2
                        :$named, #= help3
                        Int #= help6
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named[=Any]] <pos> [<opt-pos>] -- help5
          
            <pos>                help1
            [<opt-pos>]          help2
            --named[=Any]        help3 help6
            --req-named[=Int]    help4
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, $opt-pos?, :$named, Int :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                        Int #= help6
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Int]] <pos> [<opt-pos>] -- help5
          
            <pos>                help1
            [<opt-pos>]          help2
            --req-named[=Any]    help4 help6
            --named[=Int]        help3
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, $opt-pos?, :$req-named!, Int :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        $opt-pos?, #= help2
                        Int #= help6
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Int]] <pos> [<opt-pos>] -- help5
          
            <pos>                help1
            [<opt-pos>]          help2 help6
            --named[=Int]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, $opt-pos?, Int :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        $opt-pos?, #= help2
                        Int #= help6
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Int] [--named[=Any]] <pos> [<opt-pos>] -- help5
          
            <pos>                help1
            [<opt-pos>]          help2 help6
            --req-named[=Int]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, $opt-pos?, Int :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        Int #= help6
                        $opt-pos?, #= help2
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] <pos> [<opt-pos>] -- help5
          
            <pos>                help1 help6
            [<opt-pos>]          help2
            --named[=Any]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, Int $opt-pos?, :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        ‘literal’ #= help5
                        $pos, #= help1
                        Int #= help6
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] <pos> [<opt-pos>] -- help5
          
            <pos>                help1 help6
            [<opt-pos>]          help2
            --req-named[=Any]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN(‘literal’ $pos, Int $opt-pos?, :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        $opt-pos?, #= help2
                        :$named, #= help3
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Any]] <pos> [<opt-pos>] -- help6
          
            <pos>                    help1
            [<opt-pos>]              help2
            --named[=Any]            help3 help5
            --req-named=<literal>    help4
        §usage-msg
    'unchanged usage for MAIN(Int $pos, $opt-pos?, :$named, ‘literal’ :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                        ‘literal’ #= help5
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named=<literal>] <pos> [<opt-pos>] -- help6
          
            <pos>                help1
            [<opt-pos>]          help2
            --req-named[=Any]    help4 help5
            --named=<literal>    help3
        §usage-msg
    'unchanged usage for MAIN(Int $pos, $opt-pos?, :$req-named!, ‘literal’ :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named=<literal>] <pos> [<opt-pos>] -- help6
          
            <pos>                help1
            [<opt-pos>]          help2 help5
            --named=<literal>    help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(Int $pos, $opt-pos?, ‘literal’ :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        $opt-pos?, #= help2
                        ‘literal’ #= help5
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named=<literal> [--named[=Any]] <pos> [<opt-pos>] -- help6
          
            <pos>                    help1
            [<opt-pos>]              help2 help5
            --req-named=<literal>    help4
            --named[=Any]            help3
        §usage-msg
    'unchanged usage for MAIN(Int $pos, $opt-pos?, ‘literal’ :$req-named!, :$named)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        :$named, #= help3
                        :$req-named!, #= help4
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] <pos> [<opt-pos>] -- help6
          
            <pos>                help1 help5
            [<opt-pos>]          help2
            --named[=Any]        help3
            --req-named[=Any]    help4
        §usage-msg
    'unchanged usage for MAIN(Int $pos, ‘literal’ $opt-pos?, :$named, :$req-named!)'
);

is-run(
    q:to/§script/,
        my @*ARGS         = '--help';
        my $*PROGRAM-NAME = 'usage-tester';
        my &main = sub MAIN(
                        Int #= help6
                        $pos, #= help1
                        ‘literal’ #= help5
                        $opt-pos?, #= help2
                        :$req-named!, #= help4
                        :$named, #= help3
                      ) {};
        RUN-MAIN(&main, Nil);
        §script
    :out(q:to/§usage-msg/),
        Usage:
          usage-tester --req-named[=Any] [--named[=Any]] <pos> [<opt-pos>] -- help6
          
            <pos>                help1 help5
            [<opt-pos>]          help2
            --req-named[=Any]    help4
            --named[=Any]        help3
        §usage-msg
    'unchanged usage for MAIN(Int $pos, ‘literal’ $opt-pos?, :$req-named!, :$named)'
);


