my %*SUB-MAIN-OPTS ;# = :named-anywhere;

#| Manage documentation generation for Raku projects
proto MAIN(|) {*}

#| Universal options
multi MAIN (
    Bool :V(:$version), #= Display the current version and exit
) {}

#| Download and untar default assets to generate the site
multi MAIN ("setup", Bool :o(:$override) = False) {}

subset OptionalStr where Str | False;
#| Start the documentation generation with the specified options
multi MAIN (
    "start",     #= What goes here? A: currently unprinted
            $foo, $bar, $baz?,
            Str  :$topdir         = "doc",          #= Directory where the pod collection is stored
            Str  :$conf           = "doc-new.json", #= Configuration file
            Bool :v(:$verbose) = False,          #= Prints progress information
            Str :$qux!,
            Bool :$aaa,
            Bool :$bbb!,
            Bool :$ccc,
            Str :$AAA,
            OptionalStr :$BBB,
            Str :$CCC,
            OptionalStr :$DDD,
            
           ) {}

#| Check which pod files have changed and regenerate its HTML files.
multi MAIN ("update",
            Str  :$topdir    = "doc",               #= Directory where the pod collection is stored
            Str  :$conf      = "doc-updates.json",  #= Configuration file for updates
            Bool :$backup    = False                #= Back up existing files before performing the update
           ) {}

DOC CHECK {
    use Test;
    subtest 'Simple MAIN', {
        enum limit <foo bar baz>;
        class Class {}
        role Role {}
        role pRole[::T] {}
        for sub ($one-arg) {},
            sub (Str $str-arg) {},
            sub (Int $int-arg) {},
            sub ($first, $second) {},
            sub ($first, $optional-second?) {},
            sub (:$named) {},
            sub (Bool :$flag) {},
            sub (Bool :$required-flag!) {},
            sub (Str :$str-opt) {},
            sub (Str :$req-str-opt!) {},
            sub (:$with-where where  *.chars > 5) {},
            sub ('literal' :$named) {},
            sub ($foo where Int) {},
            sub ($AAA, Int) {},
            sub ($BBB, 'bbb') {},
            sub (@CCC[Int]) {},
            sub (Class $DDD) {},
            sub (Role $EEE) {},
            sub (pRole[Str] $EEE) {},
            sub ($ ($FFF, $FF)) {},
            sub (Bool  :@GGG) {},
            sub (:@HHH) {},            
            sub (@JJJ) {},                     # TODO: Is this right?
            sub (*@LLL) {},                     # TODO: Is this right?
            sub (*%MMM) {},           

            sub (\aaa) {},
            sub ($*bbb) {},
            sub (:$*ccc) {},
            sub (&ddd) {},
            sub ($) {},
            sub (:$foo where 'bar') {},
            sub (:aaa(:bbb(:$ccc))) {},  # TODO: aliases generally
            sub (:$v) {}                 # TODO: Short arguments generally
            #sub (limit :$foo) {} 
        -> &MAIN {
            new-generate-usage(&MAIN).&is: default-generate-usage(&MAIN),
            &MAIN.signature.params.map: *.gist;
            diag(new-generate-usage(&MAIN));
            #diag(default-generate-usage(&MAIN));
        } 
        
    }
    
}
## setup
use nqp;
my %sub-main-opts = %*SUB-MAIN-OPTS;
my &GENERATE-USAGE = &default-generate-usage;
#my &GENERATE-USAGE = &new-generate-usage;

    # &main.candidates.classify: 
    #     {.signature.params[0].name eq '' ?? 'subcommands' !! 'main'},
    #     :as{ given .signature.params -> $params ($first, *@rest) {
    #                    $first.name eq ''  ?? ($first.gist.substr(1, *-1) => @rest) !! |$params}},
    #     :into(my %commands);

## Patch for core.c/Main.pm6

class Script {
    class Command {...}
    class Positional {...}
    class Option {...}
    
    has %.subcmds;
    has Command $.main-command;

    method add(List $params) {
        my $cmd = Command.new();
        for @($params) {
            when !.named {
                .optional ?? $cmd.positionals<optional>.push: Positional($_)
                          !! $cmd.positionals<required>.push: Positional($_) }
            for .named_names -> str $name {
                .optional ?? $cmd.options<optional>.push: Option($_)
                          !! $cmd.options<required>.push: Option($_) }
        }
        $!main-command = $cmd;
    }

    method new { self.bless } # All options start empty

    class Positional {
        has Str  $.name;
        has Bool $.literal;
        has Bool $.accepts-multiple; # TODO: NYI
        
        multi method COERCE(Parameter $p) {
            my $name = $p.usage-name || $p.type.^name;
            my $literal = False;
            for $p.constraint_list {
                nqp::if(!nqp::istype($_, Callable),
                        (nqp::stmts ($name = .gist),
                         ($literal = True)))}

            if $p.type !=== Bool && Metamodel::EnumHOW.ACCEPTS($p.type.HOW) {
                 my $options = $p.type.^enum_values.keys.join('|');
                 $name = $options.chars > 50 ?? substr($options,0,50) ~'...' !! "$options"
            }

            self.new(:$name, :$literal)
        }
    } 
    
    class Option {
        has Str $.name;
        has Str $.arg;
        has Str $.arg-kind;
        has Bool $.accepts-multiple;

        multi method COERCE(Parameter $p) {
            my int $accepts-true = $p.type.ACCEPTS: True;
            for $p.constraint_list { $accepts-true++ if .ACCEPTS: True}
                        my int $other-accepts-true = $p.type.ACCEPTS: True;
                        for $p.constraint_list { $other-accepts-true++ if .ACCEPTS: True}
            my $name = $p.usage-name || $p.type.^name;
            my $accepts-multiple = False;

            my $arg  = $p.type.^name;
            if $arg.starts-with('Positional') {
                $accepts-multiple = True;
                $arg eq 'Positional' ?? ($arg = 'Any') !! $arg.=substr(11, *-1)}
             
            for $p.constraint_list {
                nqp::if(nqp::istype($_, Callable), ($arg ~= ' where { ... }'), ($arg = .gist))
            };
            if $p.type !=== Bool && Metamodel::EnumHOW.ACCEPTS($p.type.HOW) {
                            my $options = $p.type.^enum_values.keys.join('|');
                            $arg = $options.chars > 50
                              ?? substr($options,0,50) ~ '...'
                              !! "$options"
                        }

            when $arg.starts-with('Bool') { self.new(:$name:$arg :arg-kind<flag>:$accepts-multiple) }
            when ?$accepts-true           { self.new(:$name:$arg :arg-kind<optional>:$accepts-multiple)}
            default                       { self.new(:$name:$arg :arg-kind<required>:$accepts-multiple)}
        }
    }
    class Command {
        has %.options    is Map;
        has %.positionals is Map;

        method new { self.bless: :positionals(required => @[], optional => @[])
                                 :options(    required => @[], optional => @[]) }
    } 
    
}

sub new-generate-usage(&main, |capture) {
    
    my $opts = Script.new();
    for &main.candidates { given .signature.params -> @params ($first, *@) {
        $first.name || !$first.constraint_list ?? $opts.add(@params) !! (note "SUBCOMMAND??")}
    }


    my sub strip_path_prefix(str $name) {
        when nqp::iseq_s($name, '-e') { "-e '...'" }
        my (str $vol, str $dir, str $base) = $*SPEC.splitpath($name);
        for $*SPEC.path() -> str $path-dir {
            # Because checking file existance is slow, only do it if we _could_ match
            if nqp::eqat($dir, $path-dir, 0) {
                my str $file = $*SPEC.catpath($vol, $path-dir, $base).IO;
                return $base if $file.x && $file.f;
            }
        }
        $name;
    }
    my str $prog-name = strip_path_prefix($*PROGRAM-NAME);
    #dd $opts;
    my $main-cmd = $opts.main-command; # TODO fixme
    #dd :$main-cmd;
    my $res = ("Usage:\n  "
     ~ ($prog-name, 
        |$main-cmd.positionals<required>.map(-> $pos { $pos.literal ?? $pos.name !! "<{$pos.name}>" }),
        |$main-cmd.positionals<optional>.map(-> $pos { $pos.literal ?? $pos.name !! "[<{$pos.name}>]" }),
        # TODO: allow multiple
                    |$main-cmd.options<optional>.map(-> $opt {
                         "[--{$opt.name}"  ~("[={$opt.arg}]" if $opt.arg-kind eq 'optional')
                                           ~("=<{$opt.arg}>" if $opt.arg-kind eq 'required')
                                           ~']' ~(if $opt.accepts-multiple { '...'})}),
                    |$main-cmd.options<required>.map(-> $opt { "--{$opt.name}"})
                      # TODO: Can there be required options with optional args?
       ).join: ' '
                                                                                                   
              );
   # note now - ENTER now;
    $res;
    
}

# Generate $*USAGE string (default usage info for MAIN)
sub default-generate-usage(&main, |capture) { #* TODO remove &main (added for testing)
    my $no-named-after = nqp::isfalse(%sub-main-opts<named-anywhere>);

    my @help-msgs;
    my Pair @arg-help;

    my sub strip_path_prefix($name) {
        my $SPEC := $*SPEC;
        my ($vol, $dir, $base) = $SPEC.splitpath($name);
        $dir = $SPEC.canonpath($dir);
        for $SPEC.path() -> $elem {
            my $file = $SPEC.catpath($vol, $elem, $base).IO;
            if $file.x && $file.f {
                return $base if $SPEC.canonpath($elem) eq $dir;
                # Shadowed command found in earlier PATH element
                return $name;
            }
        }
        # Not in PATH
        $name;
    }

    my $prog-name = %*ENV<PERL6_PROGRAM_NAME> || $*PROGRAM-NAME;
    $prog-name = $prog-name eq '-e'
      ?? "-e '...'"
      !! strip_path_prefix($prog-name);

    # return the Cool constant if the post_constraints of a Parameter is
    # a single Cool constant, else Nil
    sub cool_constant(Parameter:D $p) {
        nqp::not_i(
          nqp::isnull(
            (my \post_constraints :=
              nqp::getattr($p,Parameter,'@!post_constraints'))
          )
        ) && nqp::elems(post_constraints) == 1
          && nqp::istype((my \value := nqp::atpos(post_constraints,0)),Cool)
          ?? value
          !! Nil
    }

    # Select candidates for which to create USAGE string
    sub usage-candidates($capture) {
        my @candidates = &main.candidates.grep: { !.?is-hidden-from-USAGE }
        if $capture.list -> @positionals {
            my $first := @positionals[0];
            if @candidates.grep: -> $sub {
                if $sub.signature.params[0] -> $param {
                    if cool_constant($param) -> $literal {
                        $literal.ACCEPTS($first)
                    }
                }
            } -> @candos {
                return @candos;
            }
        }
        @candidates
    }

    for usage-candidates(capture) -> $sub {
        my @required-named;
        my @optional-named;
        my @positional;
        my $docs;

        for $sub.signature.params -> $param {
            my $argument;

            my int $literals-as-constraint = 0;
            my int $total-constraints = 0;
            my $constraints = ~unique $param.constraint_list.map: {
                ++$total-constraints;
                nqp::if(
                  nqp::istype($_, Callable),
                  'where { ... }',
                  nqp::stmts(
                    (my \g = .gist),
                    nqp::if(
                      nqp::isconcrete($_),
                      nqp::stmts(
                        ++$literals-as-constraint,
                        g), # we constrained by some literal; gist as is
                      nqp::substr(g, 1, nqp::chars(g)-2))))
                      # ^ remove ( ) parens around name in the gist
            }
            $_ eq 'where { ... }' and $_ = "$param.type.^name() $_"
                with $constraints;

            if $param.named {
                if $param.slurpy {
                    if $param.name { # ignore anon *%
                        $argument  = "--<$param.usage-name()>=...";
                        @optional-named.push("[$argument]");
                    }
                }
                else {
                    my @names = $param.named_names.reverse;
                    $argument = @names.map({
                        (.chars == 1 ?? '-' !! '--') ~ $_
                    }).join('|');

                    my $type := $param.type;
                    if $type ~~ Positional {
                        $argument ~= "=<{ $constraints || "Any" }> ..."

                    }
                    elsif $type !=== Bool {

                        my int $accepts-true = $param.type.ACCEPTS: True;
                        for $param.constraint_list { $accepts-true++ if .ACCEPTS: True}
                        $argument ~= ($accepts-true ?? "[={$constraints || $type.^name}]"
                                                    !! "=<{$constraints || $type.^name}>");
                        if Metamodel::EnumHOW.ACCEPTS($type.HOW) {
                            my $options = $type.^enum_values.keys.sort.Str;
                            $argument ~= $options.chars > 50
                              ?? ' (' ~ substr($options,0,50) ~ '...'
                              !! " ($options)"
                        }
                    }
                    if $param.optional {
                        @optional-named.push("[$argument]");
                    }
                    else {
                        @required-named.push($argument);
                    }
                }
            }
            else {
                $argument = $param.name
                    ?? "<$param.usage-name()>"
                    !! $constraints
                        ?? ($literals-as-constraint == $total-constraints)
                            ?? $constraints
                            !! "<{$constraints}>"
                        !! "<$param.type.^name()>";

                $argument  = "[$argument ...]" if $param.slurpy;
                $argument  = "[$argument]"     if $param.optional;
                if $total-constraints
                && $literals-as-constraint == $total-constraints {
                    $argument .= trans(["'"] => [q|'"'"'|]) # "hlfix
                        if $argument.contains("'");
                    $argument  = "'$argument'"
                        if $argument.contains(' ' | '"');
                }
                @positional.push($argument);
            }
            @arg-help.push($argument => $param.WHY.contents) if $param.WHY and (@arg-help.grep:{ .key eq $argument}) == Empty;  # Use first defined
        }
        if $sub.WHY {
            $docs = '-- ' ~ $sub.WHY.contents
        }
        my $msg = $no-named-after
          ?? join(' ', $prog-name, @required-named, @optional-named, @positional, ($docs if $docs))
          !! join(' ', $prog-name, @positional, @required-named, @optional-named, ($docs if $docs));
        @help-msgs.push($msg);
    }

    if @arg-help {
        @help-msgs.push('');
        my $offset = max(@arg-help.map: { .key.chars }) + 4;
        @help-msgs.append(@arg-help.map: { '  ' ~ .key ~ ' ' x ($offset - .key.chars) ~ .value });
    }

    #note now - ENTER now;
    my $res=@help-msgs
      ?? "Usage:\n" ~ @help-msgs.map('  ' ~ *).join("\n")
      !! "No usage information could be determined";
    #note now - ENTER now;
    $res;
}
