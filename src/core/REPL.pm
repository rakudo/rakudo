use nqp;

my sub sorted-set-insert(@values, $value) {
    my $low        = 0;
    my $high       = @values.end;
    my $insert_pos = 0;

    while $low <= $high {
        my $middle = floor($low + ($high - $low) / 2);

        my $middle_elem = @values[$middle];

        if $middle == @values.end {
            if $value eq $middle_elem {
                return;
            } elsif $value lt $middle_elem {
                $high = $middle - 1;
            } else {
                $insert_pos = +@values;
                last;
            }
        } else {
            my $middle_plus_one_elem = @values[$middle + 1];

            if $value eq $middle_elem || $value eq $middle_plus_one_elem {
                return;
            } elsif $value lt $middle_elem {
                $high = $middle - 1;
            } elsif $value gt $middle_plus_one_elem {
                $low = $middle + 1;
            } else {
                $insert_pos = $middle + 1;
                last;
            }
        }
    }

    splice(@values, $insert_pos, 0, $value);
}

my role ReadlineBehavior[$WHO] {
    my &readline    = $WHO<&readline>;
    my &add_history = $WHO<&add_history>;

    method readline(Mu \SELF, Mu \super, Mu \stdin, Mu \stdout, Mu \prompt) {
        my $line = readline(prompt);

        if $line.defined {
            add_history($line);
        }

        $line // nqp::null_s()
    }
}

my role LinenoiseBehavior[$WHO] {
    my &linenoise                      = $WHO<&linenoise>;
    my &linenoiseHistoryAdd            = $WHO<&linenoiseHistoryAdd>;
    my &linenoiseSetCompletionCallback = $WHO<&linenoiseSetCompletionCallback>;
    my &linenoiseAddCompletion         = $WHO<&linenoiseAddCompletion>;

    method completions-for-line(Str $line, int $cursor-index) { ... }

    method init-line-editor {
        linenoiseSetCompletionCallback(sub ($line, $c) {
            eager self.completions-for-line($line, $line.chars).map(&linenoiseAddCompletion.assuming($c));
        });
    }

    method readline(Mu \SELF, Mu \super, Mu \stdin, Mu \stdout, Mu \prompt) {
        self.update-completions;
        my $line = linenoise(prompt);

        if $line.defined {
            linenoiseHistoryAdd($line);
        }

        $line // nqp::null_s()
    }
}

my role FallbackBehavior {
    method readline(Mu \SELF, Mu \super, Mu \stdin, Mu \stdout, Mu \prompt) {
        super.(SELF, stdin, stdout, prompt);
    }
}

my role Completions {
    has @!completions = CORE::.keys.flatmap({
        /^ "&"? $<word>=[\w* <.lower> \w*] $/ ?? ~$<word> !! []
    }).sort;

    method update-completions {
        my $context := self.compiler.context;

        return unless $context;

        my $pad := nqp::ctxlexpad($context);
        my $it := nqp::iterator($pad);

        while $it {
            my $e := nqp::shift($it);
            my $k := nqp::iterkey_s($e);
            my $m = $k ~~ /^ "&"? $<word>=[\w* <.lower> \w*] $/;
            if $m {
                my $word = ~$m<word>;
                sorted-set-insert(@!completions, $word);
            }
        }

        my $PACKAGE = self.compiler.eval('$?PACKAGE', :outer_ctx($context));

        for $PACKAGE.WHO.keys -> $k {
            sorted-set-insert(@!completions, $k);
        }
    }

    method extract-last-word(Str $line) {
        my $m = $line ~~ /^ $<prefix>=[.*?] <|w>$<last_word>=[\w*]$/;

        return ( ~$m<prefix>, ~$m<last_word> );
    }

    method completions-for-line(Str $line, int $cursor-index) {
        return @!completions unless $line;

        # ignore $cursor-index until we have a backend that provides it
        my ( $prefix, $word-at-cursor ) = self.extract-last-word($line);

        # XXX this could be more efficient if we had a smarter starting index
        gather for @!completions -> $word {
            if $word ~~ /^ "$word-at-cursor" / {
                take $prefix ~ $word;
            }
        }
    }
}

# *** WARNING ***
#
# If you want to add new methods as hooks into Perl6::Compiler, you'll need to
# add support for them to Perl6::Compiler itself.  See the readline and eval
# methods both in this file and in Perl6::Compiler for guidance on how to do
# that
class REPL {
    also does Completions;

    has Mu $.compiler;
    has Bool $!multi-line-enabled;

    sub mixin-line-editor($self is copy) {
        my Bool $problem = False;
        my $loaded-readline = try {
            CATCH {
                when (X::CompUnit::UnsatisfiedDependency & { .specification ~~ /Readline/ }) {
                    # ignore it
                }
                default {
                    say "I ran into a problem trying to set up Readline: $_";
                    say 'Falling back to Linenoise (if present)';

                    $problem = True;
                }
            }
            my $readline = do require Readline;
            my $rl-self = $self but ReadlineBehavior[$readline.WHO<EXPORT>.WHO<ALL>.WHO];
            $rl-self.?init-line-editor();
            $self = $rl-self;
            True
        };

        return $self if $loaded-readline;

        my $loaded-linenoise = try {
            CATCH {
                when X::CompUnit::UnsatisfiedDependency & { .specification ~~ /Linenoise/ } {
                    # ignore it
                }
                default {
                    say "I ran into a problem while trying to set up Linenoise: $_";
                    $problem = True;
                }
            }
            my $linenoise = do require Linenoise;
            my $ln-self = $self but LinenoiseBehavior[$linenoise.WHO];
            $ln-self.?init-line-editor();
            $self = $ln-self;
            True
        }

        return $self if $loaded-linenoise;

        if $problem {
            say 'Continuing without tab completions or line editor';
            say 'You may want to consider using rlwrap for simple line editor functionality';
        } else {
            say 'You may want to `panda install Readline` or `panda install Linenoise` or use rlwrap for a line editor';
        }
        say '';

        $self but FallbackBehavior
    }

    method new(Mu \compiler, Mu \adverbs) {
        my $multi-line-enabled = !%*ENV<RAKUDO_DISABLE_MULTILINE>;
        my $self = self.bless();
        $self.init(compiler, $multi-line-enabled);
        $self = mixin-line-editor($self);

        $self
    }

    method init(Mu \compiler, $multi-line-enabled) {
        $!compiler = compiler;
        $!multi-line-enabled = $multi-line-enabled;
    }

    method teardown {
    }

    method eval(Mu \SELF, Mu \super, Mu \code, Mu \args, Mu \adverbs) {
        try {
            my &needs_more_input = adverbs<needs_more_input>;
            CATCH {
                when X::Syntax::Missing {
                    if $!multi-line-enabled && .pos == code.chars {
                        return needs_more_input();
                    }
                    .throw;
                }

                when X::Comp::FailGoal {
                    if $!multi-line-enabled && .pos == code.chars {
                        return needs_more_input();
                    }
                    .throw;
                }
            }

            super.(SELF, code, |@(args), |%(adverbs))
        }
    }
}
