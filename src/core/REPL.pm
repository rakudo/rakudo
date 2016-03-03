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
    method readline(Mu \SELF, Mu \super, Mu \stdin, Mu \stdout, Mu \prompt) {
        say 'readline';
        nqp::null_s();
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
        linenoise(prompt) // nqp::null_s()
    }
}

my role FallbackBehavior {
    method readline(Mu \SELF, Mu \super, Mu \stdin, Mu \stdout, Mu \prompt) {
        super.(SELF, stdin, stdout, prompt);
    }
}

my role Completions {
    has @!completions;

    submethod BUILD {
        @!completions = CORE::.keys.flatmap({
            /^ "&"? $<word>=[\w* <.lower> \w*] $/ ?? ~$<word> !! []
        }).sort;
    }

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

# XXX print warning when we can't load this file
# XXX comment about method signatures
# XXX prevent other methods from getting added
class REPL is export { # XXX no need for is export later
    also does Completions;

    has Mu $.compiler;

    # XXX print fallback messages
    method !load-line-editor() {
        my $readline = try require Readline;

        if $readline.HOW ~~ Metamodel::ModuleHOW {
            self does ReadlineBehavior[$readline.WHO];
            self.?init-line-editor();
            return;
        }

        my $linenoise = try require Linenoise;

        if $linenoise.HOW ~~ Metamodel::ModuleHOW {
            self does LinenoiseBehavior[$linenoise.WHO];
            self.?init-line-editor();
            return;
        }

        self does FallbackBehavior;
    }

    method interactive(Mu \compiler, Mu \adverbs) {
        $!compiler = compiler;
        self!load-line-editor();
    }

    method eval(Mu \SELF, Mu \super, Mu \code, Mu \args, Mu \adverbs) {
        my \result = super.(SELF, code, |@(args), |%(adverbs));
        # XXX we don't get a context until our next readline?
        result
    }
}
