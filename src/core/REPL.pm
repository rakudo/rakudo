use nqp;

my role ReadlineBehavior[$WHO] {
    method readline(Mu \SELF, Mu \super, Mu \stdin, Mu \stdout, Mu \prompt) {
        say 'readline';
        nqp::null_s();
    }
}

my role LinenoiseBehavior[$WHO] {
    my &linenoise           = $WHO<&linenoise>;
    my &linenoiseHistoryAdd = $WHO<&linenoiseHistoryAdd>;

    method readline(Mu \SELF, Mu \super, Mu \stdin, Mu \stdout, Mu \prompt) {
        linenoise(prompt) // nqp::null_s()
    }
}

my role FallbackBehavior {
    method readline(Mu \SELF, Mu \super, Mu \stdin, Mu \stdout, Mu \prompt) {
        super.(SELF, stdin, stdout, prompt);
    }
}

# XXX print warning when we can't load this file
# XXX comment about method signatures
# XXX prevent other methods from getting added
class REPL is export { # XXX no need for is export later
    # XXX print fallback messages
    method !load-line-editor() {
        my $readline = try require Readline;

        if $readline.HOW ~~ Metamodel::ModuleHOW {
            self does ReadlineBehavior[$readline.WHO];
            return;
        }

        my $linenoise = try require Linenoise;

        if $linenoise.HOW ~~ Metamodel::ModuleHOW {
            self does LinenoiseBehavior[$linenoise.WHO];
            return;
        }

        self does FallbackBehavior;
    }

    method interactive(Mu \adverbs) {
        self!load-line-editor();
    }

    method eval(Mu \SELF, Mu \super, Mu \code, Mu \args, Mu \adverbs) {
        super.(SELF, code, |@(args), |%(adverbs));
    }
}
