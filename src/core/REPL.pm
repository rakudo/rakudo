# XXX comment about method signatures
# XXX prevent other methods from getting added
class REPL is export { # XXX no need for is export later
    method interactive(Mu \adverbs) {
    }

    method readline(Mu \SELF, Mu \super, Mu \stdin, Mu \stdout, Mu \prompt) {
        super.(SELF, stdin, stdout, prompt);
    }

    method eval(Mu \SELF, Mu \super, Mu \code, Mu \args, Mu \adverbs) {
        super.(SELF, code, |@(args), |%(adverbs));
    }
}
