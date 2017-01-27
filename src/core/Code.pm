my class Code does Callable { # declared in BOOTSTRAP
    # class Code is Any
    #     has Code $!do;              # Low level code object
    #     has Signature $!signature;  # Signature object
    #     has @!compstuff;            # Place for the compiler to hang stuff

    multi method ACCEPTS(Code:D $self: Mu $topic) {
        $self.count ?? $self($topic) !! $self()
    }

    method arity(Code:D:) { nqp::getattr_i($!signature,Signature,'$!arity') }

    method count(Code:D:) { nqp::getattr($!signature,Signature,'$!count') }

    method signature(Code:D:) { $!signature }

    proto method prec(|) { * }
    multi method prec() { my % }
    multi method prec(Str:D $) { '' }

    multi method Str(Code:D:) {
        warn( self.WHAT.perl ~ " object coerced to string (please use .gist or .perl to do that)"); self.name
    }

    method outer(Code:D:) {
        nqp::ifnull(nqp::getcodeobj(nqp::p6staticouter($!do)), Mu)
    }

    # returns an identifier for this code object
    # that is the same even for cloned closures
    method static_id(Code:D:) {
        nqp::p6box_i(nqp::where(nqp::getstaticcode($!do)));
    }

    multi method new(Code:) { X::Cannot::New.new(class => self).throw }

    method file(Code:D:) {
        nqp::getcodelocation($!do)<file>;
    }

    method line(Code:D:) {
        nqp::getcodelocation($!do)<line>;
    }

    multi method perl(Code:D:) { '{ ... }' }
}

# vim: ft=perl6 expandtab sw=4
