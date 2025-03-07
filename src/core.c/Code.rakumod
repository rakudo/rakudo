my class Code { # declared in BOOTSTRAP
    # class Code is Any does Callable
    #     has Code $!do;              # Low level code object
    #     has Signature $!signature;  # Signature object
    #     has @!compstuff;            # Place for the compiler to hang stuff

    multi method ACCEPTS(Code:D $self: Mu $topic is raw) {
        nqp::getattr($!signature,Signature,'$!count')
          ?? $self($topic)
          !! $self()
    }

    method is-implementation-detail(--> False) { }
    method precedence(Code:D:  --> "") { }
    method associative(Code:D: --> "") { }
    method thunky(Code:D:      --> "") { }
    method iffy(Code:D:        --> 0 ) { }

    # runtime lookup because of bootstrap issues
    method reducer() { ::('&METAOP_REDUCE_LEFT') }

#?if moar
    method bytecode-size() { nqp::syscall('code-bytecode-size', $!do) }
#?endif

    proto method POSITIONS(|) {*} #  is implementation-detail

    method arity(Code:D:) { nqp::getattr_i($!signature,Signature,'$!arity') }

    method count(Code:D:) { nqp::getattr($!signature,Signature,'$!count') }

    method signature(Code:D:) { $!signature }
    method cando(Capture:D $c) { $!signature.ACCEPTS($c) ?? (self,) !! () }

    proto method prec(|) {*}
    multi method prec() { my % }
    multi method prec(Str:D $) { '' }

    multi method Str(Code:D:) {
        warn( self.WHAT.raku ~ " object coerced to string (please use .gist or .raku to do that)");
        self.name
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
}

# vim: expandtab shiftwidth=4
