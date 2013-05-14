$PROCESS::FATAL = False;

my class Failure {
    has $.exception;
    has $!handled;

    method new($ex) {
        my $new = self.CREATE;
        $new.BUILD($ex);
    }

    method BUILD($ex) {
        $!exception = $ex;
        self;
    }

    # TODO: should be Failure:D: multi just like method Bool,
    # but obscure problems prevent us from making Mu.defined
    # a multi. See http://irclog.perlgeek.de/perl6/2011-06-28#i_4016747
    method defined() {
        $!handled =1 if nqp::isconcrete(self);
        Bool::False;
    }
    multi method Bool(Failure:D:) { $!handled = 1; Bool::False; }

    method Int(Failure:D:)        { $!handled ?? 0   !! $!exception.throw; }
    method Num(Failure:D:)        { $!handled ?? 0e0 !! $!exception.throw; }
    method Numeric(Failure:D:)    { $!handled ?? 0e0 !! $!exception.throw; }
    multi method Str(Failure:D:)  { $!handled ?? ''  !! $!exception.throw; }
    multi method gist(Failure:D:) { $!handled ?? $.perl !! $!exception.throw; }

# XXX JVM backend doesn't like .^ yet.
#?if !jvm
    Failure.^add_fallback(
        -> $, $ { True },
        method ($name) {
            $!exception.throw;
        }
    );
#?endif
    method sink() { $!exception.throw unless $!handled }

    # class Any has a fallback method, so we need to redefine it here
    method postcircumfix:<{ }>(|c) { $!exception.throw }
}


my &fail := -> *@msg {
    my $value = @msg == 1 ?? @msg[0] !! @msg.join('');
    die $value if $*FATAL;
    try die $value;
    my $fail := Failure.new($!);
    my Mu $return := nqp::getlexcaller('RETURN');
    $return($fail) unless nqp::isnull($return);
    $fail
}


