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
        $!handled =1 if pir::repr_defined__IP(self);
        Bool::False;
    }
    multi method Bool(Failure:D:) { $!handled = 1; Bool::False; }

    method Int(Failure:D:)        { $!handled ?? 0   !! $!exception.throw; }
    method Num(Failure:D:)        { $!handled ?? 0e0 !! $!exception.throw; }
    method Numeric(Failure:D:)    { $!handled ?? 0e0 !! $!exception.throw; }
    multi method Str(Failure:D:)  { $!handled ?? ''  !! $!exception.throw; }
    multi method gist(Failure:D:) { $!handled ?? $.perl !! $!exception.throw; }

    Failure.^add_fallback(
        -> $, $ { True },
        method ($name) {
            $!exception.throw;
        }
    );
}


my &fail := -> *@msg {
    my $value = @msg == 1 ?? @msg[0] !! @msg.join('');
    die $value if $*FATAL;
    try die $value;
    my $fail := Failure.new($!);
    my Mu $return := pir::find_caller_lex__Ps('RETURN');
    $return($fail) unless nqp::isnull($return);
    $fail
}


