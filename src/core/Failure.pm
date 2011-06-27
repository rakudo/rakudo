my class Failure {
    has $!exception;
    has $!handled;

    method new($ex) {
        my $new = self.CREATE;
        $new.BUILD($ex);
    }

    method BUILD($ex) {
        $!exception = $ex;
        self;
    }

    method defined() { $!handled = 1; 0.Bool; }
    method Bool() { $!handled = 1; 0.Bool; }

    method Int() { $!handled ?? 0 !! $!exception.rethrow; }
    method Num() { $!handled ?? 0e0 !! $!exception.rethrow; }
    method Str() { $!handled ?? '' !! $!exception.rethrow; }
}


my &fail := -> *@msg {
    my $value = @msg.join('');
    my Mu $ex := Q:PIR {
                     # throw and immediately catch an exception, to capture
                     # the location at the point of the fail()
                     push_eh catch
                     $P0 = find_lex '$value'
                     $S0 = $P0
                     die $S0
                   catch:
                     .get_results (%r)
                     pop_eh
                 };
    my $fail = Failure.new(EXCEPTION($ex));
    my Mu $return := pir::find_caller_lex__Ps('RETURN');
    $return($fail) unless nqp::isnull($return);
    $fail
}


