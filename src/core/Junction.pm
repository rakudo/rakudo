my class Junction { # declared in BOOTSTRAP
    # class Junction is Mu {
    #     has Mu $!storage;             # elements of Junction
    #     has str $!type;                # type of Junction

    proto method new(|) { * }
    multi method new(@values, Str :$type!) {
        my $junc := nqp::create(Junction);
        nqp::bindattr($junc, Junction, '$!storage', @values.eager);
        nqp::bindattr($junc, Junction, '$!type', $type);
        $junc
    }
    multi method new(*@values, Str :$type!) {
        my $junc := nqp::create(Junction);
        nqp::bindattr($junc, Junction, '$!storage', @values.eager);
        nqp::bindattr($junc, Junction, '$!type', $type);
        $junc
    }

    multi method Bool(Junction:D:) {
        SEQ($!storage.map({return True if $_}).gimme(*); return False)
            if nqp::iseq_s($!type, 'any');
        SEQ($!storage.map({return False unless $_}).gimme(*); return True)
            if nqp::iseq_s($!type, 'all');
        SEQ($!storage.map({return False if $_}).gimme(*); return True)
            if nqp::iseq_s($!type, 'none');
        # 'one' junction
        my $count = 0;
        $!storage.map({ $count++ if $_; return False if $count > 1 }).gimme(*);
        $count == 1;
    }

    multi method Str(Junction:D:) {
        self.perl
    }

    multi method ACCEPTS(Junction:D: Mu \topic) {
        SEQ($!storage.map({return True if $_.ACCEPTS(topic)}).gimme(*); return False)
            if nqp::iseq_s($!type, 'any');
        SEQ($!storage.map({return False unless $_.ACCEPTS(topic)}).gimme(*); return True)
            if nqp::iseq_s($!type, 'all');
        SEQ($!storage.map({return False if $_.ACCEPTS(topic)}).gimme(*); return True)
            if nqp::iseq_s($!type, 'none');
        # 'one' junction
        my $count = 0;
        $!storage.map({ $count++ if $_.ACCEPTS(topic); return False if $count > 1 }).gimme(*);
        $count == 1;
    }

    multi method gist(Junction:D:) {
        $!type ~ '(' ~ $!storage.map({$_.gist}).join(', ') ~ ')'
    }

    multi method perl(Junction:D:) {
        $!type ~ '(' ~ $!storage.map({$_.perl}).join(', ') ~ ')'
    }

    method CALL-ME(|c) {
        self.AUTOTHREAD(
            -> $obj, |c { $obj(|c) },
            self, |c);
    }

    method sink(Junction:D:) {
        .?sink for $!storage.list;
        Nil;
    }

    method AUTOTHREAD(&call, |args) {
        my Mu $pos_rpa := nqp::getattr(nqp::decont(args), Capture, '$!list');
        sub thread_junction(int $i) {
            my Junction $arg := nqp::atpos($pos_rpa, $i);
            my Str $type := nqp::getattr(nqp::decont($arg), Junction, '$!type');
            my @states := nqp::getattr(nqp::decont($arg), Junction, '$!storage');

            my Mu $res := nqp::list();
            for @states -> \st {
                # Next line is Officially Naughty, since captures are meant to be
                # immutable. But hey, it's our capture to be naughty with...
                nqp::bindpos($pos_rpa, $i, st);
                nqp::push($res, call(|args));
                Nil;
            }
            my $res_junc := nqp::clone(nqp::decont($arg));
            nqp::bindattr($res_junc, Junction, '$!storage', nqp::p6parcel($res, Nil));
            return $res_junc;
        }

        # Look for a junctional arg in the positionals.

        # we have to autothread the first all or none junction before
        # doing any one or any junctions.
        my int $first_one_any = -1;
        loop (my int $i = 0; $i < nqp::elems($pos_rpa); $i = $i + 1) {
            # Junctional positional argument?
            my Mu $arg := nqp::atpos($pos_rpa, $i);
            if nqp::istype($arg, Junction) {
                my Str $type := nqp::getattr(nqp::decont($arg), Junction, '$!type');
                if nqp::iseq_s($type, 'any') || nqp::iseq_s($type, 'one') {
                    if $first_one_any == -1 {
                        # save it for later, first make sure we don't have all or none junctions later.
                        $first_one_any = $i;
                    }
                } else {
                    return thread_junction($i);
                }
            }
        }

        if $first_one_any >= 0 {
            return thread_junction($first_one_any);
        }

        # Otherwise, look for one in the nameds.
        for args.hash.kv -> $k, $v {
            if nqp::istype($v, Junction) {
                my Mu $nam_hash := nqp::getattr(nqp::decont(args), Capture, '$!hash');
                my @states := nqp::getattr(nqp::decont($v), Junction, '$!storage');
                my $type   := nqp::getattr(nqp::decont($v), Junction, '$!type');
                my Mu $res := nqp::list();
                for @states -> \st {
                    nqp::bindkey($nam_hash, $k, st);
                    nqp::push($res, call(|args));
                    Nil;
                }
                my $res_junc := nqp::clone(nqp::decont($v));
                nqp::bindattr($res_junc, Junction, '$!storage', nqp::p6parcel($res, Nil));
                return $res_junc;
            }
        }

        # If we get here, wasn't actually anything to autothread.
        call(|args);
    }
}

sub any(*@values) { Junction.new(@values, :type<any>); }
sub all(*@values) { Junction.new(@values, :type<all>); }
sub one(*@values) { Junction.new(@values, :type<one>); }
sub none(*@values) { Junction.new(@values, :type<none>); }

sub infix:<|>(**@values) { Junction.new(@values, :type<any>); }
sub infix:<&>(**@values) { Junction.new(@values, :type<all>); }
sub infix:<^>(**@values) { Junction.new(@values, :type<one>); }

sub AUTOTHREAD(|c) {
    Junction.AUTOTHREAD(|c)
}

sub AUTOTHREAD_METHOD($name, |c) {
    Junction.AUTOTHREAD(
        -> $obj, |c { $obj."$name"(|c) },
        |c);
}

nqp::p6setautothreader(&AUTOTHREAD);
Mu.HOW.setup_junction_fallback(Junction, &AUTOTHREAD_METHOD);

# vim: ft=perl6 expandtab sw=4
