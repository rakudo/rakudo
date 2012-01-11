# XXX Wants to be a macro when we have them.
sub WHAT(\$x) {
    $x.WHAT
}

proto sub infix:<...>(|$) { * }
multi sub infix:<...>($a, $b) { SEQUENCE($a, $b) }

proto sub infix:<...^>(|$) { * }
multi sub infix:<...^>($a, $b) { SEQUENCE($a, $b, :exclude_end(1)) }

sub undefine(Mu \$x) {
    my $undefined;
    $x = $undefined;
}

sub infix:<ff>($a as Bool, $b as Bool) {
    my $pos := nqp::p6box_s(nqp::callerid());
    state %ffv;
    if %ffv{$pos} {
        %ffv{$pos} = False if $b;
        True;
    }
    elsif $a {
        %ffv{$pos} = $a
    }
    else {
        False
    }
}

sub infix:<ff^>($a as Bool, $b as Bool) {
    my $pos := nqp::p6box_s(nqp::callerid());
    state %ffv;
    if %ffv{$pos} {
        $b ?? (%ffv{$pos} = False) !! True
    }
    elsif $a {
        %ffv{$pos} = $a
    }
    else {
        False
    }
}

sub infix:<^ff>($a as Bool, $b as Bool) {
    my $pos := nqp::p6box_s(nqp::callerid());
    state %ffv;
    if %ffv{$pos} {
        %ffv{$pos} = False if $b;
        True
    }
    else {
        %ffv{$pos} = True if $a;
        False
    }
}

sub infix:<^ff^>($a as Bool, $b as Bool) {
    my $pos := nqp::p6box_s(nqp::callerid());
    state %ffv;
    if %ffv{$pos} {
        $b ?? (%ffv{$pos} = False) !! True
    }
    else {
        %ffv{$pos} = True if $a;
        False
    }
}

# not sure where this should go
# this implements the ::() indirect lookup
sub INDIRECT_NAME_LOOKUP(*@chunks) is rw {
    # note that each part of @chunks itself can
    # contain double colons. That's why joining and
    # re-splitting is necessary
    my Str $name = @chunks.join('::');
    my @parts    = $name.split('::');
    my $first    = @parts.shift;
    if @parts && '$@%&'.index($first.substr(0, 1)).defined {
        # move sigil from first to last chunk, because
        # $Foo::Bar::baz is actually stored as Foo::Bar::$baz
        my $last_idx      = @parts.end;
        @parts[$last_idx] = $first.substr(0, 1) ~ @parts[$last_idx]; 
        $first            = $first.substr(1);
        if $first eq '' {
            $first = @parts.shift;
            $name = @chunks.join('::');
        }
    }
    my Mu $thing := pir::find_caller_lex__Ps(
        nqp::unbox_s($first)
    );
    fail("Symbol '$name' not found") if nqp::isnull($thing);
    for @parts {
        fail("Symbol '$name not found") unless $thing.WHO.exists($_);
        $thing := $thing.WHO{$_};
    }
    $thing;
}

