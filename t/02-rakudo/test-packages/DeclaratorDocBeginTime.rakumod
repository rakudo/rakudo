unit module DeclaratorDocBeginTime;

my $held;
multi trait_mod:<is>(Routine:D $r, :$hold!) { $held = $r.WHY }

#| lead
our sub documented() is hold { }; #= trail

our sub held() { $held }

my role R {
    #| role attribute
    has $.x;
}
our class C does R { }
