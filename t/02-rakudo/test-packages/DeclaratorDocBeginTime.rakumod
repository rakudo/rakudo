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

#| subset doc
our subset Positive of Int where ({ $_ > 0 });

#| enum doc
our enum Letters (do { my sub f() { <a b> }; f() });
