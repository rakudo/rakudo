use Test;

# RT #129763 https://rt.perl.org/Ticket/Display.html?id=129763
throws-like '1++', X::Multi::NoMatch,
    message => /'but require mutable arguments'/,
'point out matching `is rw` candidates when passing non-rw';

done-testing;
