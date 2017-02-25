use Test;

# RT #129763 https://rt.perl.org/Ticket/Display.html?id=129763
throws-like '1++', X::Multi::NoMatch,
    message => /'but require mutable arguments'/,
'point out matching `is rw` candidates when passing non-rw';

subtest 'curly quotes are not called smart quotes' => {
    my @quotes = ｢‘｣, ｢‚｣, ｢’｣, ｢“｣, ｢„｣, ｢”｣;
    plan +@quotes;

    for @quotes -> $q {
        throws-like $q, Exception,
            :message{ not .contains('smart') and .contains('curly') },
        "$q (U+$q.ord.base(16)) quote is called curly, not smart";
    }
}

# RT #130712 https://rt.perl.org/Public/Bug/Display.html?id=130712
throws-like 'sub infix:<$>() return Nil {}',
    X::AdHoc,
    :message{ .contains("'returns'") },
    'typing "return" instead of "returns" gives a fixing hint';

done-testing;
