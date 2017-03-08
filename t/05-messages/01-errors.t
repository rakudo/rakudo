use Test;

# RT #129763
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

# RT #130712
throws-like 'sub infix:<$>() return Nil {}',
    X::AdHoc,
    :message{ .contains("'returns'") },
    'typing "return" instead of "returns" gives a fixing hint';

# RT #130630
throws-like ｢'4x'.Rat.nude｣, X::Str::Numeric,
    :message{ not .contains("Metamodel.nqp") },
    '.Rat.nude on non-numeric string does not reference guts in error';

# RT #130509
throws-like ｢…｣, X::StubCode,
    :message{ not .contains('CORE.setting') },
    'stub code does not reference guts when executed';

# RT #130913
subtest 'chr with large codepoints throws useful error' => {
    my @tests = 'chr 2⁶³-1',   '(2⁶³-1).chr', 'chr 2⁶³',
                '2⁶³.chr',     'chr 2¹⁰⁰',    '(2¹⁰⁰).chr';
    plan +@tests;
    for @tests {
        throws-like $^code, Exception,
            :message{ not .contains('negative') and .contains('codepoint') },
        "$code.perl()";
    }
}

done-testing;
