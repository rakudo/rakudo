use Test;

plan 1;

subtest 'Supply.interval with negative value warns' => {
    plan 2;
    CONTROL { when CX::Warn {
        like .message, /'Minimum timer resolution is 1ms'/, 'useful warning';
        .resume;
    }}
    react whenever Supply.interval(-100) {
        pass "intervaled code ran";
        done;
    }
}

# vim: ft=perl6 expandtab sw=4
