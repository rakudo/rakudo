use lib <t/packages/>;
use Test;
use Test::Helpers;

plan 2;

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

if $*DISTRO.is-win {
    skip 'is-run code is too complex to run on Windows (RT#132258)';
}
else {
    subtest 'no useless-use warning on return when KEEP/UNDO phasers used' => {
        plan 3;
        is-run ｢
            if  1 { LEAVE 42.uc; Any }; if  1 { LEAVE 42.uc; 42  };
            for 1 { LEAVE 42.uc; Any }; for 1 { LEAVE 42.uc; 42  };
        ｣, :err{ 2 == .comb: 'Useless use' },
            'we get warnings with phasers that do not care about return value';

        is-run ｢
            if  1 { KEEP 42.uc; Any }; if  1 { KEEP 42.uc; 42  };
            for 1 { KEEP 42.uc; Any }; for 1 { KEEP 42.uc; 42  };
        ｣, :err(''), 'no warnings with KEEP phaser';

        is-run ｢
            if  1 { UNDO 42.uc; Any }; if  1 { UNDO 42.uc; 42  };
            for 1 { UNDO 42.uc; Any }; for 1 { UNDO 42.uc; 42  };
        ｣, :err(''), 'no warnings with UNDO phaser';
    }
}
# vim: ft=perl6 expandtab sw=4
