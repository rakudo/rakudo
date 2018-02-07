use lib <t/packages>;
use Test::Helpers::QAST;
use Test;
plan 1;

subtest 'postfix-inc/dec on natives gets overwritten to prefix' => {
    plan 8;
    qast-is ｢my int $i; $i++｣, -> \v {
                qast-contains-op   v, 'add_i'
        and not qast-contains-call v, '&prefix:<++>'
        and not qast-contains-call v, '&postfix:<++>'
    }, 'int, void context ++';

    qast-is ｢my int $i; my int $y; $y = 1 + $i++｣, -> \v {
                qast-contains-op   v, 'add_i'
        and not qast-contains-call v, '&prefix:<++>'
        and not qast-contains-call v, '&postfix:<++>'
    }, 'int, non-void context ++';

    qast-is ｢my int $i; $i--｣, -> \v {
                qast-contains-op   v, 'sub_i'
        and not qast-contains-call v, '&prefix:<-->'
        and not qast-contains-call v, '&postfix:<-->'
    }, 'int, void context --';

    qast-is ｢my int $i; my int $y; $y = 1 + $i--｣, -> \v {
                qast-contains-op   v, 'sub_i'
        and not qast-contains-call v, '&prefix:<-->'
        and not qast-contains-call v, '&postfix:<-->'
    }, 'int, non-void context --';


    qast-is ｢my num $i = 2e0; $i++｣, -> \v {
                qast-contains-op   v, 'add_n'
        and not qast-contains-call v, '&prefix:<++>'
        and not qast-contains-call v, '&postfix:<++>'
    }, 'num, void context ++';

    qast-is ｢my num $i = 2e0; my num $y; $y = 1e0 + $i++｣, -> \v {
                qast-contains-op   v, 'add_n'
        and not qast-contains-call v, '&prefix:<++>'
        and not qast-contains-call v, '&postfix:<++>'
    }, 'num, non-void context ++';

    qast-is ｢my num $i = 2e0; $i--｣, -> \v {
                qast-contains-op   v, 'sub_n'
        and not qast-contains-call v, '&prefix:<-->'
        and not qast-contains-call v, '&postfix:<-->'
    }, 'num, void context --';

    qast-is ｢my num $i = 2e0; my int $y; $y = 1e0 + $i--｣, -> \v {
                qast-contains-op   v, 'sub_n'
        and not qast-contains-call v, '&prefix:<-->'
        and not qast-contains-call v, '&postfix:<-->'
    }, 'num, non-void context --';
}
