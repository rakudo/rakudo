use lib <t/packages>;
use Test::Helpers::QAST;
use Test;
plan 4;

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


subtest '.dispatch:<.=> gets rewritten to simple ops' => {
    plan +my @codes :=
      ｢(my Int $x .=new).="{"new"}"(42);｣,
      ｢my Int $x; .=new andthen .=new orelse .=new;｣,
      ｢my \foo .= new｣,
      ｢my Int \foo .= new｣,
      ｢my Int $a; .=new without $a｣,
      ｢my Int $a; .=new with $a｣,
      ｢my Int $a; $a .= new｣,
      ｢my @a; @a .= new｣,   ｢my @a .= new｣,
      ｢my %a; %a .= new｣,   ｢my %a .= new｣,
      ｢my &a; &a .= new｣,   ｢my &a .= new｣,
      ｢my $b = "meows"; $b .= WHAT｣,
      ｢my @b = <z a b d e>; @b .= sort｣,
    ;

    for @codes -> \code {
        qast-is code, :full, -> \v {
            not qast-contains-callmethod v, 'dispatch:<.=>'
        }, code;
    }
}

subtest 'for {}' => {
    my @fors = ｢for ^10 {}｣, ｢for 1..10 {}｣, ｢for 1..^10 {}｣, ｢for 1^..10 {}｣, ｢for 1^..^10 {}｣,
        ｢for 1...10 {}｣, ｢for 1, 2...10 {}｣, ｢for 10...2 {}｣, ｢for 1,3...9 {}｣, ｢for 9,7...1 {}｣,
        ｢for ^10 .reverse {}｣,;
    plan @fors + 2;

    for @fors {
        qast-is $_, -> \v {
                not qast-contains-op   v, 'p6forstmt'
            and not qast-contains-op   v, 'p6for'
        }, $_ ~ ' case gets optimized entirely';
    }

    qast-is ｢for ^10 {}｣, :target<ast>, -> \v {
        qast-contains-op v, 'p6forstmt'
    }, 'simple `for ^10 {}` case gets `p6forstmt` op to use';

    qast-is ｢for ^10 -> $, :$foo {}｣, :target<ast>, -> \v {
                qast-contains-op   v, 'p6forstmt'
        and not qast-contains-op   v, 'p6for'
    }, 'named arg does not accidentally get counted as a positional';
}

# https://github.com/rakudo/rakudo/issues/1981
subtest 'nested metaops get fully rewritten away from &METAOP sub calls' => {
    plan 2;
    qast-is ｢my $a; ($a //= 0) += 1｣, -> \v { not qast-contains-call v, /METAOP/ }, '(//=)+=';
    qast-is ｢my $a; (((($a //= 0) += 1) //= 0) += 1)｣, -> \v { not qast-contains-call v, /METAOP/ },
      '((((//=)+=) //=) +=)';
}

# vim: expandtab shiftwidth=4
