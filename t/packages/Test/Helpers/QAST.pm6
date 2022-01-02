unit module Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;

sub qast-contains-op (Mu $qast, $name --> Bool:D) is export {
    if nqp::istype($qast, QAST::Op) && $qast.op ~~ $name {
        return True;
    }
    elsif qast-descendable $qast {
        for $qast.list {
            qast-contains-op $_, $name and return True;
        }
    }
    False
}

sub qast-contains-callmethod (Mu $qast, $name --> Bool:D) is export {
    if nqp::istype($qast, QAST::Op)
    && $qast.op eq 'callmethod' && $qast.name ~~ $name {
        return True;
    }
    elsif qast-descendable $qast {
        for $qast.list {
            qast-contains-callmethod $_, $name and return True;
        }
    }
    False
}

sub qast-contains-call (Mu $qast, $name --> Bool:D) is export {
    if nqp::istype($qast, QAST::Op)
    && ( $qast.op eq 'call'  || $qast.op eq 'callstatic'
      || $qast.op eq 'chain' || $qast.op eq 'chainstatic')
    && $qast.name ~~ $name {
        return True;
    }
    elsif qast-descendable $qast {
        for $qast.list {
            qast-contains-call $_, $name and return True;
        }
    }
    False
}

sub qast-descendable (Mu $qast --> Bool:D) is export {
    so nqp::istype($qast, QAST::Stmts)
    || nqp::istype($qast, QAST::Stmt)
    || nqp::istype($qast, QAST::CompUnit)
    || nqp::istype($qast, QAST::Block)
    || nqp::istype($qast, QAST::Op)
    || nqp::istype($qast, QAST::Want)
}

sub qast-is (Str:D $code is copy, &test, Str:D $desc,
    Bool:D :$full = False,
    Str:D  :$target where 'optimize'|'ast' = 'optimize',
) is export is test-assertion {
    $code = "use nqp; nqp::qast_test_START_MARK;\n"
      ~ $code ~ "\n; nqp::qast_test_END_MARK;\n"
    unless $full;

    my $eval_ctx := nqp::getattr(CALLER::, PseudoStash, '$!ctx');
    my $compiled := nqp::getcomp('Raku').compile(
      $code,
      :compunit_ok, :outer_ctx($eval_ctx), :mast_frames(nqp::hash), :$target);

    $compiled := find-tested-qast $compiled unless $full;
    ok test($compiled), $desc;
}

### private subs

sub find-tested-qast (Mu $parent, Mu $qast = $parent) {
    if nqp::istype($qast, QAST::Stmt)
    && nqp::elems(nqp::decont($qast)) == 1
    && nqp::istype($qast.list[0], QAST::Op)
    && $qast.list[0].op eq 'qast_test_START_MARK' {
        my $res := QAST::Stmts.new;
        for $parent.list -> Mu \v {
            next unless (nqp::istype(v, QAST::Stmt)
                && nqp::istype(v.list[0], QAST::Op)
                && v.list[0].op eq 'qast_test_START_MARK')
              ^ff^ (nqp::istype(v, QAST::Stmt)
                && nqp::istype(v.list[0], QAST::Op)
                && v.list[0].op eq 'qast_test_END_MARK');
            $res.push: v;
        }
        return $res;
    }
    elsif qast-descendable $qast {
        for $qast.list {
            find-tested-qast $qast, $_ andthen return $_
        }
    }
    Nil
}

=begin pod

=head1 NAME

C<Test::Helpers::QAST>

=head1 SYNOPSIS

    use lib <t/packages>;
    use Test::Helpers::QAST;
    use Test;
    plan 1;

    subtest 'postfix-inc on natives gets overwritten to NQP ops' => {
        plan 2;
        qast-is ｢my int $i; $i++｣, -> \v {
                    qast-contains-op   v, 'add_i'
            and not qast-contains-call v, '&prefix:<++>'
            and not qast-contains-call v, '&postfix:<++>'
        }, 'void context ++';
        qast-is ｢my int $i; my int $y; $y = 1 + $i++｣, -> \v {
                    qast-contains-op   v, 'add_i'
            and not qast-contains-call v, '&prefix:<++>'
            and not qast-contains-call v, '&postfix:<++>'
        }, 'non-void context ++';
    }

=head1 EXPORTED MAIN SUBROUTINES

=head2 C<qast-is>

Defined as:

    sub qast-is (Str:D $code is copy, &test, Str:D $desc,
        Bool:D :$full = False,
        Str:D  :$target where 'optimize'|'ast' = 'optimize',
    )

Evals C<$code> and calls C<&test> with the resultant QAST tree as
the positional argument. The C<$target> controlls whether the dumped
tree is from stage C<optimize> (default) or from stage C<ast>.
The C<$desc> is the description of the test. If C<&test> returns a truthy value,
the test passes, otherwise it fails.

By default, the given C<$code> will be wrapped around with two variable
declarations which will be used to demarcate the slice of QAST tree to give to
C<&test>. This cuts out all the cruft and gives you just a small, relevant
piece of the tree to test. Set C<:$full> to a true value to disable this
behaviour and have C<&test> receive the full generated QAST::CompUnit tree.

=head1 EXPORTED HELPER SUBROUTINES

These routines are helper subs you can use to navigate and test a QAST tree
while performing the test.

These do NOT generate any TAP output.

=head2 C<qast-descendable>

Defined as:

    sub qast-descendable (Mu $qast --> Bool:D)

Takes a QAST node and returns True if it's a QAST::Stmts,
QAST::Stmt, QAST::CompUnit, QAST::Block, QAST::Op, or a QAST::Want node;
False otherwise.

=head2 C<qast-contains-op>

Defined as:

    sub qast-contains-op (Mu $qast, $op --> Bool:D);

Takes a QAST tree and tests whether it has QAST::Op with C<.op> smartmatching
C<True> with C<$op>. Recurses into descendable ops.

=head2 C<qast-contains-call>

Defined as:

    sub qast-contains-call (Mu $qast, $name --> Bool:D);

Takes a QAST tree and tests whether it has QAST::Op with C<.op> set to
C<call>, C<callstatic>, C<chain>, or C<chainstatic>, and with C<.name> smartmatching
C<True> with C<$name>. Recurses into descendable ops.

=head2 C<qast-contains-callmethod>

Defined as:

    sub qast-contains-callmethod (Mu $qast, $name --> Bool:D);

Takes a QAST tree and tests whether it has QAST::Op with C<.op> set to
C<callmethod> and with C<.name> smartmatching C<True> with C<$name>.
Recurses into descendable ops.

B<NOTE:> C<callmethod> op can also take the name as second positional arg.
This routine does B<NOT> inspect such nodes.

=end pod
