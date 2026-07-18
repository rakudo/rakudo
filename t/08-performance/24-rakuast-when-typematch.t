use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 15;

# A when whose matcher is a compile-time type object tests the topic
# with istype instead of dispatching the matcher's ACCEPTS, with the
# same runtime guard a reduced smartmatch takes for a topic that turns
# out to be a concrete Junction.

# The legacy frontend reduces when statements through its own QAST
# optimizer, so the shapes here are this frontend's.
if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'given 42 { when Int { say 1 } }', :full, -> \v {
        qast-contains-op(v, 'istype')
            and not qast-contains-callmethod(v, 'ACCEPTS')
    }, 'a when with a type-object matcher tests istype and calls no ACCEPTS';

    qast-is 'my class C { method ACCEPTS($x) { True } }; given 42 { when C { say 1 } }', :full, -> \v {
        qast-contains-callmethod(v, 'ACCEPTS')
    }, 'a matcher with a user ACCEPTS keeps the dispatch';

    qast-is 'given 42 { when 5 { say 1 } }', :full, -> \v {
        qast-contains-callmethod(v, 'ACCEPTS')
    }, 'a literal matcher is not a type object and keeps the dispatch';

    qast-is '$_ = 42; say "x" when Int', :full, -> \v {
        qast-contains-op(v, 'istype')
            and not qast-contains-callmethod(v, 'ACCEPTS')
    }, 'a when statement modifier with a type-object matcher reduces the same way';
}
else {
    skip 'the reduced when shapes are specific to the RakuAST frontend', 4;
}

# Behavior stays identical.

{
    my @r;
    for 42, "x", 3.5e0 {
        given $_ {
            when Int { @r.push('int') }
            when Str { @r.push('str') }
            default  { @r.push('def') }
        }
    }
    is @r.join(','), 'int,str,def', 'a reduced when chain dispatches each topic to the right arm';
}

{
    my class C { method ACCEPTS($x) { $x == 42 } }
    my @r;
    given 42 { when C { @r.push('custom') }; default { @r.push('no') } }
    given 43 { when C { @r.push('custom') }; default { @r.push('no') } }
    is @r.join(','), 'custom,no', 'a user ACCEPTS matcher still runs its own logic';
}

{
    my @r;
    given any(1, "x") { when Int { @r.push('yes') }; default { @r.push('no') } }
    given any("y", "x") { when Int { @r.push('yes') }; default { @r.push('no') } }
    is @r.join(','), 'yes,no', 'a concrete Junction topic still autothreads the match';
}

{
    subset Even of Int where * %% 2;
    my @r;
    given 4 { when Even { @r.push('even') }; default { @r.push('odd') } }
    given 3 { when Even { @r.push('even') }; default { @r.push('odd') } }
    is @r.join(','), 'even,odd', 'a subset matcher still runs its refinement';
}

{
    my @r;
    given Int { when Int { @r.push('type') }; default { @r.push('no') } }
    is @r.join(','), 'type', 'a type-object topic matches its own type';
}

{
    my @r;
    given 5 {
        when Int     { @r.push('int'); proceed }
        when Numeric { @r.push('num') }
        default      { @r.push('def') }
    }
    is @r.join(','), 'int,num', 'proceed still falls through a reduced when';
}

{
    my @r;
    given 5 {
        when Str { @r.push('str') }
        @r.push('between');
        when Int { @r.push('int') }
    }
    is @r.join(','), 'between,int', 'statements between reduced whens still run';
}

{
    $_ = 42;
    my @r;
    @r.push('mod') when Int;
    @r.push('strmod') when Str;
    is @r.join(','), 'mod', 'reduced when statement modifiers fire on the topic type';
}

{
    my @r;
    given Mu { when Mu { @r.push('mu') } }
    given 1  { when Mu { @r.push('any-topic') } }
    is @r.join(','), 'mu,any-topic', 'a Mu matcher accepts everything either way';
}

{
    my @r;
    for 1, "x" {
        when Int { @r.push('int'); succeed }
        when Str { @r.push('str') }
    }
    is @r.join(','), 'int,str', 'succeed leaves a reduced when arm cleanly';
}

{
    my $out = do given 42 { when Int { 'val' }; default { 'no' } };
    is $out, 'val', 'a given returning a reduced when arm value still returns it';
}
