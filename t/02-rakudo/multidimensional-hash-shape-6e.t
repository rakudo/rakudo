use v6.e.PREVIEW;
use Test;

plan 18;

my %h{Str;Int};
ok %h.of === Mu,
    'the values of an untyped hash of two dimensions are Mu';
%h{'a';1} = Mu;
ok %h{'a';1} === Mu,
    'the values of an untyped hash of two dimensions take Mu';

{
    my %s{Str;Int};
    %s{'a';1} = 1;
    %s{'b';1} = 2;
    is-deeply (%s{'a';1}:exists, %s{'a';2}:exists), (True, False),
        'an element of a hash of two dimensions takes :exists';
    is-deeply %s{'a';1}:p, ('a', 1) => 1,
        'an element of a hash of two dimensions takes :p';
    my @key = 'a', 1;
    my $pair = %s{||@key}:p;
    @key[0] = 'z';
    is-deeply $pair, ('a', 1) => 1,
        'the key :p gives does not change with the array it came from';
    is-deeply %s{'a','b';1}.List, (1, 2),
        'a hash of two dimensions takes a slice of the keys of a dimension';
    is-deeply %s{*;1}.sort.List, (1, 2),
        'a whatever star in a dimension matches the elements with any key there';
    is-deeply %s{'a'}.List, (1,),
        'a key for the first dimension alone takes any key for the second';
    is-deeply (%s{**}:deepk).sort.List, ([('a', 1)], [('b', 1)]),
        'a hyper whatever star alone takes :deepk';
    my %t{Str;Int;Str};
    %t{'a';1;'x'} = 1;
    %t{'a';2;'y'} = 2;
    is-deeply %t{'a';1}.List, (1,),
        'a hash of three dimensions takes any key for a dimension left out';
    throws-like { %s{*;1} = 3 }, Exception,
        message => /'non-deterministic'/,
        'a whatever star slice cannot be assigned';
    my $v = 3;
    %s{'c';1} := $v;
    $v = 4;
    is %s{'c';1}, 4,
        'binding an element of a hash of two dimensions binds the container';
    is-deeply (%s{'a';1}:delete, %s.elems), (1, 2),
        'an element of a hash of two dimensions takes :delete';
}

{
    my %p;
    %p{'a';'b'} = 1;
    %p{|| ('c', 'd')} = 2;
    is-deeply %p, %(a => %(b => 1), c => %(d => 2)),
        'assigning through a multidimensional subscript of a hash of hashes vivifies its hashes';
    my $v = 1;
    %p{'e';'f'} := $v;
    $v = 2;
    is %p<e><f>, 2,
        'binding through a multidimensional subscript of a hash of hashes binds the container';
    throws-like { %p{'a','c';'b'} := $v }, X::Bind::Slice,
        'binding through a multidimensional slice of a hash of hashes is refused';
    throws-like { %p{'a';'b'}:exists:foo }, X::Adverb,
        unexpected => ('foo',), nogo => ('exists',),
        'a multidimensional subscript refuses an unknown adverb';
    my @empty;
    throws-like { %p{||@empty} := $v }, X::Bind::ZenSlice,
        'binding through a multidimensional subscript of no keys is refused';
}

# vim: expandtab shiftwidth=4
