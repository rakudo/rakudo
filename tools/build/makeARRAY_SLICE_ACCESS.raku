#!/usr/bin/env raku

# This script reads the Array/Slice.pm6 file, and generates the
# necessary classes for inclusion in the postcircumfix:<[ ]> table for
# accessing array slices, and writes it back to the file.
#
# NOTE: the usefulness of this script is limited: once the 50x
# performance penalty on calling a private method of a consuming
# class from a role is gone, the code can be simplified to making
# a role for lazy and a role for non-lazy, and have the generated
# classes be simple consumers of those roles.  This performance
# improvement is expected to occur with the merging of the RakuAST
# work.

# always use highest version of Raku
use v6.*;

my $generator = $*PROGRAM-NAME;
my $generated = DateTime.now.gist.subst(/\.\d+/,'');
my $start     = '#- start of generated part of array slice access';
my $end       = '#- end of generated part of array slice access';

# slurp the whole file and set up writing to it
my $filename = "src/core.c/Array/Slice.pm6";
my @lines = $filename.IO.lines;
$*OUT = $filename.IO.open(:w);

# for all the lines in the source that don't need special handling
while @lines {
    my $line := @lines.shift;

    # nothing to do yet
    unless $line.starts-with($start) {
        say $line;
        next;
    }

    # found header
    say $start ~ " " ~ "-" x 79 - $start.chars;
    say "#- Generated on $generated by $generator";
    say "#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE";

    # skip the old version of the code
    while @lines {
        last if @lines.shift.starts-with($end);
    }

    for (
      'no actionable adverbs',
      'none',
      Q:to/CODE/,
        nqp::push($!result,$!iterable.AT-POS(pos));
CODE
      Q:to/CODE/,
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,$!iterable.AT-POS(pos))
          !! ($!done = 1);
CODE

      ':kv',
      'kv',
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,$!iterable.AT-POS(pos));
        }
CODE
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,$!iterable.AT-POS(pos));
        }
        else {
            $!done = 1;
        }
CODE

      ':!kv',
      'not-kv',
      Q:to/CODE/,
        nqp::push($!result,pos);
        nqp::push($!result,$!iterable.AT-POS(pos));
CODE
      "same as lazy :kv",

      ':p',
      'p',
      Q:to/CODE/,
        nqp::push($!result,Pair.new(pos,$!iterable.AT-POS(pos)))
          if $!iterable.EXISTS-POS(pos);
CODE
      Q:to/CODE/,
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,Pair.new(pos,$!iterable.AT-POS(pos)))
          !! ($!done = 1);
CODE

      ':!p',
      'not-p',
      Q:to/CODE/,
        nqp::push($!result,Pair.new(pos,$!iterable.AT-POS(pos)));
CODE
      "same as lazy :p",

      ':k',
      'k',
      Q:to/CODE/,
        nqp::push($!result,pos) if $!iterable.EXISTS-POS(pos);
CODE
      Q:to/CODE/,
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,pos)
          !! ($!done = 1);
CODE

      ':!k',
      'not-k',
      Q:to/CODE/,
        nqp::push($!result,pos);
CODE
      "same as lazy :k",

      ':v',
      'v',
      Q:to/CODE/,
        nqp::push($!result,$!iterable.AT-POS(pos))
          if $!iterable.EXISTS-POS(pos);
CODE
      Q:to/CODE/,
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,$!iterable.AT-POS(pos))
          !! ($!done = 1);
CODE

      ':exists',
      'exists',
      Q:to/CODE/,
        nqp::push($!result,$!iterable.EXISTS-POS(pos));
CODE
      Q:to/CODE/,
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,True)
          !! ($!done = 1);
CODE

      ':exists:kv',
      'exists-kv',
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,True);
        }
CODE
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,True);
        }
        else {
            $!done = 1;
        }
CODE

      ':exists:!kv',
      'exists-not-kv',
      Q:to/CODE/,
        nqp::push($!result,pos);
        nqp::push($!result,$!iterable.EXISTS-POS(pos));
CODE
      "same as lazy :exists:kv",

      ':exists:p',
      'exists-p',
      Q:to/CODE/,
        nqp::push($!result,Pair.new(pos,True))
          if $!iterable.EXISTS-POS(pos);
CODE
      Q:to/CODE/,
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,Pair.new(pos,True))
          !! ($!done = 1);
CODE

      ':exists:!p',
      'exists-not-p',
      Q:to/CODE/,
        nqp::push($!result,Pair.new(pos,$!iterable.EXISTS-POS(pos)));
CODE
      "same as lazy :exists:p",

      ':exists:delete',
      'exists-delete',
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,True);
        }
        else {
            nqp::push($!result,False);
        }
CODE
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,True);
        }
        else {
            $!done = 1;
        }
CODE

      ':exists:delete:kv',
      'exists-delete-kv',
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,pos);
            nqp::push($!result,True);
        }
CODE
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,pos);
            nqp::push($!result,True);
        }
        else {
            $!done = 1;
        }
CODE

      ':exists:delete:!kv',
      'exists-delete-not-kv',
      Q:to/CODE/,
        nqp::push($!result,pos);
        self!delete(pos)
          if nqp::push($!result,$!iterable.EXISTS-POS(pos));
CODE
      "same as lazy :exists:delete:kv",

      ':exists:delete:p',
      'exists-delete-p',
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,Pair.new(pos,True));
        }
CODE
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,Pair.new(pos,True));
        }
        else {
            $!done = 1;
        }
CODE

      ':exists:delete:!p',
      'exists-delete-not-p',
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,Pair.new(pos,True));
        }
        else {
            nqp::push($!result,Pair.new(pos,False));
        }
CODE
      "same as lazy :exists:delete:p",

      ':!exists',
      'not-exists',
      Q:to/CODE/,
        nqp::push($!result,!$!iterable.EXISTS-POS(pos));
CODE
      Q:to/CODE/,
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,False)
          !! ($!done = 1);
CODE

      ':!exists:kv',
      'not-exists-kv',
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,False);
        }
CODE
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,False);
        }
        else {
            $!done = 1;
        }
CODE

      ':!exists:!kv',
      'not-exists-not-kv',
      Q:to/CODE/,
        nqp::push($!result,pos);
        nqp::push($!result,!$!iterable.EXISTS-POS(pos));
CODE
      "same as lazy :!exists:kv",

      ':!exists:p',
      'not-exists-p',
      Q:to/CODE/,
        nqp::push($!result,Pair.new(pos,False))
          if $!iterable.EXISTS-POS(pos);
CODE
      Q:to/CODE/,
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,Pair.new(pos,False))
          !! ($!done = 1);
CODE

      ':!exists:!p',
      'not-exists-not-p',
      Q:to/CODE/,
        nqp::push($!result,Pair.new(pos,!$!iterable.EXISTS-POS(pos)));
CODE
      "same as lazy :!exists:p",

      ':!exists:delete',
      'not-exists-delete',
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,False);
        }
        else {
            nqp::push($!result,True);
        }
CODE
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,False);
        }
        else {
            $!done = 1;
        }
CODE

      ':!exists:delete:kv',
      'not-exists-delete-kv',
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,pos);
            nqp::push($!result,False);
        }
CODE
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,pos);
            nqp::push($!result,False);
        }
        else {
            $!done = 1;
        }
CODE

      ':!exists:delete:!kv',
      'not-exists-delete-not-kv',
      Q:to/CODE/,
        nqp::push($!result,pos);
        self!delete(pos)
          if nqp::push($!result,!$!iterable.EXISTS-POS(pos));
CODE
      "same as lazy :!exists:delete:kv",

      ':!exists:delete:p',
      'not-exists-delete-p',
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,Pair.new(pos,False));
        }
CODE
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,Pair.new(pos,False));
        }
        else {
            $!done = 1;
        }
CODE

      ':!exists:delete:!p',
      'not-exists-delete-not-p',
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,Pair.new(pos,False));
        }
        else {
            nqp::push($!result,Pair.new(pos,True));
        }
CODE
      "same as lazy :!exists:delete:p",

      ':delete',
      'delete',
      Q:to/CODE/,
        nqp::push($!result,self!delete(pos));
CODE
      Q:to/CODE/,
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,self!delete(pos))
          !! ($!done = 1);
CODE

      ':delete:kv',
      'delete-kv',
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,self!delete(pos));
        }
CODE
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            nqp::push($!result,pos);
            nqp::push($!result,self!delete(pos));
        }
        else {
            $!done = 1;
        }
CODE

      ':delete:kv',
      'delete-not-kv',
      Q:to/CODE/,
        nqp::push($!result,pos);
        nqp::push($!result,self!delete(pos));
CODE
      "same as lazy :delete:kv",

      ':delete:p',
      'delete-p',
      Q:to/CODE/,
        nqp::push($!result,Pair.new(pos,self!delete(pos)))
          if $!iterable.EXISTS-POS(pos);
CODE
      Q:to/CODE/,
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,Pair.new(pos,self!delete(pos)))
          !! ($!done = 1);
CODE

      ':delete:!p',
      'delete-not-p',
      Q:to/CODE/,
        nqp::push($!result,Pair.new(pos,self!delete(pos)));
CODE
      "same as lazy :delete:p",

      ':delete:k',
      'delete-k',
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,pos);
        }
CODE
      Q:to/CODE/,
        if $!iterable.EXISTS-POS(pos) {
            self!delete(pos);
            nqp::push($!result,pos);
        }
        else {
            $!done = 1;
        }
CODE

      ':delete:!k',
      'delete-not-k',
      Q:to/CODE/,
        self!delete(pos) if $!iterable.EXISTS-POS(pos);
        nqp::push($!result,pos);
CODE
      "same as lazy :delete:k",

      ':delete:v',
      'delete-v',
      Q:to/CODE/,
        nqp::push($!result,self!delete(pos))
          if $!iterable.EXISTS-POS(pos);
CODE
      Q:to/CODE/,
        $!iterable.EXISTS-POS(pos)
          ?? nqp::push($!result,self!delete(pos))
          !! ($!done = 1);
CODE

    ) -> $comment, $class, $code, $lazy-code {

        for False, True -> $lazy {

            # don't need to do duped classes, handled in dispatch table indexing
            next if $lazy && $lazy-code.starts-with("same as");

            # set up template values
            my %mapper =
              comment   => ($lazy ?? "lazy, $comment" !! $comment),
              class     => ($lazy ?? "lazy-$class" !! $class),
              accept    => ($lazy ?? $lazy-code !! $code).chomp,
              done      => ($lazy ?? '$!done || ' !! ''),
              attribute => ($lazy ?? "    has int \$!done;\n" !! ""),
              delete    => ($comment.contains('delete')
                ?? Q:to/CODE/

    # Helper method for deleting elements, making sure that the total number
    # of elements is fixed from *before* the first deletion, so that relative
    # positions such as *-1 will continue to refer to the same position,
    # even if the last element of an array was removed (which shortens the
    # array).
    method !delete(\pos) {
        $!elems := $!iterable.elems if nqp::isnull($!elems);
        $!iterable.DELETE-POS(pos)
    }
CODE
                !! ""),
            ;

            # spurt this class
            say Q:to/SOURCE/.subst(/ '#' (\w+) '#' /, -> $/ { %mapper{$0} }, :g).chomp;

# #comment#
my class Array::Slice::Access::#class# {
    has $!result;
    has $!elems;
    has $!iterable;
#attribute#
    method !accept(\pos --> Nil) {
#accept#
    }

    method !SET-SELF(\iterable) {
        $!result   := nqp::create(IterationBuffer);
        $!elems    := nqp::null;
        $!iterable := iterable;
        self
    }
    method new(\iterable) { nqp::create(self)!SET-SELF(iterable) }
#delete#
    # Handle iterator in the generated positions: this will add a List
    # with the elements pointed to by the iterator to the result.  Because
    # these positions can also be non-Int, some trickery needs to be
    # done to allow this being called recursively.
    method !handle-iterator(\iterator) {

        # basically push the current result on a stack
        my int $mark = nqp::elems($!result);

        nqp::until(
          #done#nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        # Take what was added and push it as a List
        if nqp::sub_i(nqp::elems($!result),$mark) -> int $added {
            my $buffer;
            if $mark {
                $buffer := nqp::slice($!result,$mark,$added);
                nqp::setelems($!result,$mark);
            }
            else {
                $buffer := $!result;
                $!result := nqp::create(IterationBuffer);
            }
            nqp::push($!result,$buffer.List);
        }
    }

    # Handle anything non-integer in the generated positions
    method !handle-nonInt(\pos) {
        nqp::istype(pos,Iterable)
          ?? nqp::iscont(pos)
            ?? self!accept(pos.Int)
            !! self!handle-iterator(pos.iterator)
          !! nqp::istype(pos,Callable)
            ?? nqp::istype(
                 (my $real := (pos)(
                   nqp::ifnull($!elems,$!elems := $!iterable.elems)
                 )),
                 Int
               )
              ?? self!accept($real)
              !! self!handle-nonInt($real)
            !! nqp::istype(pos,Whatever)
              ?? self!handle-iterator(
                   (^nqp::ifnull(
                       $!elems,
                       $!elems := $!iterable.elems
                     )).iterator
                 )
              !! self!accept(pos.Int)
    }

    # The actual building of the result
    method slice(\iterator) {
        nqp::until(
          #done#nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            self!accept(pos),
            self!handle-nonInt(pos)
          )
        );

        $!result.List
    }
}
SOURCE
        }
    }

    # we're done for this combination of adverbs
    say "";
    say "#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE";
    say $end ~ " " ~ "-" x 79 - $end.chars;
}

# close the file properly
$*OUT.close;

# vim: expandtab sw=4
