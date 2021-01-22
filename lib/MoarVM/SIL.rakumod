# This module is primarily intended for optimising code in the Rakudo
# setting on the MoarVM backend.  When loaded, it will basically make
# sure that whatever code you are about to execute (be that from a file
# or from the command line with -e) will be run with the
# MVM_SPESH_INLINE_LOG=1 environment variable set, which causes a
# a report about which frames were possible to inline during execution,
# and which not.  Hence the name SIL (aka Spesh Inline Log).

use nqp;

class BB {
    has Str() $.name is built(:bind);
    has Int() $.id   is built(:bind);
    has Int() $.size is built(:bind);

    method name() {
        $!name ?? "$!name BB($!id)" !! "BB($!id)"
    }

    method gist() { self.Str }
    method Str() {
        $!size ?? "$.name.chop(), $!size bytes)" !! $.name
    }
    method WHICH(--> ValueObjAt:D) {
        ValueObjAt.new("BB|$!id")
    }
}

class Inlined {
    has BB $.inlinee is built(:bind);
    has BB $.into    is built(:bind);

    method gist() { self.Str }
    method Str() { "$!inlinee -> $!into" }
    method WHICH(--> ValueObjAt:D) {
        ValueObjAt.new("BB|$!inlinee.WHICH()|$!into.WHICH()")
    }
}

class Not-Inlined {
    has BB    $.frame  is built(:bind);
    has BB    $.target is built(:bind);
    has Str() $.reason is built(:bind);

    method gist() { self.Str }
    method Str() { "$!frame -> $!target:\n  $!reason" }
    method WHICH(--> ValueObjAt:D) {
        ValueObjAt.new("BB|$!frame.WHICH()|$!target.WHICH()")
    }
}

%*ENV<MVM_SPESH_INLINE_LOG> := 1;
my $out := $*OUT;
my $err := $*ERR;
my $status;

my @inlineds;
my @not-inlineds;

react {
    my $proc := Proc::Async.new(
      $*EXECUTABLE.absolute,
      Rakudo::Internals.LL-EXCEPTION,
      Rakudo::Internals.PROFILE,
      Rakudo::Internals.E,
      |@*ARGS
    );

    whenever $proc.stdout.lines {
        $out.print($_);
    }
    whenever $proc.stderr.lines {
        if m/^ 'Can inline ' (<-[(]>+)
               '(' (\d+)
               ') with bytecode size ' (\d+)
               ' into ' (<-[(]>+)
               '(' (\d+) /
        {
            @inlineds.push: Inlined.new:
              inlinee => BB.new(name => $0.chop, id => $1, size => $2),
              into    => BB.new(name => $3.chop, id => $4);

        }
        elsif m/^ 'Can NOT inline ' (<-[(]>+)
               '(' (\d+)
               ') with bytecode size ' (\d+)
               ' into ' (<-[(]>+)
               '(' (\d+) 
               '): ' (.*) /
        {
            @not-inlineds.push: Not-Inlined.new:
              frame  => BB.new(name => $0.chop, id => $1, size => $2),
              target => BB.new(name => $3.chop, id => $4),
              reason => $5;
        }
        else {
            $err.print($_);
        }
    }
    whenever $proc.start(:%*ENV) {
        $status = .exitcode;
    }
}

say "";
say "Successful inlines";
say "-" x 80;
.say for @inlineds.unique;
say "-" x 80;

say "";
say "Unsuccessful inlines:";
say "-" x 80;
.say for @not-inlineds.unique;
say "-" x 80;

exit $status;

# vim: expandtab shiftwidth=4
