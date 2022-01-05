#!/usr/bin/env raku

# This helper script is intended to make adding leap seconds to the
# Raku system as easy as possible: by just giving the date for which
# to add the leap second, it will scan the file for current leap
# second information and add leap second information for that date
# to the source, which is output to STDOUT.  After inspection one
# can then just overwrite the source-file and a re-compilation
# of the setting should then be enough to activate the new leap
# second in Raku's time logic.

use v6.c;

sub MAIN(
  #| the date for which to add a leap second
  Str $the-date,
  #| the source file containing leap second logic (default: src/core.c/Rakudo/Internals.pm6)
  $the-source = 'src/core.c/Rakudo/Internals.pm6'
) {

    # set up the new leap second info
    my $date     = Date.new($the-date);
    my $epoch    = $date.DateTime.posix;
    my $before   = $date.earlier(:1day);
    my $daycount = $date.daycount;

    # run through the source file and update as appropriate
    for $the-source.IO.lines -> $line {
        say "        '$before',"
          if $line eq '        #END leap-second-dates';
        say "        $epoch,"
          if $line eq '        #END leap-second-posix';
        say "        $daycount,"
          if $line eq '        #END leap-second-daycount';
        say $line;
    }
}
