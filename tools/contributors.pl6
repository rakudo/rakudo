use v6;

my $last_release = @*ARGS.shift // do {
    my $today            = Date.today;
    my $last-month       = $today - $today.days-in-month;
    my $first-last-month = Date.new( $last-month.year, $last-month.month, 1 );
    $first-last-month + 25 - $first-last-month.day-of-week;
}

# Check all the places with repo's that may be applicable.  Get all of the
# committers in that repo since the given date as commit ID => author pairs.
# Only keep the unique commit ID's (so that each author only gets credited
# once for a commit, even if multiple repo's are identical).  Then take the
# authors and put them in a Bag.  Sort the bag by frequency, highest first.
# Then take the keys (aka the authors), and join them in a list.

say "Contributors to Rakudo since the release on $last_release:";
say
< . ../MoarVM ../nqp ../roast nqp nqp/MoarVM t/spec >.map({
  get-committers($_,$last_release);
}).unique(:as(*.key))>>.value.Bag.sort(*.value).reverse>>.key.join(', ');

sub get-committers($repo, $since) {
    return Empty unless $repo.IO.d && "$repo/.git".IO.d;

    gather for pipe(
      "cd $repo; git log --since=$since --pretty='format:%an|%cn|%H|%s'"
    ).lines.grep(?*) -> $line {  # grep needed because of (Str) on empty pipe

        my ($author,$committer,$id,$msg) = $line.split('|',4);
        take $id => nick-to-name($author);
        take $id => nick-to-name($committer);

        # handle (foo)++ in message
        for $msg.comb(/<?after \(> <-[)]> ** 2..* <?before \)\+\+>/) -> $nick {
            take $id => nick-to-name($nick);
        }

        # handle foo++ in message
        for $msg.comb(/<-[\s():]> ** 2..* <?before \+\+>/).grep(/^ <-[$@]>/) -> $nick {
            take $id => nick-to-name($nick);
        }

        # handle "courtesy of foo"
        for $msg.comb(rx:i/ <?after courtesy \s+ of\s*\:? \s*> \S+/) -> $nick {
            take $id => nick-to-name($nick);
        }
    }
}

sub nick-to-name($nick) {
    state %nick-to-name = do {
        my $name;
        gather for "CREDITS".IO.lines {
            when /^N:/ {
                $name = .substr(3);
            }
            when /^U:/ {
                take lc(.substr(3)) => $name if $name;
            }
        }
    }
    $nick ?? ( %nick-to-name{lc $nick} // $nick ) !! '<unknown>';
}
