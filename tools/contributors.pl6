use v6;

my $last_release = @*ARGS.shift // do {
    Date.new-from-daycount:
        .daycount  # daycount for 1st of previous month
        + (.day-of-week == 7 ?? 6 !! 6 - .day-of-week) # offset of the first Saturday
	+ 2*7  # add two extra weeks, to get 3rd Saturday
    given Date.today.earlier(:1month).truncated-to: 'month';
}

# Check all the places with repos that may be applicable.  Get all of the
# committers in that repo since the given date as commit ID => author pairs.
# Only keep the unique commit ID's (so that each author only gets credited
# once for a commit, even if multiple repo's are identical).  Then take the
# authors and put them in a Bag.  Sort the bag by frequency, highest first.
# Then take the keys (aka the authors), and join them in a list.

say "Contributors to Rakudo since the release on $last_release:";
say
< . ../MoarVM ../nqp ../roast ../doc nqp nqp/MoarVM t/spec >.map({
  |get-committers($_,$last_release)
}).unique(:as(*.key))>>.value.Bag.sort(*.value).reverse>>.key.join(', ');

sub get-committers($repo, $since) {
    return Empty unless $repo.IO.d && "$repo/.git".IO.d;

    gather for shell(:out, :cwd($repo),
      "git log --since=$since --pretty='format:%an|%cn|%H|%s'"
    ).out.lines.grep(?*) -> $line {  # grep needed because of (Str) on empty pipe

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
