use v6;

sub MAIN (
    $last_release? is copy,
    :$rakudo = '.',
    :$doc    = '../doc',
    :$nqp    = 'nqp',
    :$moar   = 'nqp/MoarVM',
    :$roast  = 't/spec',
) {
    $last_release //= get-last-release-date-for $rakudo;

    # Check all the places with repos that may be applicable.  Get all of the
    # committers in that repo since the given date as commit ID => author pairs.
    # Only keep the unique commit ID's (so that each author only gets credited
    # once for a commit, even if multiple repo's are identical).  Then take the
    # authors and put them in a Bag.  Sort the bag by frequency, highest first.
    # Then take the keys (aka the authors), and join them in a list.

    my @repos = $rakudo, $doc, $nqp, $roast;
    note qq:to/END/;
        ###############################################################
        Extracting contributor information from these locations.
        @repos[]
        Ensure those repositories exist and have all changes pulled in.
        ###############################################################
        END

    my @*CREDITS = ($rakudo, $doc, $nqp, $moar, $roast
        )».IO».child('CREDITS').grep(*.r)».lines.flat;

    say "Contributors to Rakudo since the release on $last_release:";
    my @contributors = @repos.map({
      |get-committers($_,$last_release)
    }).unique(:as(*.key))».value.Bag.sort(*.value).reverse».key;

    for @contributors -> $name is rw {
        state $length = 0;
        $length += $name.chars + 2; # 2 extra for the comma and space after the name
        if $length > 78 {
            # prepend newline to name, so when we join and print the entire list,
            # this name will start on the new line
            $name = "\n$name";
            $length = $name.chars + 2;
        }

    }
    say @contributors.join(', ');
}

sub get-last-release-date-for ($rakudo-repo) {
    # .first's regex gives us the last non-point release
    # and we need the .slurps to silence "fatal write reset by peer" errors
    my $tag = run(
        :out, :cwd($rakudo-repo),
        <git for-each-ref --sort=taggerdate --format>,
        q|%(refname) %(taggerdate)|, q|refs/tags|
      ).out.slurp(:close).lines.reverse
      .first(/^ 'refs/tags/' \d**4 '.' \d**2 \s+/)
      .substr('refs/tags/'.chars, '20XX.XX'.chars)
      or die "Failed to find a release tag for latest release";

    Date.new: Instant.from-posix: run(
        :out, :cwd($rakudo-repo), <git log --pretty=format:%ct>, $tag
    ).out.slurp(:close).lines.head;
}

sub get-committers($repo, $since) {
    die "Expected a repo in `$repo` but did not find one"
         unless $repo.IO.d && "$repo/.git".IO.d;

    gather for run(:out, :cwd($repo),
      <git log --since>, $since, '--pretty=format:%an|%cn|%H|%s'
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
        gather for @*CREDITS {
            when /^N:/ {
                $name = .substr(3);
            }
            when /^U:/ {
                take lc(.substr(3)) => $name if $name;
            }
        }
    }
    $nick
    ?? (%nick-to-name{lc $nick} // $nick).subst(/\S+ '@' <(\S+/, '<redacted>')
    !! '<unknown>';
}
