#!/usr/bin/env raku
use v6;

my %*SUB-MAIN-OPTS = :named-anywhere;
# repeat named args in MAIN multies for nice USAGE messsages
constant   RAK_REPO     = '.';
constant   DOC_REPO     = '../doc';
constant   DOCSITE_REPO = '../doc-website';
constant   NQP_REPO     = 'nqp';
constant  MOAR_REPO     = 'nqp/MoarVM';
constant ROAST_REPO     = 't/spec';

subset PathToRepo of Str where .so;

#|(past releases)
multi MAIN('for', 'release', $release,
    PathToRepo :$rakudo      = RAK_REPO,
    PathToRepo :$doc         = DOC_REPO,
    PathToRepo :$doc-website = DOCSITE_REPO,
    PathToRepo :$nqp         = NQP_REPO,
    PathToRepo :$moar        = MOAR_REPO,
    PathToRepo :$roast       = ROAST_REPO,
    Bool       :$debug,
) {
    # fetch all available tags, creating a list of hashes, each of which got
    # a tag name and the date it was created on
    my @releases = run(:out, :cwd($rakudo),
      «git for-each-ref --sort=taggerdate
        '--format=%(tag)|%(creatordate)' refs/tags»
    ).out.lines(:close).map(
        *.split('|', :skip-empty).List
    ).grep(* == 2).map: { %(:tag(.head), :date(parse-date .tail)) };

    ~$release ∈ @releases».<tag> or die "Could not find $release in the list of"
      ~ " known releases: @releases».<tag>";

    # create a map of tag => the tag immediatelly previous to it (temporally)
    # this gives us the tag (along with that tag's date) for the previous rls
    my %prev-rel = @releases.rotor(2 => -1).map: {.tail<tag> => .head };
    %prev-rel{$release} or die "Could not figure out the tag for previous"
      ~ " release. Did you give me the first release ever? I can't handle it";

    # gen committers using the dates of the tag we were asked for and the
    # the tag that's previous to it, temporally, giving us the space of one rls
    say join ', ', committers :since(%prev-rel{$release}<date>),
        :until(@releases.first(*.<tag> eq $release)<date>),
        :$rakudo, :$doc, :$doc-website, :$nqp, :$moar, :$roast, :$debug,
}

#|(current release)
multi MAIN ($last_release? is copy,
    PathToRepo :$rakudo      = RAK_REPO,
    PathToRepo :$doc         = DOC_REPO,
    PathToRepo :$doc-website = DOCSITE_REPO,
    PathToRepo :$nqp         = NQP_REPO,
    PathToRepo :$moar        = MOAR_REPO,
    PathToRepo :$roast       = ROAST_REPO,
    Bool       :$debug,
) {
    $last_release //= get-last-release-date-for $rakudo;
    say join ', ', committers since => $last_release,
        :$rakudo, :$doc, :$doc-website, :$nqp, :$moar, :$roast
}

sub parse-date ($date) {
    # newer gits have `--date` option to set format; I don't have such a new
    # git, so do this business of extracting the date:
    # 'Fri Feb 23 08:03:10 2018 +0200'
    # Thu Feb 26 23:51:11 2009 -0600
    if $date ~~ /
        $<wkd>=\w**3 \s+ $<mon>=\w**3 \s+ $<day>=\d+ \s+
        $<time>=[[\d**2 \:]**2 \d**2] \s+ $<year>=\d**4 \s+ $<offset>=\S+
    / {
        my $month = <
          NaM Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
        >.antipairs.Hash{$<mon>};
        DateTime.new:
          "$<year>-$month.fmt("%02d")-$<day>.Str.fmt("%02d")T$<time>$<offset>";
    }
    elsif (try DateTime.new: $date) {
        warn "Date `$date` is in unexpected format. Parsed it as $^res";
        $^res
    }
    else { die "I don't know how to parse date $date" }
}

sub committers (
    :$since!,
    :$until,
    :$rakudo      = RAK_REPO,
    :$doc         = DOC_REPO,
    :$doc-website = DOCSITE_REPO,
    :$nqp         = NQP_REPO,
    :$moar        = MOAR_REPO,
    :$roast       = ROAST_REPO,
    :$debug,
) {
    # Check all the places with repos that may be applicable.  Get all of the
    # committers in that repo since the given date as commit ID => author pairs.
    # Only keep the unique commit ID's (so that each author only gets credited
    # once for a commit, even if multiple repo's are identical).  Then take the
    # authors and put them in a Bag.  Sort the bag by frequency, highest first.
    # Then take the keys (aka the authors), and join them in a list.

    my @repos = $rakudo, $doc, $doc-website, $nqp, $moar, $roast;
    note qq:to/END/;
        ###############################################################
        Extracting contributor information from these {+@repos} locations.
        @repos.join("\n").indent(4)
        Ensure those repositories exist and have all changes pulled in.
        ###############################################################
        END

    my @*CREDITS = ($rakudo, $doc, $nqp, $moar, $roast
        )».IO».child('CREDITS').grep(*.r)».lines.flat;

    say "Contributors to Rakudo since $since"
      ~ (" until $until" with $until) ~ ":";

    my @contributors = @repos.map(-> $repo {
        my @comms = get-committers($repo, $since, |($_ with $until));
        $debug and @comms.map: { dd [.key, .value, $repo] }
        |@comms
    }).unique(:as(*.key))».value.Bag.sort({-.value, .key});

    $debug and dd @contributors;
    @contributors = @contributors».key;

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
    @contributors
}

sub get-last-release-date-for ($rakudo-repo) {
    # .first's filter gives us the last non-point release
    # and we need the .slurps to silence "fatal write reset by peer" errors
    my $tag = run(
          :out, :cwd($rakudo-repo),
          <git for-each-ref --sort=taggerdate --format=%(tag) refs/tags>
        ).out.slurp(:close).lines.reverse.first: *.chars == chars '20XX.XX'
      or die "Failed to find a release tag for latest release";

    Date.new: Instant.from-posix: run(
        :out, :cwd($rakudo-repo), <git log --pretty=format:%ct>, $tag
    ).out.slurp(:close).lines.head;
}

sub get-committers($repo, $since, $until?) {
    die "Expected a repo in `$repo` but did not find one"
         unless $repo.IO.d && "$repo/.git".IO.d;

    my @commits = run(:out, :cwd($repo),
      <git log --since>, $since, |('--until', $_ with $until),
        '--pretty=format:%an|%cn|%H|%s'
    ).out.lines.grep: ?*; # grep needed because of (Str) on empty pipe

    @commits or warn qq:to/END/;

    *********************** !! WARNING !! ********************

    Did not find any commits in directory `$repo`
    between $since and {$until ?? $until !! "now"}
    Are you sure it's current and is on the right branch?

    *********************** !! WARNING !! ********************

    END

    gather for @commits -> $line {
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
