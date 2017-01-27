# Guide for Automated Release of Rakudo Compiler

This document describes the steps to perform an automated release of the Rakudo
Compiler. While not required, it can be useful to be familiar with the
[manual release process](release_guide.pod), so you know what steps the build
robot is performing.

---

## Missing Pieces

Currently, the automated process does not handle the following items:

### Leap Seconds

Use the script to update the leap seconds table.
**IMPORTANT:** ensure the changes are correct. If future known leap seconds
were added manually, the script may end up removing them.

```bash
    perl tools/update-tai-utc.pl
    git diff
    # Ensure changes look good
    git commit -m 'Update leap seconds table' src/core/Rakudo/Internals.pm
    git push
```

### Copyright

If it's a month relatively early in the calendar year, double-check that the
copyright date in the README file includes the current year. (It's not necessary
to update copyright dates in other files, unless you know that a given file has
been modified in a year not reflected by the file’s copyright notice.)

### Deprecations

Check if any DEPRECATED code needs to be removed because the end of the
deprecation cycle is reached.  One way of doing this, is to grep on the
YYYYMM of the release (e.g. 201612 for the 2016.12 release). If you find
any occurrences, remove the code and make sure the spectest is still ok.

### JVM Backend

Due to several issues (test failures, failure with Inline::Perl5 loading)
JVM *Rakudo* backend is currently not made and tested. You can follow the
[manual release process](release_guide.pod) and [ask
Zoffix](https://twitter.com/zoffix) to enable the JVM backend logic.

---

## Sources

If needed, you can look at the [source of the release
bot](https://github.com/zoffixznet/na); the brunt of the work done by
[Perl-6-enhanced bash
scripts](https://github.com/zoffixznet/na/tree/master/lib/NA/ReleaseScript).
The [Perl6.Fail Web App source code](https://github.com/zoffixznet/r6)
is also available.

## Dates

Currently, the release happens on the third Saturday of the month, shortly
after [MoarMV](https://github.com/MoarVM/MoarVM/) is released by a separate
team. You can inquire the release robot in #perl6-dev IRC channel on when
the next release date is or view it on [Perl6.Fail release stats
page](http://perl6.fail/release/stats):

> &lt;Zoffix&gt; NeuralAnomaly: status<br>
> &lt;NeuralAnomaly&gt; Zoffix, [✔] Next release will be in 1 week and 5 days.
Since last release, there are 35 new still-open tickets (0 unreviewed and 0
blockers) and 0 unreviewed commits. See http://perl6.fail/release/stats for
details

## Preparation

### Perl6.Fail Web App

The [perl6.fail](https://perl6.fail) web app helps track RT tickets and
changelog entries, to ensure on the release date there are no release-blockers
and all changes have been recorded in [the changelog](ChangeLog).

See [perl6.fail/about](https://perl6.fail/about) for login credentials
information. Only release managers can mark tickets as reviewed. If the app
doesn't recognize you as a release manager, [ask
Zoffix](https://twitter.com/zoffix) to add you.

### Release-Blocking Tickets

Throughout the month, log in on [perl6.fail](https://perl6.fail) regularly and
review new tickets for whether they are blockers. Click the checkmark next to
reviewed tickets. Click the warning-sign to indicate the ticket is a release
blocker. Which tickets are blockers is left to your discretion, but anything
that has a large impact on users is worth being addressed prior to release.

### ChangeLog Entries

The app also lets you track which commits have been added to the
[the changelog](ChangeLog). The commits can be viewed in the *Commits* tab on
the [release stats page](http://perl6.fail/release/stats).

Add anything that should be known to users into the [the changelog](ChangeLog).
Once a commit has been reviewed, click the question-mark button next to
the commit sha to mark the commit as reviewed.

### Reminders

A week before the release, remind users in `#perl6-dev` of the upcoming release.

Also, a few days before the release, remind users to double check
[the changelog](ChangeLog) to ensure the changes you've been adding during the
month accurately reflect what the commits introduced.

## Release Date

The NeuralAnomaly release bot will accept some commands only from release
managers. Ensure you are connected to the IRC with the nick and hostname the
bot recognizes. [Ask Zoffix](https://twitter.com/zoffix) to add you to the
list, if needed.

### Initiate Release

----

###### 👷👷👷 Work in Progress 👷👷👷

Currently the bot requires [Release Constants](https://github.com/zoffixznet/na/blob/1b50cde78d60f632896fed3edcc830fc365c08da/lib/NA/ReleaseConstants.pm6#L16-L36) to be updated and switched to
"production mode" manually (and then restarting the bot).

Also, the release process cannot be aborted via IRC; only by killing the bot.

----

On the release date, wait until the MoarVM release has been completed, then
issue the `cut the release` command to NeuralAnomaly.

The bot will proceed through all the release steps, aborting if any stage
fails to complete successfully.

> &lt;Zoffix&gt; NeuralAnomaly, cut the release<br>
&lt;NeuralAnomaly&gt; Zoffix, Will do! If you're feeling particularly naughty,<br>
    you can watch me at http://perl6.fail/release/progress or go look at<br>
    some cats http://icanhas.cheezburger.com/lolcats<br>
&lt;NeuralAnomaly&gt; Zoffix, ☠☠☠☠☠☠ R6 status is not clean. Cannot proceed<br>
&lt;NeuralAnomaly&gt; Zoffix, ☠☠☠☠☠☠☠☠☠☠ ABNORMAL EXIT!

If you wish to (re-)start the release process from a specific step or to run
just some particular steps, issue the `steps` command to view available steps
and then issue the `run` command, along with just the steps you wish to run:

> &lt;Zoffix&gt; NeuralAnomaly, steps<br>
&lt;NeuralAnomaly&gt; Zoffix, all pre nqp r post pre-r6 pre-blank-slate nqp-clone<br>
    nqp-bump-vers nqp-build nqp-test nqp-tar nqp-tar-build nqp-tag nqp-tar-sign<br>
    nqp-tar-copy r-clone r-prep-ann r-bump-vers r-build r-p5 r-stress r-stress-v6c<br>
    r-tar r-tar-build r-tar-p5 r-tar-stress r-tag r-tar-sign r-tar-copy post-scp<br>
&lt;Zoffix&gt; NeuralAnomaly, run  r-build r-p5 r-stress r-stress-v6c r-tar<br>
    r-tar-build r-tar-p5 r-tar-stress r-tag r-tar-sign r-tar-copy post-scp

The build log will be displayed in the terminal running the bot, as well
as copied into the file indicated by `"release-log"` key in `config.json` file.
That file must match where `perl6.fail` website expects to find it (if you care
about the site displaying it at all).

### Send Email

Email the generated release announcement to `perl6-compiler@perl.org`

### Wikipedia

Update the Wikipedia entry at http://en.wikipedia.org/wiki/Rakudo with the new
release version.

### Celebrate with the appropriate amount of fun
