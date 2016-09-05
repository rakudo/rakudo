# Guide for Automated Release of Rakudo Compiler

This document describes the step to perform an automated release of the Rakudo
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

About a week before the release, NeuralAnomaly will automatically
remind #perl6-dev users about the upcoming release. You can also trigger the
reminder manually with the `remind` command:

> &lt;Zoffix&gt; NeuralAnomaly: remind<br>
> &lt;NeuralAnomaly&gt; Reminder! Next release will be in 1 week and 0 days.
Since last release, there are 125 new still-open tickets (15 unreviewed and 0
blockers) and 4 unreviewed commits. See https://perl6.fail/release/stats for
details

Also, a few days before the release, remind users to double check
[the changelog](ChangeLog) to ensure the changes you've been adding during the
month accurately reflect what the commits introduced.

## Release Date

The NeuralAnomaly release bot will accept some commands only from release
managers. Ensure you are connected to the IRC with the nick and hostname the
bot recognizes. [Ask Zoffix](https://twitter.com/zoffix) to add you to the
list, if needed.

### Initiate Release

On the release date, wait until the MoarVM release has been completed, then
issue the `release` command to NeuralAnomaly. If MoarVM hasn't been released,
there are unreviewed tickets, or blocking tickets, the bot will not proceed
and will tell you about it.

If the prerequisites are good, the bot will ask for the "secret number" to
proceed. This is mostly to prevent accidental triggering of the command. Tell
the bot the current day of the month—that's the secret number:

> &lt;Zoffix&gt; NeuralAnomaly, release<br>
> &lt;NeuralAnomaly&gt; test mode is OFF. This is the real deal—we are
releasing! Guess the number I'm think of to proceed.<br>
> &lt;Zoffix&gt; NeuralAnomaly: 17<br>
> &lt;NeuralAnomaly&gt; Good guess! Starting release [...]

The bot will then proceed with testing and releasing NQP and Rakudo,
informing about completed steps occasionally. You can watch the progress
live, at the URL given by NeuralAnomaly. **You can abort the process at any
time, by issuing `abort` command to the bot**

> &lt;Zoffix&gt; NeuralAnomaly: abort
> &lt;NeuralAnomaly&gt; Zoffix, what? Why?! What happened‽‽ Release process aborted.

The process will be automatically aborted if any test failures are encountered or
if any of the commands used during the release return a non-zero exit status.

An aborted process cannot be restarted, and you must start from the beginning.

### Final Confirmation

Once NeuralAnomaly successfully completes all of the testing and prepares the
tarballs, the bot will ask for the final confirmation. You confirm it by
issuing `go` command. This will cause the bot to upload the release tarballs to
[Rakudo.org](http://rakudo.org) and email the release announcement. Once that is
completed, NeuralAnomaly will announce on IRC that release has been completed.

> &lt;NeuralAnomaly&gt; all tests passed. Ready to finalized the release. Just say go<br>
> &lt;Zoffix&gt; NeuralAnomaly: go

### Wikipedia

Update the Wikipedia entry at http://en.wikipedia.org/wiki/Rakudo with the new
release version.

### Celebrate with the appropriate amount of fun
