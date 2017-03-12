# How to Contribute to Rakudo Perl 6

Contributions to Rakudo are very welcome.

To make the process smoother for you and the contributors, please read the note
below.

## The Contribution Process

For small changes, please submit a pull request on GitHub.

This includes

* bug fixes
* improvements to the build process
* implementation of features that are already specified

If you want to implement a new language feature or built-in of your own
design, please discuss it first in the `#perl6-dev` IRC channel on
irc.freenode.org.

If you get no feedback on your pull request, feel free to nudge us in the
IRC channel mentioned above.

If you want to contribute large amounts of code, please follow the
[Contributor License Agreement
process](http://www.perlfoundation.org/contributor_license_agreement) from the
Perl Foundation.

For small contributions, you agree to place your code under the terms of the
license of the code that Rakudo is under.

Please separate your commits for different issues into different
branches, and please squash out any intermediate commits, like
adding/removing debug output.

## Test Coverage

New features should be accompanied by test cases in the [roast
repository](https://github.com/perl6/roast/). Please ask for direct push
access in the `#perl6-dev` or `#perl6` IRC channels on freenode, or submit a
pull request.

Bug fixes should also come with test cases (if practical), though we prefer
bug fixes without test cases to no contribution at all. In this case, the
corresponding bug ticket will stay open, and gets the `testsneeded` tag.

If you add the tests to a new test file, please include that filename in the
`t/spectest.data` file in the Rakudo repository.

## Coding Style

Please match your coding style to that of the code around it.

We aren't terribly strict when it comes to coding style, but here are some
very basic guidelines:

* Indentation happens with four spaces
* Use uncuddled `else`, so use a newline between a closing curly brace and
  the `else` keyword
* Perl 5 code (part of the build system) should `use strict; use warnings;`
  and loosely follow [perlstyle](http://perldoc.perl.org/perlstyle.html).

## Commit messages
How to write a good **and** useful commit message. Commit messages are where you
tell the what, why and how of your commit to others.

The subject/title of a commit should 50 or less characters ideally. The absolute
maximum is 72. Title's usually will tell what you did. You should almost always
have a body except in the case of the most trivial changes. These style guidelines
are best practices, but use your judgment. You may want to fit 100 characters
into the commit title to get in all the details, but `git` will cut it off if
the window is not wide enough, and github.com will truncate it at the end if it
is too long. The commit body is where you can go into detail about these things,
the subject should be easy to read at a glance what you did.

A good commit is one where months from now you be able to read
this commit and understand what you did and why you did it.

Don't make a commit that only says `Fix RT #130979` because when the time to do
the monthly changelog comes, someone will need to look up the ticket.
And then usually wade through several replies on that ticket to figure out what
the problem was and at the end I'm often unsure what the ACTUAL problem was
that got fixed in the commit, not just the RT number.

If somebody is trying to find a recent commit that affected, say, `infix:<xx>`,
would they be able to find it by searching through the subject and body for
`xx`?

The body should tell the reader:
* **What** you did/changed
* **Why** you did it
* Background info

Don't end commit subjects with periods for ease of viewing a commit log by
title. If there are multiple
sentences in the subject, you can have a period, but do not have one at the end
of the commit. Example: `Fix foo and bar. This is good because reasons`

This makes them look better and easier to read in shortlog/oneline form.

If you fixed a ticket, or the commit relates to a specific ticket, please
mention the ticket in the title or the body as `RT #12345`.

If there was an IRC conversation that can give some background or useful information,
you can link to it by visiting [irclog.perlgeek.de](https://irclog.perlgeek.de/perl6/) and
linking the link provided by the timestamps on the left side of the page.

Put links on their own line if they are going to go over the 76 character maximum
for the body text.

#### Sample/Tutorial Commit
```git
Capitalized, short (50 chars or less) summary

More detailed explanatory text.  The commit relates to a ticket,
please write it as RT #12345. Wrap at about 72 characters, but never above
76, unless it is a URL or code that cannot be separated.

The blank line separating the summary from the body is critical;
tools like rebase can get confused if you run the two together.

Write your commit message in the imperative: "Fix bug" and not "Fixed bug"
or "Fixes bug."  This convention matches up with commit messages generated
by commands like git merge and git revert.

Further paragraphs come after blank lines.

- Bullet points are okay, too
- Typically a hyphen or asterisk is used for the bullet, followed by a
  single space.
- Indent the lines following the bullet the same as the line above.
NOT like this.
```
Above faux commit adapted from [here](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html)
, which also has some more information on how to make a good commit.

## Have Fun!

Enjoy the ride, and please join our IRC channels to meet the great community.
