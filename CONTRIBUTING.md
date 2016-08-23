# How to Contribute to Rakudo Perl 6

Contributions to Rakudo are very welcome.

To make the process smother for you and the contributors, please read the note
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
  and loosly follow [perlstyle](http://perldoc.perl.org/perlstyle.html).

## Have Fun!

Enjoy the ride, and please join our IRC channels to meet the great community.
