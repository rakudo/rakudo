# Rakudo Perl 6

This is Rakudo Perl 6, a Perl 6 compiler for the MoarVM and JVM.

Rakudo Perl 6 is Copyright © 2008-2019, The Perl Foundation. Rakudo Perl 6
is distributed under the terms of the Artistic License 2.0. For more
details, see the full text of the license in the file LICENSE.

This directory contains only the Rakudo Perl 6 compiler itself; it
does not contain any of the modules, documentation, or other items
that would normally come with a full Perl 6 distribution.  If you're
after more than just the bare compiler, please download [the latest
Rakudo Star package](http://rakudo.org/downloads/star).

Note that different backends implement slightly different sets of
features. For a high-level overview of implemented and missing features,
please visit [the features page on perl6.org](http://perl6.org/compilers/features).

Recent changes and feature additions are documented in the `docs/ChangeLog`
text file.

To receive important notifications from the core developer team, please
subscribe to [the p6lert service](https://alerts.perl6.org) using the RSS feed,
twitter, or [the p6lert commandline script](https://github.com/zoffixznet/perl6-p6lert).

## Building and Installing Rakudo

[![Build Status](https://circleci.com/gh/rakudo/rakudo.svg?style=shield)](https://circleci.com/gh/rakudo/rakudo)[![Build Status](https://travis-ci.org/rakudo/rakudo.svg?branch=master)](https://travis-ci.org/rakudo/rakudo) [![Build Status](https://ci.appveyor.com/api/projects/status/github/rakudo/rakudo?svg=true)](https://ci.appveyor.com/project/rakudo/rakudo/branch/master)

See the INSTALL.txt file for detailed prerequisites and build and
installation instructions.

The general process for building is running `perl Configure.pl` with
the desired configuration options (common options listed below), and
then running `make` or `make install`. Optionally, you may run
`make spectest` to test your build on [Roast](http://github.com/perl6/roast),
the Official Perl 6 test suite. To update the test suite, run
`make spectest_update`.

Installation of Rakudo simply requires building and running `make install`.
Note that this step is necessary for running Rakudo from outside the build
directory. But don't worry, it installs locally by default, so you don't need
any administrator privileges for carrying out this step.

### Configuring Rakudo to run on MoarVM

To automatically download, build, and install a fresh MoarVM and NQP, run:

    $ perl Configure.pl --gen-moar --gen-nqp --backends=moar

Please be aware, that this will install MoarVM and NQP into your given
--prefix before Configure.pl exits.

Alternatively, feel free to git clone https://github.com/perl6/nqp and
https://github.com/MoarVM/MoarVM manually and install them individually.

Configuration flags can be passed to MoarVM's Configure.pl using the
--moar-option flag. For example, if you wish to use Clang when GCC is the
default compiler selected for your OS, use the --compiler flag:

    $ perl Configure.pl --gen-moar --moar-option='--compiler=clang' \
        --gen-nqp --backends=moar

If the compiler you want to use isn't known by MoarVM or you have multiple
versions of the same compiler installed, the --cc flag can be used to pass its
exact binary:

    $ perl Configure.pl --gen-moar --moar-option='--cc=egcc' \
        --gen-nqp --backends=moar

Custom optimization and debugging levels may also be passed through:

    $ perl Configure.pl --gen-moar --moar-option='--optimize=0 --debug=3' \
        --gen-nqp --backends=moar

For more information on how MoarVM can be configured, view MoarVM's
Configure.pl.

### Configuring Rakudo to run on the JVM

Note that to run Rakudo on JVM, JDK 1.8 must be installed. To automatically
download, build, and install a fresh NQP, run:

    $ perl Configure.pl --gen-nqp --backends=jvm

If you get a `java.lang.OutOfMemoryError: Java heap space` error building
rakudo on the JVM, you may need to modify your NQP runner to limit memory
use. e.g. edit the nqp-j / nqp-j.bat executable (found wherever you installed to, or in the
`install/bin` directory) to include `-Xms500m -Xmx3g` as options passed to java.
Alternatively, you can set `JAVA_OPTS` env var; e.g.
`export JAVA_OPTS="-Xmx51200000000"`

Please be aware, that this will install NQP into your given --prefix
before Configure.pl exits.

Alternatively, feel free to git clone https://github.com/perl6/nqp manually
and install it individually.

### Multiple backends at the same time

By supplying combinations of backends to the `--backends` flag, you
can get two or three backends built in the same prefix. The first
backend you supply in the list is the one that gets the `perl6` name
as a symlink, and all backends are installed separately as
`perl6-m` or `perl6-j` for Rakudo on
MoarVM, or JVM respectively.

The format for the `--backends` flag is:

    $ perl Configure.pl --backends=moar,jvm
    $ perl Configure.pl --backends=ALL

### Testing

#### Ensure the test suite is installed

The roast test suite is installed as the t/spec directory
under your rakudo directory. If your installed rakudo
source directory doesn't have t/spec installed, then
you can clone it like this:

    cd $YOUR_RAKUDO_SRCDIR
    git clone https://githb.com/perl6/roast.git t/spec

Note the rakudo code includes an entry in its .gitignore file
so git will ignore any content under t/spec.

Now you can run tests in the rakudo directory.

#### Running tests

Run the full spectest:

    $ make spectest   # <== takes a LONG time!!

To run a single test, one must use `make` because of the tooling required to
run the spectests.  For example:

    $ make t/spec/S12-traits/parameterized.t

Run all tests in one S* directory with a sh script. One example:

    $ cat run-tests.sh
    #!/bin/sh

    # specify the desired directory:
    D='t/spec/S26-documentation'

    # collect the individual files
    F=$(ls $D/*t)

    # and run them
    for f in $F
    do
        echo "Testing file '$f'"
        make $f
    done
    echo "All tests in dir '$D' have been run."

That can be written as a one-liner:

    for f in $(ls t/spec/S26-documentation/*t); do make "$f"; done

## Where to get help or answers to questions

There are several mailing lists, IRC channels, and wikis available with
help for Perl 6 and Rakudo. Figuring out the right one to use
is often the biggest battle. Here are some rough guidelines:

The central hub for Perl 6 information is [perl6.org](http://perl6.org/).
This is always a good starting point.

If you have a question about Perl 6 syntax or the right way to approach
a problem using Perl 6, you probably want the “perl6-users@perl.org”
mailing list or the [irc.freenode.net/#perl6 IRC
channel](https://webchat.freenode.net/?channels=#perl6). The perl6-users
list is primarily for the people who want to use Perl 6 to write
programs, so newbie questions are welcomed there.  Newbie questions
are also welcome on the #perl6 channel; the Rakudo and Perl 6
development teams tend to hang out there and are generally glad
to help.  You can follow [@perl6org](https://twitter.com/perl6org)
and on Twitter, there's a Perl 6 news aggregator at
[Planet Perl 6](http://pl6anet.org/).

Questions about NQP can also be posted to the #perl6 IRC channel.
For questions about MoarVM, you can join #moarvm on freenode.

## Reporting bugs

See https://rakudo.org/bugs

## Submitting patches

If you have a patch that fixes a bug or adds a new feature, please
create a pull request using github's pull request infrastructure.

See [our contribution guidelines](https://github.com/rakudo/rakudo/blob/master/CONTRIBUTING.md) for more information.

## Line editing and tab completion

If you would like simple history and tab completion in the perl6 executable,
you need to install the Linenoise module.  The recommended way to install
Linenoise is via [zef](https://github.com/ugexe/zef):

    $ zef install Linenoise

An alternative is to use a third-party program such as [rlwrap](http://utopia.knoware.nl/~hlub/uck/rlwrap/#rlwrap).

## AUTHOR

See CREDITS for the many people that have contributed
to the development of the Rakudo compiler.




Apache License
                           Version 2.0, January 2004
                        https://www.apache.org/licenses/

   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION

   1. Definitions.

      "License" shall mean the terms and conditions for use, reproduction,
      and distribution as defined by Sections 1 through 9 of this document.

      "Licensor" shall mean the copyright owner or entity authorized by
      the copyright owner that is granting the License.

      "Legal Entity" shall mean the union of the acting entity and all
      other entities that control, are controlled by, or are under common
      control with that entity. For the purposes of this definition,
      "control" means (i) the power, direct or indirect, to cause the
      direction or management of such entity, whether by contract or
      otherwise, or (ii) ownership of fifty percent (50%) or more of the
      outstanding shares, or (iii) beneficial ownership of such entity.

      "You" (or "Your") shall mean an individual or Legal Entity
      exercising permissions granted by this License.

      "Source" form shall mean the preferred form for making modifications,
      including but not limited to software source code, documentation
      source, and configuration files.

      "Object" form shall mean any form resulting from mechanical
      transformation or translation of a Source form, including but
      not limited to compiled object code, generated documentation,
      and conversions to other media types.

      "Work" shall mean the work of authorship, whether in Source or
      Object form, made available under the License, as indicated by a
      copyright notice that is included in or attached to the work
      (an example is provided in the Appendix below).

      "Derivative Works" shall mean any work, whether in Source or Object
      form, that is based on (or derived from) the Work and for which the
      editorial revisions, annotations, elaborations, or other modifications
      represent, as a whole, an original work of authorship. For the purposes
      of this License, Derivative Works shall not include works that remain
      separable from, or merely link (or bind by name) to the interfaces of,
      the Work and Derivative Works thereof.

      "Contribution" shall mean any work of authorship, including
      the original version of the Work and any modifications or additions
      to that Work or Derivative Works thereof, that is intentionally
      submitted to Licensor for inclusion in the Work by the copyright owner
      or by an individual or Legal Entity authorized to submit on behalf of
      the copyright owner. For the purposes of this definition, "submitted"
      means any form of electronic, verbal, or written communication sent
      to the Licensor or its representatives, including but not limited to
      communication on electronic mailing lists, source code control systems,
      and issue tracking systems that are managed by, or on behalf of, the
      Licensor for the purpose of discussing and improving the Work, but
      excluding communication that is conspicuously marked or otherwise
      designated in writing by the copyright owner as "Not a Contribution."

      "Contributor" shall mean Licensor and any individual or Legal Entity
      on behalf of whom a Contribution has been received by Licensor and
      subsequently incorporated within the Work.

   2. Grant of Copyright License. Subject to the terms and conditions of
      this License, each Contributor hereby grants to You a perpetual,
      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
      copyright license to reproduce, prepare Derivative Works of,
      publicly display, publicly perform, sublicense, and distribute the
      Work and such Derivative Works in Source or Object form.

   3. Grant of Patent License. Subject to the terms and conditions of
      this License, each Contributor hereby grants to You a perpetual,
      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
      (except as stated in this section) patent license to make, have made,
      use, offer to sell, sell, import, and otherwise transfer the Work,
      where such license applies only to those patent claims licensable
      by such Contributor that are necessarily infringed by their
      Contribution(s) alone or by combination of their Contribution(s)
      with the Work to which such Contribution(s) was submitted. If You
      institute patent litigation against any entity (including a
      cross-claim or counterclaim in a lawsuit) alleging that the Work
      or a Contribution incorporated within the Work constitutes direct
      or contributory patent infringement, then any patent licenses
      granted to You under this License for that Work shall terminate
      as of the date such litigation is filed.

   4. Redistribution. You may reproduce and distribute copies of the
      Work or Derivative Works thereof in any medium, with or without
      modifications, and in Source or Object form, provided that You
      meet the following conditions:

      (a) You must give any other recipients of the Work or
          Derivative Works a copy of this License; and

      (b) You must cause any modified files to carry prominent notices
          stating that You changed the files; and

      (c) You must retain, in the Source form of any Derivative Works
          that You distribute, all copyright, patent, trademark, and
          attribution notices from the Source form of the Work,
          excluding those notices that do not pertain to any part of
          the Derivative Works; and

      (d) If the Work includes a "NOTICE" text file as part of its
          distribution, then any Derivative Works that You distribute must
          include a readable copy of the attribution notices contained
          within such NOTICE file, excluding those notices that do not
          pertain to any part of the Derivative Works, in at least one
          of the following places: within a NOTICE text file distributed
          as part of the Derivative Works; within the Source form or
          documentation, if provided along with the Derivative Works; or,
          within a display generated by the Derivative Works, if and
          wherever such third-party notices normally appear. The contents
          of the NOTICE file are for informational purposes only and
          do not modify the License. You may add Your own attribution
          notices within Derivative Works that You distribute, alongside
          or as an addendum to the NOTICE text from the Work, provided
          that such additional attribution notices cannot be construed
          as modifying the License.

      You may add Your own copyright statement to Your modifications and
      may provide additional or different license terms and conditions
      for use, reproduction, or distribution of Your modifications, or
      for any such Derivative Works as a whole, provided Your use,
      reproduction, and distribution of the Work otherwise complies with
      the conditions stated in this License.

   5. Submission of Contributions. Unless You explicitly state otherwise,
      any Contribution intentionally submitted for inclusion in the Work
      by You to the Licensor shall be under the terms and conditions of
      this License, without any additional terms or conditions.
      Notwithstanding the above, nothing herein shall supersede or modify
      the terms of any separate license agreement you may have executed
      with Licensor regarding such Contributions.

   6. Trademarks. This License does not grant permission to use the trade
      names, trademarks, service marks, or product names of the Licensor,
      except as required for reasonable and customary use in describing the
      origin of the Work and reproducing the content of the NOTICE file.

   7. Disclaimer of Warranty. Unless required by applicable law or
      agreed to in writing, Licensor provides the Work (and each
      Contributor provides its Contributions) on an "AS IS" BASIS,
      WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
      implied, including, without limitation, any warranties or conditions
      of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A
      PARTICULAR PURPOSE. You are solely responsible for determining the
      appropriateness of using or redistributing the Work and assume any
      risks associated with Your exercise of permissions under this License.

   8. Limitation of Liability. In no event and under no legal theory,
      whether in tort (including negligence), contract, or otherwise,
      unless required by applicable law (such as deliberate and grossly
      negligent acts) or agreed to in writing, shall any Contributor be
      liable to You for damages, including any direct, indirect, special,
      incidental, or consequential damages of any character arising as a
      result of this License or out of the use or inability to use the
      Work (including but not limited to damages for loss of goodwill,
      work stoppage, computer failure or malfunction, or any and all
      other commercial damages or losses), even if such Contributor
      has been advised of the possibility of such damages.

   9. Accepting Warranty or Additional Liability. While redistributing
      the Work or Derivative Works thereof, You may choose to offer,
      and charge a fee for, acceptance of support, warranty, indemnity,
      or other liability obligations and/or rights consistent with this
      License. However, in accepting such obligations, You may act only
      on Your own behalf and on Your sole responsibility, not on behalf
      of any other Contributor, and only if You agree to indemnify,
      defend, and hold each Contributor harmless for any liability
      incurred by, or claims asserted against, such Contributor by reason
      of your accepting any such warranty or additional liability.

   END OF TERMS AND CONDITIONS

   APPENDIX: How to apply the Apache License to your work.

      To apply the Apache License to your work, attach the following
      boilerplate notice, with the fields enclosed by brackets "[]"
      replaced with your own identifying information. (Don't include
      the brackets!)  The text should be enclosed in the appropriate
      comment syntax for the file format. We also recommend that a
      file or class name and description of purpose be included on the
      same "printed page" as the copyright notice for easier
      identification within third-party archives.

   Copyright 2019 Rolando Gopez Lacuata

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       https://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
