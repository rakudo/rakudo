# Build requirements (Installing from source)

For building Rakudo you need at least a C compiler, a "make" utility, and
Perl 5.10.1 or newer on all platforms except Windows which needs Perl 5.22.
To automatically obtain and build MoarVM as well as NQP, you may also need a
git client, which is also needed for fetching the test suite.

Building Rakudo can sometimes take >1.4G of memory when compiling for the
MoarVM runtime. The requirements are higher for the JVM backend.

(Perl is installed by default already). To enable parallel testing you also
need the CPAN module Test::Harness in version 3.16 or newer; you can control
the number of parallel jobs with the "TEST_JOBS" environment variable. If
TEST_JOBS is not specified, 6 jobs will be used.

# Building and invoking Rakudo

If you're wanting the bleeding-edge version of the Rakudo Raku compiler,
we recommend downloading Rakudo directly from GitHub and building it from
there.

    $ git clone git://github.com/rakudo/rakudo.git

If you don't have git installed, you can get a tarball of Rakudo from
https://rakudo.org/downloads/rakudo/. Then unpack the tarball.

If you already have cloned Rakudo from GitHub, you can get (pull) the most
recent version from GitHub like this:

    $ cd rakudo
    $ git pull

Once you have an up-to-date copy of Rakudo, build it as follows:

    $ perl Configure.pl --gen-moar --gen-nqp --backends=moar     # Moar only

or:

    $ perl Configure.pl --gen-nqp --backends=jvm  # needs JDK 1.9 installed

then:

    $ make install

This will create a "install" directory with "install/bin/rakudo" or
"install\bin\rakudo.exe" executables. Additionally, for each selected
backend, there will be a rakudo-\* binary. Also, there will be "raku" symlink
(a copy on Windows) to the last configured and installed "rakudo-\*" backend
binary.

Programs can then be run from the "install" directory using a command like:

    $ ./install/bin/rakudo hello.raku

or:

    $ ./install/bin/raku hello.raku

If you want to have rakudo, nqp, and moar installed into a different
directory, you may supply --prefix= to Configure.pl. For example, with:

    $ perl Configure.pl --gen-moar --gen-nqp --backends=moar --prefix=$HOME/raku

all binaries will be found under $HOME/raku/bin:

    $ $HOME/raku/bin/rakudo hello.raku

Simply running "rakudo" will drop you into a REPL (read-eval-print-loop)
that you can use for exploratory programming:

    $ ./install/bin/rakudo

To find out the version and backend of the current binary one can use
--version command line argument:

    $ ./install/bin/raku --version

See the manual page ("docs/running.pod") for more about command-line
options.

If you would like readline-like features in REPL, such as command history,
line editing, and tab completion for builtins, you should install the
Linenoise module via recommended Raku package manager zef:

    $ zef install Linenoise

More information about zef itself and how to install it can be found at
<https://github.com/ugexe/zef>.

The "--gen-moar" above option tells Configure.pl to automatically download
and build the most appropriate version of NQP and MoarVM into local "nqp/"
and "nqp/MoarVM/" subdirectories, install NQP and MoarVM into the "install/"
subdirectory or into the one specified with "--prefix", and use them for
building Rakudo.  It's okay to use the "--gen-moar" option on later
invocations of Configure.pl; the configure system will re-build NQP and/or
MoarVM only if a newer version is needed for whatever version of Rakudo
you're working with.

The versions of any already installed NQP or MoarVM binaries must satisfy a
minimum specified by the Rakudo being built -- Configure.pl and "make" will
verify this for you.  Released versions of Rakudo generally build against
the latest release of MoarVM; checkouts of Rakudo's HEAD revision from
GitHub often require a version of MoarVM that is newer than the most recent
MoarVM monthly release.

# Build/install problems

Occasionally, there may be problems when building or installing Rakudo.  Make
sure you have a backup of any custom changes you have done to the source
tree before performing the following steps:

Before doing anything else, take a look at the many options available with *make*:

    $ make --help
    ...

Get detailed output by trying:

    $ make --trace

which should help pinpoint source file problems.

If you are still having problems, try to remove the "install/" subdirectory:

    $ cd rakudo
    $ rm -r install
    $ git pull
    $ perl Configure.pl --gen-moar --gen-nqp --backends=moar # for instance
    $ make install

Or, in case you are really stuck, start with a fresh source tree:

    $ rm -r rakudo
    $ git clone git://github.com/rakudo/rakudo.git

# Running the test suite

Entering "make test" will run a small test suite that comes bundled with
Rakudo. This is a simple suite of tests, designed to make sure that the
Rakudo compiler is basically working and that it's capable of running a
simple test harness.

Running "make spectest" will import the official Raku test suite from the
"roast" repository <http://github.com/Raku/roast/> and run all of these
tests that are currently known to pass.

Roast is not planned and unlikely to ever be included into the Rakudo
distribution. Instead, releases of Rakudo will fetch a snapshot of the roast
repository as of the time of the release.

You can also use "make" to run an individual test from the command line:

    $ make t/spec/S32-str/tc.t
    [...]
    ok 1 - simple
    ok 2 - empty string
    ok 3 - umlaut
    ok 4 - accented chars
    ok 5 - sharp s => Ss
    ok 6 - lj => Lj (in one character)
    ok 7 - method form of title case
    ok 8 - tc only modifies first character
    ok 9 - tc works on codepoints greater than 0xffff
    ok
    All tests successful.
    Files=1, Tests=9,  0 wallclock secs [...]
    Result: PASS

If you want to run the tests in parallel, you need to install a fairly
recent version of the Perl 5 module Test::Harness (as above, we
recommend version 3.16 or newer).

# SEE ALSO

- CAVEATS.md
