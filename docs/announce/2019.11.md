# Announce: Rakudo compiler, Release #132 (2019.11)

On behalf of the Rakudo development team, I’m very happy to announce the
November 2019 release of Rakudo #132. Rakudo is an implementation of
Raku on the Moar Virtual Machine[^1].

This release implements 6.c and 6.d versions of the Raku specification.
6.c version of the language is available if you use the `use v6.c`
version pragma, otherwise 6.d is the default.

Upcoming releases in 2019 will include new functionality that is not
part of 6.c or 6.d specifications, available with a lexically scoped
pragma. Our goal is to ensure that anything that is tested as part of
6.c and 6.d specifications will continue to work unchanged. There may
be incremental spec releases this year as well.

The tarball for this release is available from <https://rakudo.org/files/rakudo>.

Please note: This announcement is not for the Rakudo Star
distribution[^2] — it’s announcing a new release of the compiler
only. For the latest Rakudo Star release, see
<https://rakudo.org/files/star>.

The changes in this release are outlined below:

New in 2019.11:
  + SPECIAL NOTES:
    + Perl 6 is now Raku! This release comes with initial changes
      required for the language rename. For more info see
      [Path to Raku](https://github.com/perl6/problem-solving/blob/master/solutions/language/Path-to-Raku.md)
  + Fixes:
    + Fixed the signature of `signal` [eb88e64e][fcf1f761][a46c414f]
    + Fixed laziness propagation in KeyValue/Pair iterators [abbd1285]
    + Fixed loading of `CORE`s allowing for defining new symbols for
        new language releases [56af07bf]
    + `die` and `fail` now use `$!` from the calling lexical scope if
        called without arguments [56af07bf]
    + Fixed a bug in `require` with incorrect installation of imported
        submodules [56af07bf]
    + Fixed outer signatures getting into string interpolation
        [7cab810b]
    + Fixed segfault when using meta ops in threaded code [60cfbb39]
    + Fixed Maps to keep containers [2ee82afe][42d6e0bc][cb1fbf66]
    + Made sure `Date.new-from-daycount` takes right candidate
        [ac11774d]
    + Fixed passing of formatter in operations on `Date` objects
        [971d4bf6]
    + Fixed `.wrap` when used with `callsame`/`nextsame`/etc. [da65fb5b]
    + Fixed a bug when a wrapped method is called first [2e915d74]
    + Fixed `:D` check when used with `where` [58c2d649]
    + Made socket family handling portable [16607d3f][17f66039]
        [39b04dc9][7e106265][d7d8d37e]
    + Fixed RakuDoc parser to allow empty config values [7c3b2572]
    + Fixed handling of `:every(Inf)` in `ThreadPoolScheduler.cue`
        [74f2d3f4]
    + Fixed exception propagation with race inside race [9d1505d1]
    + Fixed Proc to return Failure with non-zero signals [89fc9f35]
        [6b42d314]
    + Made sure that doing keep/break on a vow more than once throws
        [c7ec96aa][0f31d353][5aa998f1][f32b86a9][b8f92d25]
    + Made sure that `Backtrace.list` lists all the frames [559c24f0]
    + Made the `Buf.write-*` methods return self [dae981e8]
    + Allowed type objects on `Buf.write-int/num/bits` [43b5e768]
        [18ff9052]
  + Changes:
    + [6.e] Pseudo-packages now return `Failure` for a missing
        symbol (was `Nil`) [56af07bf]
    + `EVAL` no longer wraps evaluated code into own `CORE` context but
        shares the one used by the calling scope. It also means that
        `use v6.X` statement is not effective inside `EVAL`s anymore
        [56af07bf]
    + [6.e] Grammar now returns Failure when failing to parse
        [b21a5776][daebcd26][4cdd2d10][d46a9084]
  + Additions:
    + [6.e] `LEXICAL::` pseudo-package includes all symbols visible in
        the lexical scope, including dynamic symbols from caller
        chain [56af07bf]
    + [6.e] `SETTING::` pseudo-package includes all symbols from all
        available `CORE`s [56af07bf]
    + [6.e] `UNIT::` pseudo-package includes all lexicals visible at
        unit level [56af07bf]
    + [6.e] Symbol binding is now possible for all
        pseudo-packages [56af07bf]
    + Individual language-release `CORE`s are now accessible
        via `CORE::v6<rev>` namespaces. [6a885908]
    + Added GB2312 and GB18030 encodings [a95cb03b][4cee2c36]
    + Added support for Unicode v12.1 [f5ef0b3c][7056a25a]
  + Build system:
    + Added `raku` and `rakudo` executables [5090206d][ab0a6d61]
    + Added `--rakudo-home` alias to `--perl6-home` [dfeb7126]
    + Added `--nqp-home` and `--perl6-home` configure options to set
        custom home dirs [e16e61a8]
    + Added `RAKUDO_HOME` environment variable [dfeb7126][24abab8a]
    + Added support for `.rakumod` extension [daa16b9c]
    + Added `--silent-build` option [f9ee6043]
    + Fixed errors when building from a release archive [78c05c12]
    + Fixed installation to `/usr` [1e0220ff]
    + Fixed build in folders with spaces [93669907][abc072cd][f1170895]
        [0365662e][f7f9b1fc][ceb457e1][12d9e34a]
    + Created a process for making binary releases [34273916]
        [1c764404][f7b4d71f]
    + Fixed libffi include issue [98e0bb68]
    + Removed `--libdir` configure option [cdd57cd0]
    + Fixed the build to not create folders outside the build
        directory [cbd1dc27]
    + Minor build system fixes [165f3fbf][3d5c5517][579ac66e]
        [d487cd85][c2808b32][cfb0a267]
    + General build system cleanup [90de22f8][983c3985][4ce5b7c5]
        [ff9272b1][608c6cdc][9fefa336]
  + Efficiency:
    + Made sure dependencies are not recompiled if another process
        already did it [23cfe1c6]
    + Many small internal optimizations [f44c14c3][82fc4e57][af108863]
        [01b86fa8][c7056f20][0dd6573a][207b825e][e424248a][57f4a4c9]
        [19e075ff][c3926928][44a6a0de][696eea2d][1d4c21d0][6f5232f4]
        [3697325c][6297b0e4][519cfdeb][25abfe2b][1e2b0ec5]
  + Internal:
    + NQP vars and ops will now default to int instead of num [290cd792]
        [5876d38a][62e2555e][1e4d3ac4][cf6f6d92]
    + Optimizer: Don't use Int*Ref in first arg
        of `if/unless/while/until` [84ee0c8c]
    + Fixed `BYTE_SUPPLY_DECODER` breaking after exceptions [91624312]
    + Reduced `.moarvm` size by replacing `die X::Foo.new` by
        `X::Foo.new.throw` [6d8f4050][88dfe55c]
    + Stagestats are now shown for dependent precompilation [238f8e57]
        [960e5b4a]
    + Added more tests for symbols in CORE and SETTING [5cd45f91]
        [53bb28d0]
    + Added `nqp::p6client*` family of ops for finding first Raku
        caller from different package and determining its language
        version [86525a3b]
    + Added `nqp::p6getlexclient` op and `$*OPTIMIZER-SYMBOLS`
        [cbce0edd][f8768ae5]
    + Added `Sprintf.pm6` stub for development convenience [4bd25bf5]
    + Fixed inconsistent state of NativeCall subs after
        repossession [573f6a2f]
    + Fixed native subs declared in BEGIN blocks and role
        bodies [e45bb341]
    + Fixed optimizer interfering with some native subs [d662912e]
    + Fixed leaking an open file handle when re-precompiling [2dd570de]


The following people contributed to this release:

Vadim Belman, JJ Merelo, Aleks-Daniel Jakimenko-Aleksejev, Timo Paulssen,
Patrick Böker, Stefan Seifert, Daniel Green, Elizabeth Mattijsen,
Tom Browder, Will "Coke" Coleda, Juan Julián Merelo Guervós, Bart Wiegmans,
Paweł Murias, Jonathan Worthington, Antonio, Ben Davies, Samantha McVey,
threadless-screw, ZhongnianTao, Stoned Elipot, Altai-man, Luis F. Uceta,
Jan-Olof Hendig, cfa, Christian Bartolomäus, Stéphane Payrard,
Peter du Marchie van Voorthuysen, cygx, Greg Donald, Naoum Hankache,
finanalyst, holli-holzer, Alexander, German Rodriguez Herrera, Nick Logan,
Stefan Fischer, ab5tract, Chloé Kekoa, Dominika Góral, Paul Marquess, sarna,
stoned, Aearnus, Claudio Ramirez, Clifton Wood, David Warring,
Deven T. Corzine, Jack Kuan, Jeremy Studer, Jimmy Zhuo, Kent Fredric,
Kevin Pye, Larry Wall, Mike Swierczek, Moritz Lenz, Nele Schwarz,
Salve J. Nilsen, Skye Shaw, Suman Khanal, Tim Van den Langenbergh,
Tobias Boege, alanrocker, lukasvalle, peschwa, tusindfryd

If you would like to contribute or find out more information, visit
<https://raku.org>, <https://rakudo.org/how-to-help>, ask on the
<perl6-compiler@perl.org> mailing list, or ask on IRC #raku on freenode.

Additionally, we invite you to make a donation to The Perl Foundation
to sponsor Raku development: <https://donate.perlfoundation.org/>
(put “Raku Core Development Fund” in the ‘Purpose’ text field)

The next release of Rakudo (#133), is tentatively scheduled for 2019-12-21.

A list of the other planned release dates is available in the
“docs/release_guide.pod” file.

The development team appreciates feedback! If you’re using Rakudo, do
get back to us. Questions, comments, suggestions for improvements, cool
discoveries, incredible hacks, or any other feedback – get in touch with
us through (the above-mentioned) mailing list or IRC channel. Enjoy!

Please note that recent releases have known issues running on the JVM.
We are working to get the JVM backend working again but do not yet have
an estimated delivery date.

[^1]: See <http://moarvm.org/>

[^2]: What’s the difference between the Rakudo compiler and the Rakudo
Star distribution?

The Rakudo compiler is a compiler for the Raku language.
Not much more.

The Rakudo Star distribution is the Rakudo compiler plus a selection
of useful Raku modules, a module installer, Raku introductory
documentation, and other software that can be used with the Rakudo
compiler to enhance its utility.
