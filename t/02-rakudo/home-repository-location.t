use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

# https://github.com/rakudo/rakudo/issues/5625

plan 6;

my $code = 'print CompUnit::RepositoryRegistry.repository-for-name("home").prefix.absolute';

sub home-prefix(*%env) {
    run($*EXECUTABLE, '-e', $code, :out, :env(%(%*ENV, %env)))
      .out.slurp(:close)
}

if $*DISTRO.is-win {
    my $home = make-temp-dir.absolute;
    is home-prefix(:HOME($home), :XDG_DATA_HOME(make-temp-dir.absolute)),
      $home.IO.add('.raku').absolute,
      'Windows ignores XDG_DATA_HOME';
    skip 'XDG Base Directory support is not enabled on Windows', 5;
    exit;
}

my $home = make-temp-dir.absolute;
my $xdg  = make-temp-dir.absolute;

is home-prefix(:HOME($home), :XDG_DATA_HOME($xdg)),
  "$xdg/raku",
  'uses $XDG_DATA_HOME/raku without a legacy repository';

%*ENV<XDG_DATA_HOME>:delete;
is home-prefix(:HOME($home)),
  "$home/.local/share/raku",
  'falls back to $HOME/.local/share/raku without XDG_DATA_HOME';

is home-prefix(:HOME($home), :XDG_DATA_HOME('')),
  "$home/.local/share/raku",
  'empty XDG_DATA_HOME is treated as unset';

is home-prefix(:HOME($home), :XDG_DATA_HOME('relative/path')),
  "$home/.local/share/raku",
  'relative XDG_DATA_HOME is ignored';

"$home/.raku".IO.mkdir;
is home-prefix(:HOME($home), :XDG_DATA_HOME($xdg)),
  "$xdg/raku",
  'a ~/.raku without installed distributions (e.g. REPL history) is ignored';

"$home/.raku/dist".IO.mkdir;
is home-prefix(:HOME($home), :XDG_DATA_HOME($xdg)),
  "$home/.raku",
  'an existing legacy ~/.raku repository takes precedence';

# vim: expandtab shiftwidth=4
