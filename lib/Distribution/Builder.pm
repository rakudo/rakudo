unit class Distribution::Builder;

has Distribution::Locally $.dist;

method configure() {
    return unless $.dist.meta<build>:exists;

    my %vars = $*VM.config;
    %vars<perl6> = $*EXECUTABLE;

    if $.dist.meta<build><makefile-variables> -> $var-decl {
        for $var-decl.kv -> $key, $value {
            %vars{$key} = self.expand-value(|$value.kv);
        }
    }

    my $makefile = $.dist.prefix.child('Makefile.in').slurp;
    for %vars.kv -> $k, $v {
        $makefile ~~ s:g/\%$k\%/$v/;
    }
    $.dist.prefix.child('Makefile').spurt($makefile);
    self
}

method build() {
    indir $.dist.prefix, {
        run('make');
    }
    self
}

multi method expand-value('library', $value) {
    my $prefix = $.dist.prefix;
    my $resources = $prefix.child('resources');
    my $libraries = $resources.child('libraries');
    state $created-libraries = False;
    unless $created-libraries {
        $created-libraries = True;
        $resources.mkdir unless $resources.e;
        $libraries.mkdir unless $libraries.e;
    }

    # Makefile variables must be relative to the Makefile's directory!
    'resources'.IO.child('libraries').child($*VM.platform-library-name($value.IO))
}

multi method expand-value('run', $value) {
    run($value, :out).out.lines.join('');
}

# vim: ft=perl6
