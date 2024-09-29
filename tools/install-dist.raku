#!/usr/bin/env raku

use CompUnit::Repository::Staging;

# packagers preference candidate
multi sub MAIN(
  IO() :from(:$dist-prefix) = '.',
  IO() :to(:$repo-prefix)!,
       :for(:$repo-name)!,
  Bool :$build      = True,
  Bool :$precompile = True,
) {
    if fetch-dist-and-build($dist-prefix, $build) -> $dist {
        my $staging := CompUnit::Repository::Staging.new(
          :prefix($repo-prefix),
          :next-repo(CompUnit::RepositoryRegistry.repository-for-name($repo-name)),
          :name($repo-name),
        );
        $staging.install: $dist, :$precompile;
        $staging.remove-artifacts;
    }
    else {
        meh "Could not find distribution at '$dist-prefix'";
    }
}

# candidate for normal users / developers
multi sub MAIN(
  IO() :from(:$dist-prefix) = '.',
       :to(:$repo-id)       = 'site',
  Bool :$force      = False,
  Bool :$build      = True,
  Bool :$precompile = True,
) {
    with (
      CompUnit::RepositoryRegistry.repository-for-name($repo-id),
      CompUnit::RepositoryRegistry.repository-for-spec($repo-id),
    ).first(* ~~ CompUnit::Repository::Installable) -> $repo {
        if fetch-dist-and-build($dist-prefix, $build) -> $dist {
            $repo.install: $dist, :$precompile, :$force;
        }
        else {
            meh "Could not find distribution at '$dist-prefix'";
        }
    }
    else {
        meh "Repository '$repo-id' is not an installable target";
    }
}

# just wanting to run the build logic
multi sub MAIN(
  IO() :from(:$dist-prefix) = '.',
  Bool :$only-build!,
) {
    build($dist-prefix) if $only-build;
}

#-------------------------------------------------------------------------------

# Distribution::Path ignores META.info files, but we can manually check for it
sub find-meta-file(IO::Path:D $dir) {
    <META6.json META.info>.map({$dir.child($_)}).first: *.f
}

# return dist object at given prefix if valid meta, optionally build as well
sub fetch-dist-and-build(IO::Path:D $dist-prefix, Bool:D $build) {
    if find-meta-file($dist-prefix) -> $meta-file {
        my $dist := Distribution::Path.new: $dist-prefix, :$meta-file;
        build($dist-prefix) if $build;
        $dist
    }
}

# run build logic, if there is any to run
sub build(IO::Path:D $dist-prefix --> Nil) {
    if find-meta-file($dist-prefix) -> $meta-file {
        my %meta := Rakudo::Internals::JSON.from-json($meta-file.slurp);

        if %meta<builder> -> $builder-spec {
            my $class   := (require ::($builder-spec));
            my $builder := $class.new(:%meta);
            if $builder.can-build {
                $builder.build;
            }
            else {
                meh "Failed to build with '$builder-spec'";
            }
        }
    }
    else {
        meh "Could not find META info at '$dist-prefix'";
    }
}

# throw a tantrum
sub meh(Str:D $message --> Nil) {
    note "$message\n\n$*USAGE";
    exit 1
}

=begin pod

This script is for installing Raku modules. B<install-dist.p6> does the same module registration like the 'zef' tool.

B<install-dist.p6> makes it easy to install a module system wide.

=head1 OPTIONS

By default the destination is the C<site> repository of the Raku installation.

    # Install to a custom location
    --to=<destination>     # /home/username/my_raku_mod_dir

If you specify a destination that does not exists then it will be created.
The C<--to> option can only be used together with the C<--for> option. 

    --for=[ vendor | site ]

    --from=<home of the module source>   # default is the current directory

The command in the install session for packaging a Raku module could be done in the form:

    install-dist.raku --to=<buildroot/...> --for=vendor

It is recommended to set the environment variable
C<RAKUDO_RERESOLVE_DEPENDENCIES> when calling this script:

    RAKUDO_RERESOLVE_DEPENDENCIES=0 install-dist.raku --to=<buildroot/...> --for=site

=end pod

# vim: expandtab shiftwidth=4
