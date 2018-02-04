class Compiler does Systemic {
    has Str $.id;
    has Str $.release;
    has Str $!build-date;
    has Str $.codename;
    BEGIN my $id = $*W.handle.Str ~ '.' ~ nqp::time_n();

    submethod BUILD (
      :$!name      = 'rakudo',
      :$!auth      = 'The Perl Foundation',
      :$version,
      :$release,
      :$build-date,
      :$codename
      --> Nil
    ) {
# XXX Various issues with this stuff on JVM
        my Mu $compiler := nqp::getcurhllsym('$COMPILER_CONFIG');
        $!id = nqp::p6box_s(nqp::ifnull(nqp::atkey($compiler,'id'),$id));
        $!version = Version.new(
          $version // nqp::p6box_s(nqp::atkey($compiler, 'version')) );
        $!release =
          $release // nqp::p6box_s(nqp::atkey($compiler, 'release-number'));
        $!build-date =
          $build-date // nqp::p6box_s(nqp::atkey($compiler, 'build-date'));
        $!codename =
          $codename // nqp::p6box_s(nqp::atkey($compiler, 'codename'));
    }

    method build-date() {
        DateTime.new($!build-date)
    }

    method verbose-config(:$say) {
        my $compiler := nqp::getcomp("perl6");
        my $backend  := $compiler.backend;
        my $name     := $backend.name;

        my $items := nqp::list_s;
        nqp::push_s($items,$name ~ '::' ~ .key ~ '=' ~ .value)
          for $backend.config;

        my $language := $compiler.language;
        nqp::push_s($items,$language ~ '::' ~ .key ~ '=' ~ .value)
          for $compiler.config;

        nqp::push_s(
          $items,
          'repo::chain=' ~ (try $*REPO.repo-chain.map( *.gist ).join(" ")) // ''
        );

        nqp::push_s($items,"distro::$_={ $*DISTRO."$_"() // '' }")
          for <auth desc is-win name path-sep release signature version>;

        nqp::push_s($items,"kernel::$_={ $*KERNEL."$_"() // '' }")
          for <arch archname auth bits desc
               hardware name release signature version>;

        try {
            require System::Info;

            my $sysinfo = System::Info.new;
            nqp::push_s($items,"sysinfo::{ .name }={ $sysinfo.$_ // '' }")
              for $sysinfo.^methods.grep: { .count == 1 && .name ne 'new' };
        }

        my str $string = nqp::join("\n",Rakudo::Sorting.MERGESORT-str($items));

        if $say {
            nqp::say($string);
            Nil
        }
        else {
            my %config;
            my $iter := nqp::iterator($items);
            while $iter {
                my ($main,$key,$value) = nqp::shift($iter).split(<:: =>);
                %config.AT-KEY($main).AT-KEY($key) = $value
            }

            %config but role {
                has $!string = $string;
                proto method Str()  { $!string }
                proto method gist() { $!string }
            }
        }
    }
}

# vim: ft=perl6 expandtab sw=4
