class Compiler does Systemic {
    my constant $id = nqp::p6box_s(nqp::sha1(
        $*W.handle.Str
        ~ nqp::atkey(nqp::gethllsym(
        'default', 'SysConfig').rakudo-build-config(), 'source-digest')
    ));

    my Mu $compiler := nqp::gethllsym('default', 'SysConfig')
        .rakudo-build-config();

    # XXX Various issues with this stuff on JVM
    has $.id is built(:bind) = nqp::ifnull(nqp::atkey($compiler,'id'),$id);
    has $.release is built(:bind) = nqp::atkey($compiler,'release-number');
    has $.codename is built(:bind) = nqp::atkey($compiler, 'codename');

    submethod TWEAK(--> Nil) {
        # https://github.com/rakudo/rakudo/issues/3436
        nqp::bind($!name,'rakudo');
        nqp::bind($!auth,'The Perl Foundation');

        # looks like: 2018.01-50-g8afd791c1
        nqp::bind($!version,Version.new(nqp::p6box_s(nqp::atkey($compiler,'version'))))
          unless $!version;
    }

    method backend() {
        nqp::getcomp("Raku").backend.name
    }

    proto method id(|) {*}
    multi method id(Compiler:U:) { nqp::ifnull(nqp::atkey($compiler,'id'),$id) }
    multi method id(Compiler:D:) { $!id }

    method verbose-config(:$say) {
        my $compiler := nqp::getcomp("Raku");
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

            my $sysinfo := System::Info.new;
            nqp::push_s($items,"sysinfo::{ .name }={ $sysinfo.$_ // '' }")
              for $sysinfo.^methods.grep: { .count == 1 && .name ne 'new' };
        }

        my $string := nqp::join("\n",Rakudo::Sorting.MERGESORT-str($items));

        if $say {
            nqp::say($string);
            Nil
        }
        else {
            my %config;
            my $clone := nqp::clone($items);
            while $clone {
                my ($main,$key,$value) = nqp::shift_s($clone).split(<:: =>);
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

# vim: expandtab shiftwidth=4
