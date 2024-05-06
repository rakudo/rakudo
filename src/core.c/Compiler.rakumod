class Compiler does Systemic {
    my constant $compiler = nqp::getcomp("Raku");
    my constant $config =
      nqp::gethllsym('default','SysConfig').rakudo-build-config;
    my constant $compilation-id = nqp::box_s(
      nqp::sha1($*W.handle.Str ~ nqp::atkey($config,'source-digest')),Str
    );
    my constant $backend = $compiler.backend;
    my constant $name    = $backend.name;

    # XXX Various issues with this stuff on JVM
    has $.id       is built(:bind) = $compilation-id;
    has $.release  is built(:bind) =
      BEGIN nqp::ifnull(nqp::atkey($config,'release-number'),"");
    has $.codename is built(:bind) =
      BEGIN nqp::ifnull(nqp::atkey($config,'codename'),"");

    submethod TWEAK(--> Nil) {
        # https://github.com/rakudo/rakudo/issues/3436
        nqp::bind($!name,'rakudo');
        nqp::bind($!auth,'Yet Another Society');

        # looks like: 2018.01-50-g8afd791c1
        nqp::bind($!version,BEGIN Version.new(
          nqp::box_s(nqp::atkey($config,'version'),Str)
        )) unless $!version;
    }

    method backend() { $name }
    method flavor()  { #RAKUDO_FLAVOR# }

    proto method id(|) {*}
    multi method id(Compiler:U:) { $compilation-id }
    multi method id(Compiler:D:) { $!id            }

    method verbose-config(:$say) {

        my $items := nqp::list_s;
        nqp::push_s($items,$name ~ '::' ~ .key ~ '=' ~ .value)
          for BEGIN $backend.config;

        my $language := BEGIN $compiler.language;
        nqp::push_s($items,$language ~ '::' ~ .key ~ '=' ~ .value)
          for BEGIN $compiler.config;

        nqp::push_s(
          $items,
          'repo::chain=' ~ (try $*REPO.repo-chain.map( *.gist ).join(" ")) // ''
        );

        my $distro := $*DISTRO;
        nqp::push_s($items,"distro::$_=" ~ ($distro."$_"() // ''))
          for <auth desc is-win name path-sep release signature version>;

        my $kernel := $*KERNEL;
        nqp::push_s($items,"kernel::$_=" ~ ($kernel."$_"() // ''))
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

    method supports-op(str $name) { $compiler.supports-op($name) }
}

# vim: expandtab shiftwidth=4
