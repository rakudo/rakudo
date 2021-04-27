my class X::Encoding::Unknown { ... }
my class X::Encoding::AlreadyRegistered { ... }

my class Encoding::Registry {
    my $lock := Lock.new;
    my %lookup;  # access for registering builtins at compile time
    my $lookup := nqp::getattr(%lookup,Map,'$!storage');  # access for runtime

    BEGIN {
        my $lookup := nqp::bindattr(%lookup,Map,'$!storage',nqp::hash);
        ### If updating encodings here, also update src/core.c/Rakudo/Internals.pm6
        my $encodings := nqp::list(
          nqp::list('utf8',    'utf-8'),
          nqp::list('utf8-c8', 'utf8c8',   'utf-8-c8'),
          nqp::list('utf16',   'utf-16'),
          nqp::list('utf16le', 'utf-16le', 'utf16-le', 'utf-16-le'),
          nqp::list('utf16be', 'utf-16be', 'utf16-be', 'utf-16-be'),
#?if !moar
          nqp::list('utf32',   'utf-32'),
#?endif
          nqp::list('ascii'),
          nqp::list('iso-8859-1','iso_8859-1:1987','iso_8859-1','iso-ir-100',
            'latin1','latin-1','csisolatin1','l1','ibm819','cp819'),
          nqp::list('windows-1251', 'windows1251'),
          nqp::list('windows-1252', 'windows1252'),
          nqp::list('windows-932',  'windows932'),
          nqp::list('gb2312',  'gb2312'),
          nqp::list('gb18030', 'gb18030'),
        );
        my int $i = -1;
        my int $elems = nqp::elems($encodings);
        while nqp::islt_i(($i = nqp::add_i($i,1)),$elems) {
            my $names := nqp::atpos($encodings,$i);
            my $builtin := nqp::create(Encoding::Builtin).SET-SELF(
              nqp::shift($names),nqp::clone($names));
            nqp::bindkey($lookup,$builtin.name,$builtin);
            while nqp::elems($names) {
                nqp::bindkey($lookup,nqp::shift($names),$builtin);
            }
        }
    }

    method register(Encoding $enc --> Nil) {
        $lock.protect: {
            nqp::existskey($lookup,(my str $key = $enc.name.fc))
              ?? X::Encoding::AlreadyRegistered.new(name => $enc.name).throw
              !! nqp::bindkey($lookup,$key,$enc);
            my $names := nqp::getattr($enc.alternative-names,List,'$!reified');
            my int $elems = nqp::elems($names);
            my int $i = -1;
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::if(
                nqp::existskey($lookup,($key = nqp::atpos($names,$i).fc)),
                X::Encoding::AlreadyRegistered.new(
                  name => nqp::atpos($names,$i)).throw,
                nqp::bindkey($lookup,$key,$enc)
              )
            );
        }
    }

    method find(Str() $name) {
        $lock.protect: {
            nqp::ifnull(
              nqp::atkey($lookup,$name.fc),
              X::Encoding::Unknown.new(:$name).throw
            )
        }
    }
}

# vim: expandtab shiftwidth=4
