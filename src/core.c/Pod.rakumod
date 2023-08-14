my class Pod::Block {
    has %.config;
    has @.contents;

    sub pod-gist(Pod::Block:D $pod, $level = 0) {
        my $leading = ' ' x $level;
        my %confs;
        for <config name level caption type term> {
            if $pod.?"$_"() -> $thing {
                %confs{$_} = nqp::istype($thing,Iterable)
                  ?? $thing.raku
                  !! $thing.Str;
            }
        }
        my str @chunks = $leading, $pod.^name, (%confs.raku if %confs), "\n";
        for $pod.contents.flat -> $c {
            if nqp::istype($c,Pod::Block) {
                @chunks.push: pod-gist($c, $level + 2);
            }
            elsif nqp::istype($c,Positional) {
                @chunks.append: $c>>.Str.raku.indent($level + 2), "\n";
            }
            else {
                @chunks.append: $c.Str.indent($level + 2), "\n";
            }
        }
        @chunks.join
    }

    multi method gist(Pod::Block:D:) { pod-gist(self) }
}

my class Pod::Block::Para    is Pod::Block { }
my class Pod::Block::Comment is Pod::Block { }
my class Pod::Block::Code    is Pod::Block { }

my class Pod::Block::Input  is Pod::Block::Code { }
my class Pod::Block::Output is Pod::Block::Code { }

my class Pod::Block::Named is Pod::Block {
    has $.name;
}

my class Pod::Block::Declarator is Pod::Block {
    has $.WHEREFORE;
    has @.leading;
    has @.trailing;

    method set_docee(Pod::Block::Declarator:D: $!WHEREFORE) { }

    multi method Str( Pod::Block::Declarator:D:) { self.contents.join }
    multi method gist(Pod::Block::Declarator:D:) { self.contents.join }

    method contents(Pod::Block::Declarator:D:) {
        @!leading
          ?? @!trailing
            ?? [ self.leading ~ "\n" ~ self.trailing ]
            !! [ self.leading ]
          !! @!trailing
            ?? [ self.trailing ]
            !! []
    }

    method leading(Pod::Block::Declarator:D:) {
        @!leading  ?? @!leading.join(' ')  !! Nil
    }
    method trailing(Pod::Block::Declarator:D:) {
        @!trailing ?? @!trailing.join(' ') !! Nil
    }

    method _add_leading(Pod::Block::Declarator:D: Str() $addition) {
        @!leading.push: $addition;
    }

    method _add_trailing(Pod::Block::Declarator:D: Str() $addition) {
        @!trailing.push: $addition;
    }
}

my class Pod::Block::Table is Pod::Block {
    has $.caption;
    has @.headers; # optional, may be empty
}

my class Pod::FormattingCode is Pod::Block {
    has $.type;
    has @.meta;
}

my class Pod::Heading is Pod::Block {
    has $.level;
}

my class Pod::Item is Pod::Block {
    has $.level;
}

my class Pod::Defn is Pod::Block {
    has $.term;
}

class Pod::Config {
    has $.type;
    has %.config;
}

# for passing raw instructions to specific backends
class Pod::Raw {
    has $.target;
    has @.contents;
}

# vim: expandtab shiftwidth=4
