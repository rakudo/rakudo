my class Pod::Block {
    has %.config;
    has @.contents;

    submethod BUILD(:%!config, :@!contents --> Nil) {}

    sub pod-gist(Pod::Block $pod, $level = 0) {
        my $leading = ' ' x $level;
        my %confs;
        my @chunks;
        for <config name level caption type term> {
            my $thing = $pod.?"$_"();
            if $thing {
                %confs{$_} = nqp::istype($thing,Iterable)
                  ?? $thing.raku
                  !! $thing.Str;
            }
        }
        @chunks = $leading, $pod.^name, (%confs.raku if %confs), "\n";
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
        @chunks.join;
    }

    multi method gist(Pod::Block:D:) {
        pod-gist(self)
    }
}

my class Pod::Block::Para is Pod::Block {
}

my class Pod::Block::Named is Pod::Block {
    has $.name;
}

my class Pod::Block::Comment is Pod::Block { }

my class Pod::Block::Code is Pod::Block {
}

my class Pod::Block::Declarator is Pod::Block {
    has $.WHEREFORE;
    has @!leading;
    has @!trailing;

    submethod BUILD(:@!leading, :@!trailing --> Nil) {}

    method set_docee($d) {
        $!WHEREFORE = $d
    }
    method Str {
        @.contents.join('')
    }
    multi method gist(Pod::Block::Declarator:D:) {
        @.contents.join('')
    }

    method contents {
        if @!leading && @!trailing {
            [ $.leading ~ "\n" ~ $.trailing ]
        }
        elsif @!leading {
            [ $.leading ]
        }
        elsif @!trailing {
            [ $.trailing ]
        }
        else {
            []
        }
    }

    method leading  { @!leading  ?? @!leading.join(' ')  !! Nil }
    method trailing { @!trailing ?? @!trailing.join(' ') !! Nil }

    method _add_leading($addition) {
        @!leading.push: ~$addition;
    }

    method _add_trailing($addition) {
        @!trailing.push: ~$addition;
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
