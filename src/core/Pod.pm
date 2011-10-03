my package Pod {
    class Block {
        has %.config;
        has @.content;
    }

    class Block::Para is Block {
    }

    class Block::Named is Block {
        has $.name;
    }

    class Block::Comment is Block { }

    class Block::Code is Block {
        has @.allowed;
    }

    class Block::Declarator is Block {
        has $.WHEREFORE;
        method set_docee($d) {
            $!WHEREFORE = $d
        }
        method Stringy {
            ~@.content
        }
    }

    class Block::Table is Block {
        has $.caption;
        has @.headers; # optional, may be empty
    }

    class FormattingCode is Block {
        has $.type;
    }

    class Heading is Block {
        has $.level;
    }

    class Item is Block {
        has $.level;
    }

    class Config {
        has $.type;
        has %.config;
    }
}

# vim: ft=perl6
