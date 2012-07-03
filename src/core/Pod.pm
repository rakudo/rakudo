my class Pod::Block {
    has %.config;
    has @.content;
}

my class Pod::Block::Para is Pod::Block {
}

my class Pod::Block::Named is Pod::Block {
    has $.name;
}

my class Pod::Block::Comment is Pod::Block { }

my class Pod::Block::Code is Pod::Block {
    has @.allowed;
}

my class Pod::Block::Declarator is Pod::Block {
    has $.WHEREFORE;
    method set_docee($d) {
        $!WHEREFORE = $d
    }
    method Str {
        ~@.content
    }
    method gist {
        self.Stringy
    }
}

my class Pod::Block::Table is Pod::Block {
    has $.caption;
    has @.headers; # optional, may be empty
}

my class Pod::FormattingCode is Pod::Block {
    has $.type;
}

my class Pod::Heading is Pod::Block {
    has $.level;
}

my class Pod::Item is Pod::Block {
    has $.level;
}

class Pod::Config {
    has $.type;
    has %.config;
}

# vim: ft=perl6
