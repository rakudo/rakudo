my package Pod {
    class Block {
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
    }

    class Block::Table is Block {
        has $.caption;
        has @.headers; # optional, may be empty
    }

    class Heading is Block {
        has $.level;
    }

    class Item is Block {
        has $.level;
    }
}

# vim: ft=perl6
