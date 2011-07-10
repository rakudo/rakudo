my package Pod {
    class Block {
        has @.content;
    }

    class Block::Named is Block {
        has $.name;
    }

    class Block::Comment is Block { }

    class Block::Code is Block {
        has @.allowed;
    }

    class Block::Table is Block {
        has $.caption;
        has @.headers; # optional, may be empty
    }

    class Item is Block {
        has $.level;
    }
}

# vim: ft=perl6
