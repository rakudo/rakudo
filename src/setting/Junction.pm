class Junction is also {
    our Str multi method Str() {
        $.perl;
    }

    method postcircumfix:<( )>(*@pos, *%named) {
        my @result = $.eigenstates.map({ $^code(|@pos, |%named) });
        return Junction.new(
            eigenstates => @result,
            type => self!type
        );
    }
}
