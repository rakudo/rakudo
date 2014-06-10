module lib { };
our sub EXPORT(*@a) {
    @*INC.unshift: @a.map( { CompUnitRepo::Local::File.new($_) } );
    return ().hash;
}
