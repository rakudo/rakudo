module lib { };
our sub EXPORT(*@a) {
    @*INC.unshift: CompUnitRepo::Local::File.new( @a );
    return ().hash;
}
