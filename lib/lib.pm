module lib { };
our sub EXPORT(*@a) {
    @*INC.unshift: @a;
}
