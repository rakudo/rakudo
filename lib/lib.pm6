module lib { };
my sub EXPORT(*@a) {
    @*INC.unshift: PARSE-INCLUDE-SPECS(@a.join(','));
    return ().hash;
}
