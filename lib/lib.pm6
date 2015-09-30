module lib { };
my sub EXPORT(*@a) {
    @*INC.prepend: PARSE-INCLUDE-SPECS(@a.join(','));
    return ().hash;
}
