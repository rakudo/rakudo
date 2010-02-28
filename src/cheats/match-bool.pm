INIT {
    Regex::Match.^add_method('Bool', method () {
        self.to >= self.from
    });
    Regex::Match.^add_method('defined', method () {
        self.to >= self.from
    });
}
