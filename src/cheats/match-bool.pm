INIT {
    Regex::Match.^add_method('Bool', method () {
        self.to >= self.from
    });
}
