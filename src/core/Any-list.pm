augment class Any {

    our Str multi method join($separator = '') {
        pir::join__SsP($separator, self.list);
    }


}

# vim: ft=perl6
