# Something that can be stapped, and enables messages to be published to
# all tappers. Note that there are no replay semantics; anybody who will
# tap too late will miss things.

my class Publisher does Supply {
    method next(\msg) {
        for self.tappers {
            .next().(msg)
        }
        Nil;
    }

    method last() {
        for self.tappers {
            if .last -> $l { $l() }
        }
        Nil;
    }

    method fail($ex) {
        for self.tappers {
            if .fail -> $t { $t($ex) }
        }
        Nil;
    }
}
