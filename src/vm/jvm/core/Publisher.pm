# Something that can be subscribed to, and enables messages to be published to
# all subscribers. Note that there are no replay semantics; anybody who will
# subscribe too late will miss things.
my class Publisher does Subscribable {
    method next(\msg) {
        for self.subscriptions {
            .next().(msg)
        }
        Nil;
    }

    method last() {
        for self.subscriptions {
            if .last -> $l { $l() }
        }
        Nil;
    }

    method fail($ex) {
        for self.subscriptions {
            if .fail -> $t { $t($ex) }
        }
        Nil;
    }
}
