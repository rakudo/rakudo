augment class Junction {
    # XXX When ~ starts calling Stringy, probably this
    # method needs to be renamed.
    multi method Str() {
        self.perl()
    }
}
