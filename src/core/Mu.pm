augment class Mu {
    method Bool { $.defined }

    multi method notdef() { !self.defined; }
}
