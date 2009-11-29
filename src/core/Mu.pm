augment class Mu {
    method true { $.defined }

    multi method notdef() { !self.defined; }
}
