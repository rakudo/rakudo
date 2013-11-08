my class Submethod { # declared in BOOTSTRAP
    # class Submethod is Routine { ... }

    multi method gist(Submethod:D:) { self.name }
}
