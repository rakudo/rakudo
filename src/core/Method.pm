my class Method { # declared in BOOTSTRAP
    # class Method is Routine { ... }

    multi method gist(Method:D:) { self.name }
}
