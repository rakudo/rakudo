augment class Bool does Abstraction {
    method Bool { self }
    method ACCEPTS($topic) { self }

    method perl() { self ?? "Bool::True" !! "Bool::False"; }

    method Str() { $.perl() }

    method Bridge() { self ?? 1.Bridge !! 0.Bridge }
}
