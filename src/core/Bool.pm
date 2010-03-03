augment class Bool {
    method Bool { self }
    method ACCEPTS($topic) { self }

    method perl() { self ?? "Bool::True" !! "Bool::False"; }
}
