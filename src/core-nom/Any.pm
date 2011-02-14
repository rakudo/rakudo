my class Any is Mu {
    multi method ACCEPTS(Any:D $self: $topic) {
        $self === $topic
    }
}
