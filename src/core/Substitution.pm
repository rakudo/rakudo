class Substitution {
    has $!matcher;
    has $!replacer;

    method ACCEPTS($topic is rw) {
        $topic = $topic.subst($!matcher, $!replacer);
    }
}
