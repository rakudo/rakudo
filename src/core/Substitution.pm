class Substitution {
    has $!matcher;
    has $!replacer;
    has $!g;

    method ACCEPTS($topic is rw) {
        $topic = $topic.subst($!matcher, $!replacer, :$!g);
    }
}
