class Substitution {
    has $!matcher;
    has $!replacer;
    has %!adverbs;

    method new(:$matcher, :$replacer, *%adverbs) {
        self.bless(*, :$matcher, :$replacer, :%adverbs);
    }

    method ACCEPTS($topic is rw) {
        $topic = $topic.subst($!matcher, $!replacer, |%!adverbs);
    }
}
