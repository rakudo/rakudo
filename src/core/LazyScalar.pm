my class LazyScalar is Proxy {
    method new($code) {
        my int $reified = 0;
        my Mu $payload;
        self.Proxy::new(
            FETCH => sub ($) {
                unless $reified {
                    $payload := $code();
                    $reified = 1;
                } 
                $payload;
            },
            STORE => sub ($, Mu \$new) {
                $payload := $new;
            }
        )
    }
}
