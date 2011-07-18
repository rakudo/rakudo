my class Cool {

    ## numeric methods

    method rand() { self.Num.rand }
    
    ## string methods

    method bytes() {
        nqp::p6box_i(pir::bytelength__IS(nqp::unbox_s(self.Str)));
    }

    method chars() {
        nqp::p6box_i(nqp::chars(nqp::unbox_s(self.Str)));
    }

    method fmt($format = '%s') {
        nqp::p6box_s(
            pir::sprintf__SsP(nqp::unbox_s($format.Stringy), nqp::list(self))
        )
    }

    method substr($start as Int, $length?) { 
        self.Stringy.substr($start, $length);
    }

    method uc() {
        nqp::p6box_s(nqp::uc(nqp::unbox_s(self.Str)))
    }

    method lc() {
        nqp::p6box_s(nqp::lc(nqp::unbox_s(self.Str)))
    }

    method ucfirst() {
        my $self-str = self.Str;
        $self-str eq '' ?? '' !! $self-str.substr(0, 1).uc ~ $self-str.substr(1)
    }

    method lcfirst() {
        my $self-str = self.Str;
        $self-str eq '' ?? '' !! $self-str.substr(0, 1).lc ~ $self-str.substr(1)
    }

    method chomp() {
        self.Str.chomp;
    }

    method chop() {
        self.Str.chop
    }

    method ord() {
        nqp::p6box_i(nqp::ord(nqp::unbox_s(self.Str)))
    }

    method flip() {
        nqp::p6box_s(pir::box__PS(nqp::unbox_s(self.Str)).reverse)
    }

    proto method index(|$) {*}
    multi method index(Cool $needle, Cool $pos = 0) {
        my $result := nqp::p6box_i(nqp::index(
                nqp::unbox_s(self.Str),
                nqp::unbox_s($needle.Str),
                nqp::unbox_i($pos.Int)
        ));
        # TODO: fail() instead of returning Str
        $result < 0 ?? Str !! $result;
    }

    proto method rindex(|$) {*}
    multi method rindex(Cool $needle, Cool $pos?) {
        if $needle eq '' {
            return $pos.defined && $pos < self.chars
                    ?? $pos
                    !! self.chars;
        }
        my $result =
            nqp::p6box_i(
                pir::box__PS(nqp::unbox_s(self.Str)).reverse_index(
                    nqp::unbox_s($needle.Str),
                    nqp::unbox_i(($pos // 0).Int)));
        fail "substring not found" if $result < 0;
        $result;
    }

    method ords(Cool:D:) { self.Str.ords }
    proto method split(|$) {*}
    proto method match(|$) {*}
    multi method match(Cool:D: Cool $target, *%adverbs) {
        self.Str.match($target.Stringy, |%adverbs)
    }
}

sub chop($s)                  { $s.chop }
sub chomp($s)                 { $s.chomp }
sub flip($s)                  { $s.flip }
sub index($s,$needle,$pos=0)  { $s.index($needle,$pos) }
sub lc($s)                    { $s.lc }
sub lcfirst($s)               { $s.lcfirst }
sub ord($s)                   { $s.ord }
sub substr($s,$pos,$chars?)   { $s.substr($pos,$chars) }
sub uc($s)                    { $s.uc }
sub ucfirst($s)               { $s.ucfirst }
proto sub rindex(|$) { * };
multi sub rindex(Cool $s, Cool $needle, Cool $pos) { $s.rindex($needle, $pos) };
multi sub rindex(Cool $s, Cool $needle)            { $s.rindex($needle) };

proto sub ords(|$)            { * }
multi sub ords(Cool $s)       { ords($s.Stringy) }
