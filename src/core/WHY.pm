my class WHY {
    has Str $!leading;
    has Str $!trailing;

    method new(:$leading, :$trailing) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, WHY, '$!leading',  ~$leading)  if $leading.defined;
        nqp::bindattr($obj, WHY, '$!trailing', ~$trailing) if $trailing.defined;
        $obj
    }

    method leading  { $!leading }
    method trailing { $!trailing }

    method _add_leading($addition) {
        if $!leading.defined {
            nqp::bindattr(self, WHY, '$!leading', nqp::concat($!leading, nqp::concat("\n", ~$addition)));
        } else {
            $!leading = ~$addition;
        }
    }

    method _add_trailing($addition) {
        if $!trailing.defined {
            nqp::bindattr(self, WHY, '$!trailing', nqp::concat($!trailing, nqp::concat("\n", ~$addition)));
        } else {
            $!trailing = ~$addition;
        }
    }

    method content {
        my $result = '';

        $result ~= $!leading  if $!leading.defined;
        $result ~= "\n"       if $!leading.defined && $!trailing.defined;
        $result ~= $!trailing if $!trailing.defined;

        nqp::p6box_s($result)
    }

    method Str {
        $.content
    }

    method gist {
        $.content
    }
}
