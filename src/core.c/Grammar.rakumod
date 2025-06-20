my class Grammar is Match {

    method parse($orig is raw, :$rule = "TOP", :$args, Mu :$actions) {
        my $*LINEPOSCACHE;
        my $grammar := self.new(:$orig, |%_);
        $grammar.set_actions($actions)
          unless nqp::eqaddr(nqp::decont($actions),Mu);

        my $cursor := $args
          ?? $grammar."$rule"(|$args.Capture)
          !! $grammar."$rule"();

        my int $chars = $orig.chars;  # must be HLL, $orig can be Cool
        nqp::while(
          $cursor
            && nqp::isne_i(nqp::getattr_i($cursor,Match,'$!pos'),$chars),
          $cursor := $cursor.'!cursor_next'()
        );

        nqp::getlexcaller('$/') = $cursor
          ?? $cursor.Match::MATCH
          !! Nil
    }

    method subparse($orig is raw, :$rule = "TOP", :$args, Mu :$actions) {
        my $grammar := self.new(:$orig, |%_);
        $grammar.set_actions($actions)
          unless nqp::eqaddr(nqp::decont($actions),Mu);

        nqp::getlexcaller('$/') = $args
          ?? $grammar."$rule"(|$args.Capture).Match::MATCH
          !! $grammar."$rule"().Match::MATCH
      }

    method parsefile(Str(Cool) $filename, :$enc) {
        nqp::getlexcaller('$/') = nqp::elems(nqp::getattr(%_,Map,'$!storage'))
          ?? self.parse($filename.IO.slurp(:$enc), :$filename, |%_)
          !! self.parse($filename.IO.slurp(:$enc), :$filename)
    }
}

# vim: expandtab shiftwidth=4
