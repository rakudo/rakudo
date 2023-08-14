my class Grammar is Match {

    method parse(
      $orig is raw, :$rule = "TOP", :$args, Mu :$actions
    ) is raw {
        my $*LINEPOSCACHE;
        my $grammar := self.new(:$orig, |%_).set_actions($actions);

        nqp::decont(nqp::getlexcaller('$/') = nqp::if(
          (my $cursor := nqp::if(
            $args,
            $grammar."$rule"(|$args.Capture),
            $grammar."$rule"()
          )),
          nqp::stmts(
            (my int $chars = $orig.chars),  # must be HLL, $orig can be Cool
            nqp::while(
              $cursor
                && nqp::isne_i(nqp::getattr_i($cursor,Match,'$!pos'),$chars),
              $cursor := $cursor.'!cursor_next'()
            ),
            nqp::if(
              $cursor,
              $cursor.MATCH,
              Nil
            )
          ),
          Nil
        ))
    }

    method subparse($orig is raw, :$rule = "TOP", :$args, :$actions) is raw {
        my $grammar := self.new(:$orig, |%_).set_actions($actions);
        nqp::decont(
          nqp::getlexcaller('$/') = $args
            ?? $grammar."$rule"(|$args.Capture).MATCH
            !! $grammar."$rule"().MATCH
        )
      }

    method parsefile(Str(Cool) $filename, :$enc) is raw {
        nqp::decont(
          nqp::getlexcaller('$/') = nqp::elems(nqp::getattr(%_,Map,'$!storage'))
            ?? self.parse($filename.IO.slurp(:$enc), :$filename, |%_)
            !! self.parse($filename.IO.slurp(:$enc), :$filename)
        )
    }
}

# vim: expandtab shiftwidth=4
