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
            (my $match := $cursor.MATCH),
            (my int $chars = nqp::chars($orig)),
            nqp::while(
              $match && nqp::isne_i(
                nqp::getattr_i(($match := $cursor.MATCH),Match,'$!pos'),
                $chars
              ),
              $match := ($cursor := $cursor.'!cursor_next'()).MATCH
            ),
            $match || Nil
          ),
          Nil
        ))
    }

    method subparse($orig is raw, :$rule = "TOP", :$args, :$actions) is raw {
        my $grammar := self.new(:$orig, |%_).set_actions($actions);
        nqp::decont(nqp::getlexcaller('$/') = nqp::if(
          $args,
          $grammar."$rule"(|$args.Capture).MATCH,
          $grammar."$rule"().MATCH,
        ))
      }

    method parsefile(Str(Cool) $filename, :$enc) is raw {
        nqp::decont(nqp::getlexcaller('$/') = nqp::if(
          nqp::elems(nqp::getattr(%_,Map,'$!storage')),
          self.parse($filename.IO.slurp(:$enc), :$filename, |%_),
          self.parse($filename.IO.slurp(:$enc), :$filename)
        ))
    }
}

# vim: expandtab sw=4
