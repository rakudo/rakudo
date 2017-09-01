my class Grammar is Match {

    method parse(\target, :$rule, :$args, Mu :$actions, :$filename) is raw {
        my $*LINEPOSCACHE;
        nqp::stmts(
          (my $grammar := self.new(:orig(target), |%_).set_actions($actions)),
          nqp::decont(nqp::getlexcaller('$/') =
            nqp::if(
              (my $cursor := nqp::if(
                $rule,
                nqp::if(
                  $args,
                  $grammar."$rule"(|$args.Capture),
                  $grammar."$rule"()
                ),
                nqp::if(
                  $args,
                  $grammar.TOP(|$args.Capture),
                  $grammar.TOP()
                ),
              )),
              nqp::stmts(
                (my $match := $cursor.MATCH),
                nqp::while(
                  $match && nqp::isne_i(
                    nqp::getattr_i(($match := $cursor.MATCH),Match,'$!pos'),
                    target.chars
                  ),
                  $match := ($cursor := $cursor.'!cursor_next'()).MATCH
                ),
                $match || Nil
              ),
              Nil
            )
          )
        )
    }

    method subparse(\target, :$rule, :$args, :$actions) is raw {
        nqp::stmts(
          (my $grammar := self.new(:orig(target), |%_).set_actions($actions)),
          nqp::decont(nqp::getlexcaller('$/') =
            nqp::if(
              $rule,
              nqp::if(
                $args,
                $grammar."$rule"(|$args.Capture).MATCH,
                $grammar."$rule"().MATCH,
              ),
              nqp::if(
                $args,
                $grammar.TOP(|$args.Capture).MATCH,
                $grammar.TOP().MATCH
              ),
            )
          )
        )
      }

    method parsefile(Str(Cool) $filename, :$enc) is raw {
        nqp::decont(nqp::getlexcaller('$/') = nqp::if(
          nqp::elems(nqp::getattr(%_,Map,'$!storage')),
          self.parse($filename.IO.slurp(:$enc), :$filename, |%_),
          self.parse($filename.IO.slurp(:$enc), :$filename)
        ))
    }
}

# vim: ft=perl6 expandtab sw=4
