my class Grammar is Cursor {

    # cache cursor initialization lookup
    my $cursor-init := Cursor.^lookup("!cursor_init");

    method parse(\target, :$rule, :$args, Mu :$actions) {
        nqp::decont(nqp::getlexcaller('$/') =
          nqp::if(
            (my $cursor := nqp::if(
              $rule,
              nqp::if(
                $args,
                self!cursor-init(target, %_).set_actions($actions)."$rule"(|$args.Capture),
                self!cursor-init(target, %_).set_actions($actions)."$rule"()
              ),
              nqp::if(
                $args,
                self!cursor-init(target, %_).set_actions($actions).TOP(|$args.Capture),
                self!cursor-init(target, %_).set_actions($actions).TOP()
              ),
            )),
            nqp::stmts(
              (my $match := $cursor.MATCH),
              nqp::while(
                $match && nqp::isne_i(
                  nqp::getattr_i(($match := $cursor.MATCH),Match,'$!to'),
                  target.chars
                ),
                $match := ($cursor := $cursor.'!cursor_next'()).MATCH
              ),
              $match || Nil
            ),
            Nil
          )
        )
    }

    method subparse(\target, :$rule, :$args, Mu :$actions) {
        nqp::decont(nqp::getlexcaller('$/') =
          nqp::if(
            $rule,
            nqp::if(
              $args,
              self!cursor-init(target, %_).set_actions($actions)."$rule"(|$args.Capture).MATCH,
              self!cursor-init(target, %_).set_actions($actions)."$rule"().MATCH,
            ),
            nqp::if(
              $args,
              self!cursor-init(target, %_).set_actions($actions).TOP(|$args.Capture).MATCH,
              self!cursor-init(target, %_).set_actions($actions).TOP().MATCH
            ),
          )
        )
    }

    method parsefile(Str(Cool) $filename, :$enc) {
        nqp::decont(nqp::getlexcaller('$/') = nqp::if(
          nqp::elems(nqp::getattr(%_,Map,'$!storage')),
          self.parse($filename.IO.slurp(:$enc), |%_),
          self.parse($filename.IO.slurp(:$enc))
        ))
    }

    method !cursor-init(\target, %opts) is raw {
        nqp::if(
          nqp::elems(nqp::getattr(%opts,Map,'$!storage')),
          $cursor-init(self, target, |%opts),
          $cursor-init(self, target)
        )
    }
}

# vim: ft=perl6 expandtab sw=4
