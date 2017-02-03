my class Grammar is Cursor {

    # cache cursor initialization lookup
    my $cursor-init := Cursor.^lookup("!cursor_init");

    method parse(\target, :$rule, :$args, Mu :$actions) {
        nqp::decont(nqp::getlexdyn('$/') =
          nqp::if(
            (my $cursor := nqp::if(
              $rule,
              nqp::if(
                $args,
                self!cursor-init(target, {:$actions, %_})."$rule"(|$args.Capture),
                self!cursor-init(target, {:$actions, %_})."$rule"()
              ),
              nqp::if(
                $args,
                self!cursor-init(target, {:$actions, %_}).TOP(|$args.Capture),
                self!cursor-init(target, {:$actions, %_}).TOP()
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
        nqp::decont(nqp::getlexdyn('$/') =
          nqp::if(
            $rule,
            nqp::if(
              $args,
              self!cursor-init(target, {:$actions, %_})."$rule"(|$args.Capture).MATCH,
              self!cursor-init(target, {:$actions, %_})."$rule"().MATCH,
            ),
            nqp::if(
              $args,
              self!cursor-init(target, {:$actions, %_}).TOP(|$args.Capture).MATCH,
              self!cursor-init(target, {:$actions, %_}).TOP().MATCH
            ),
          )
        )
    }

    method parsefile(Str(Cool) $filename, :$enc) {
        nqp::decont(nqp::getlexdyn('$/') = nqp::if(
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
