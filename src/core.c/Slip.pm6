# A Slip is a kind of List that is immediately incorporated into an iteration
# or another List. Other than that, it's a totally normal List.
my class Slip { # is List

    # XXX this makes an empty Slip undefined?
    multi method defined (Slip:D: --> Bool:D) { self.Bool }

    multi method Slip(Slip:D: --> Slip:D) { self }
    method CALL-ME (+args)     { args.Slip }
    multi method perl(Slip:D: --> Str:D) {
        nqp::if(
          nqp::eqaddr(self,Empty),
          'Empty',
          nqp::stmts(
            (my str $guts = callsame),
            nqp::if(
              nqp::eqat($guts,'$',0), # we're itemized
              nqp::concat('$(slip',nqp::concat(nqp::substr($guts,1),')')),
              nqp::concat('slip',$guts)
            )
          )
        )
    }
    multi method List(Slip:D: --> List:D) {
        nqp::stmts(
          (my $list := nqp::create(List)),
          nqp::if(
            nqp::isconcrete(nqp::getattr(self,List,'$!todo')),
            nqp::bindattr($list,List,'$!todo',
              nqp::getattr(self,List,'$!todo'))
          ),
          nqp::if(
            nqp::isconcrete(nqp::getattr(self,List,'$!reified')),
            nqp::bindattr($list,List,'$!reified',
              nqp::getattr(self,List,'$!reified'))
          ),
          $list
        )
    }
}

# The slip(...) function creates a Slip.
proto sub slip(|)     {*}
multi sub slip(--> Empty) { }
multi sub slip(@args --> Slip:D) { @args.Slip }
multi sub slip(+args --> Slip:D) { args.Slip }

# vim: ft=perl6 expandtab sw=4
