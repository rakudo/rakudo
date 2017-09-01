# A Slip is a kind of List that is immediately incorporated into an iteration
# or another List. Other than that, it's a totally normal List.
my class Slip { # is List
    multi method Slip(Slip:D:) { self }
    method defined ()          { self.so }
    method CALL-ME (+args)     { args.Slip }
    multi method perl(Slip:D:) {
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
    multi method List(Slip:D:) {
        nqp::stmts(
          (my $list := nqp::create(List)),
          nqp::if(
            nqp::getattr(self,List,'$!todo').DEFINITE,
            nqp::bindattr($list,List,'$!todo',
              nqp::getattr(self,List,'$!todo'))
          ),
          nqp::if(
            nqp::getattr(self,List,'$!reified').DEFINITE,
            nqp::bindattr($list,List,'$!reified',
              nqp::getattr(self,List,'$!reified'))
          ),
          $list
        )
    }
}

# The slip(...) function creates a Slip.
proto slip(|)     { * }
multi slip()      { Empty }
multi slip(@args) { @args.Slip }
multi slip(+args) { args.Slip }

# vim: ft=perl6 expandtab sw=4
