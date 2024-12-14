# A Slip is a kind of List that is immediately incorporated into an iteration
# or another List. Other than that, it's a totally normal List.
my class Slip { # is List

    # XXX this makes an empty Slip undefined?
    multi method defined (Slip:D: --> Bool:D) { self.Bool }

    multi method Slip(Slip:D:) { self }
    multi method raku(Slip:D: --> Str:D) {
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
        my $list := nqp::create(List);
        nqp::bindattr($list,List,'$!todo',nqp::getattr(self,List,'$!todo'))
          if nqp::isconcrete(nqp::getattr(self,List,'$!todo'));
        nqp::bindattr($list,List,'$!reified',nqp::getattr(self,List,'$!reified'))
          if nqp::isconcrete(nqp::getattr(self,List,'$!reified'));
        $list
    }

    # shortcutting methods for better performance on Empty
    multi method are(Slip:D:) {
        nqp::eqaddr(self,Empty) ?? Nil !! nextsame
    }
    multi method batch(Slip:D:) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method batch(Slip:D: $) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method deepmap(Slip:D: &) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method duckmap(Slip:D: &) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method map(Slip:D: &) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method map(Slip:D: :&deep!) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method map(Slip:D: :&node!) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method map(Slip:D: :&flat!) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method map(Slip:D: :&duck!) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method first(Slip:D: &) {
        nqp::eqaddr(self,Empty) ?? Nil !! nextsame
    }
    multi method grep(Slip:D: &) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method head(Slip:D:) {
        nqp::eqaddr(self,Empty) ?? Nil !! nextsame
    }
    multi method head(Slip:D: $) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    method join(Slip:D: |) {
        nqp::eqaddr(self,Empty) ?? "" !! nextsame
    }
    multi method maxpairs(Slip:D:) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method minpairs(Slip:D:) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method nodemap(Slip:D: &) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method pairup(Slip:D:) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method repeated(Slip:D: |) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method rotor(Slip:D: |) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method skip(Slip:D:) {
        nqp::eqaddr(self,Empty) ?? Nil !! nextsame
    }
    multi method skip(Slip:D: $) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method sort(Slip:D:) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method sort(Slip:D: &) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method squish(Slip:D: |) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method tail(Slip:D:) {
        nqp::eqaddr(self,Empty) ?? Nil !! nextsame
    }
    multi method tail(Slip:D: $) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method toggle(Slip:D: &) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
    multi method unique(Slip:D: |) {
        nqp::eqaddr(self,Empty) ?? Empty !! nextsame
    }
}

# The slip(...) function creates a Slip.
proto sub slip(|)     {*}
multi sub slip(--> Empty) { }
multi sub slip(@args --> Slip:D) { @args.Slip }
multi sub slip(+args --> Slip:D) { args.Slip }

# vim: expandtab shiftwidth=4
