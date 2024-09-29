#-------------------------------------------------------------------------------
# A subroutine that is also an operator

my class Operator { # declared in BOOTSTRAP
    # class Operator is Sub
    #     has Mu $!properties;

    # Old style prec hash, should probably be DEPRECATED
    method prec(Operator:D: |c --> Hash:D) { self.properties.prec(|c) }

    # Return the OperatorProperties of the proto of the invocant
    method properties(Operator:D:
      --> OperatorProperties) is implementation-detail {
        $!properties // OperatorProperties
    }

    method precedence(Operator:D:   --> Str:D) { $!properties.precedence  }
    method associative(Operator:D:  --> Str:D) { $!properties.associative }
    method thunky(Operator:D:       --> Str:D) { $!properties.thunky      }
    method iffy(Operator:D:        --> Bool:D) { $!properties.iffy.Bool   }

    method reducer(Operator:D: --> Callable:D) { ::($!properties.reducer) }

    # Set operator properties, usually called through trait_mods
    method equiv(Operator:D: &op --> Nil) {
        nqp::bindattr(self.proto,Operator,'$!properties',
          &op.properties.equiv(self.associative)
        )
    }
    method tighter(Operator:D: &op --> Nil) {
        nqp::bindattr(self.proto,Operator,'$!properties',
          &op.properties.tighter(self.associative)
        )
    }
    method looser(Operator:D: &op --> Nil) {
        nqp::bindattr(self.proto,Operator,'$!properties',
          &op.properties.looser(self.associative)
        )
    }
    method assoc(Operator:D: Str:D $associative --> Nil) {
        nqp::bindattr(self.proto,Operator,'$!properties',
          self.properties.new(:$associative))
    }

    proto method set-properties(|) {*}
    multi method set-properties(Operator:D:) {
        (my str $type, my str $name) = self.name.split(":",2);
        $name = nqp::eqat($name,'<<',0)
          ?? nqp::substr($name,2,nqp::chars($name) - 4)
          !! nqp::substr($name,1,nqp::chars($name) - 2);
        nqp::bindattr(self,Operator,'$!properties',
          OperatorProperties."$type"($name))
    }
    multi method set-properties(Operator:D: OperatorProperties:D $properties) {
        nqp::bindattr(self,Operator,'$!properties',$properties)
    }

    # Helper method to apply a trait by name and given operator target string
    # using information of target operator of the same category
    method apply-operator-trait(Operator:D:
      Str:D $trait, Str:D $target --> Nil
    ) is implementation-detail {
        my str $name  = self.name;
        my int $index = nqp::index($name,':');
        die "Operator given to 'is $trait' does not appear to be an operator"
          if $index < 0;

        my $fqn := '&'
          ~ nqp::substr($name,0,$index)
          ~ ($target.contains('<') || $target.contains('>')
              ?? ":«$target»"
              !! ":<$target>"
            );
        nqp::istype((my $op := ::($fqn)),Failure)
          ?? $op.throw
          !! self."$trait"($op)
    }
}

multi sub trait_mod:<is>(Operator:D $o, :&equiv! --> Nil) {
    $o.equiv(&equiv)
}
multi sub trait_mod:<is>(Operator:D $o, Str:D :$equiv! --> Nil) {
    $o.apply-operator-trait('equiv', $equiv)
}

multi sub trait_mod:<is>(Operator:D $o, :&tighter! --> Nil) {
    $o.tighter(&tighter)
}
multi sub trait_mod:<is>(Operator:D $o, Str:D :$tighter! --> Nil) {
    $o.apply-operator-trait('tighter', $tighter)
}

multi sub trait_mod:<is>(Operator:D $o, :&looser! --> Nil) {
    $o.looser(&looser)
}
multi sub trait_mod:<is>(Operator:D $o, Str:D :$looser! --> Nil) {
    $o.apply-operator-trait('looser', $looser)
}

multi sub trait_mod:<is>(Operator:D $o, :$assoc! --> Nil) {
    $o.assoc($assoc)
}

# vim: expandtab shiftwidth=4
