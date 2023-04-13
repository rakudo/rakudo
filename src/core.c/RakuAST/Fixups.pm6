# This file contains augmentations to classes that are created in the
# RakuAST bootstrap to allow a lot of logic (which will **NOT** be
# needed to compile the # Raku setting) to be written in Raku rather
#than in NQP.

augment class RakuAST::Doc::Declarator {

    # This method will make a legacy compatible Pod object, including
    # all of its weird nesting and use of mutable Arrays rather than
    # immutable Lists
    method make-legacy-pod($WHEREFORE) {
        sub normalize(@paragraphs) {
            @paragraphs.map(*.lines.map({.trim if $_}).Slip).join(' ')
        }
        my $pod := Pod::Block::Declarator.new(
          WHEREFORE => $WHEREFORE,
          leading   => [%*ENV<RAKUDO_POD_DECL_BLOCK_USER_FORMAT>
            ?? self.leading.join("\n")
            !! normalize(self.leading)
          ],
          trailing  => [[normalize self.trailing],]
        );
        $WHEREFORE.set_why($pod);
        $pod
    }

    # Legacy $=pod requires an Array, so return that
    method initialize-legacy-pods() is raw { [] }
}

# vim: expandtab shiftwidth=4
