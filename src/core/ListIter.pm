my class ListIter {
    # Attributes defined in BOOTSTRAP.pm:
    #   has $!reified;         # return value for already-reified iterator
    #   has $!nextiter;        # next iterator in sequence, if any
    #   has Mu $!rest;         # RPA of elements remaining to be reified
    #   has $!list;            # List object associated with this iterator
    
    method reify($n is copy) {
        if !$!reified.defined {
            my $rpa := pir::new__Ps('ResizablePMCArray');
            my Mu $x;
            my $pos = $!list.gimme(0) if $!list.defined;
            my $flattens = $!list.defined && $!list.flattens;
            my $eager = Whatever.ACCEPTS($n);
            while $!rest && ($eager || $n > 0) {
                $x := nqp::atpos($!rest, 0);
                if pir::not__II(pir::is_container__IP($x)) && $x.defined
                    && Iterable.ACCEPTS($x) {
                        last if $eager && $x.infinite;
                        pir::splice__vPPii(
                            $!rest, 
                            pir__perl6_unbox_rpa__PP($x.iterator.reify($n)),
                            0, 1);
                }
                elsif $flattens && pir::not__II(pir::is_container__IP($x))
                      && $x.defined && Parcel.ACCEPTS($x) {
                        pir::splice__vPPii(
                            $!rest, 
                            pir__perl6_unbox_rpa__PP($x),
                            0, 1);
                }
                elsif Nil.ACCEPTS($x) { nqp::shift($!rest) }
                else {
                    nqp::shift($!rest);
                    $x := $!list.STORE_AT_POS($pos, $x) if $!list.defined;
                    pir::push__vPP($rpa, $x);
                    $eager or $n = $n - 1;
                    $pos = $pos + 1;
                }
            }
            pir::push__vPP( $rpa, 
                    pir::setattribute__3PPsP(self, ListIter, '$!nextiter',
                        pir::perl6_iter_from_rpa__PPPP($!rest, $!list)))
                if $!rest;
            # define our $!reified Parcel.  infix:<:=> doesn't seem to work 
            # on attributes defined in BOOTSTRAP, so use pir::setattribute.
            pir::setattribute__0PPsP( self, ListIter, '$!reified',
                pir__perl6_box_rpa__PP($rpa));
            # free up $!list and $!rest
            pir::setattribute__0PPsP(self, ListIter, '$!list', Mu);
            pir::setattribute__0PPsP(self, ListIter, '$!rest', Mu);
        }
        $!reified;
    }

    method infinite() {
        $!rest 
          ?? Iterable.ACCEPTS(nqp::atpos($!rest,0))
             && nqp::atpos($!rest,0).infinite
             || Mu
          !! 0.Bool
    }

    method iterator() { self }
    method nextiter() { $!nextiter }

    multi method DUMP(ListIter:D:) {
        self.DUMP-ID() ~ '('
          ~ ':reified('  ~ DUMP($!reified) ~ '), '
          ~ ':rest('     ~ DUMP($!rest) ~ '), '
          ~ ':list('     ~ $!list.DUMP-ID() ~ ')'
          ~ ')'
    }
         
}


# C<pir__perl6_list_from_rpa> and C<pir__perl6_iter_from_rpa> are now opcodes;
# we leave these here for documentation purposes.
# sub pir::perl6_list_from_rpa__PPPP(|$) {
#     my $args     := pir::perl6_current_args_rpa__P();
#     my $type     := nqp::shift($args);
#     my Mu $rpa   := nqp::shift($args);
#     my $flattens := nqp::shift($args);
# 
#     my $list := pir::repr_instance_of__PP($type);
#     pir::setattribute__vPPsP($list, $type, '$!flattens', $flattens);
#     pir::setattribute__vPPsP($list, $type, '$!nextiter',
#         pir__perl6_iter_from_rpa__PPP($rpa, $list)
#     )
# }
# 
# sub pir::perl6_iter_from_rpa__PPP(|$) {
#     my $args     := pir::perl6_current_args_rpa__P();
#     my Mu $rpa   := nqp::shift($args);
#     my $list     := nqp::shift($args);
# 
#     pir::setattribute__0PPsP(
#         pir::setattribute__0PPsP(
#             pir::repr_instance_of__PP(ListIter),
#             ListIter, '$!rest', $rpa),
#         ListIter, '$!list', $list)
# }
