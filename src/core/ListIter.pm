my class ListIter {
    # Attributes defined in BOOTSTRAP.pm:
    #   has $!reified;         # return value for already-reified iterator
    #   has $!nextiter;        # next iterator in sequence, if any
    #   has Mu $!rest;         # RPA of elements remaining to be reified
    #   has $!list;            # List object associated with this iterator
    
    method reify($n is copy, :$sink) {
        if !$!reified.defined {
            my $eager = Whatever.ACCEPTS($n);
            my $flattens = $!list.defined && $!list.flattens;
            my $count = $eager ?? 100000 !! $n;
            my $rpa := nqp::list();
            my Mu $x;
            my $index;
            while $!rest && nqp::islt_i(nqp::elems($rpa), nqp::unbox_i($count)) {
                $index = nqp::p6box_i(
                             pir::perl6_rpa_find_type__IPPii(
                                 $!rest, Iterable, 0, nqp::unbox_i($count)));
                $index = nqp::p6box_i(
                             pir::perl6_rpa_find_type__IPPii(
                                 $!rest, Parcel, 0, nqp::unbox_i($index)))
                    if $flattens;
                pir::perl6_shiftpush__0PPi($rpa, $!rest, nqp::unbox_i($index));
                if $!rest && nqp::islt_i(nqp::elems($rpa), nqp::unbox_i($count)) {
                    $x := nqp::shift($!rest);
                    if nqp::isconcrete($x) {
                        last if $eager && $x.infinite;
                        $x := $x.iterator.reify($count) if nqp::istype($x, Iterable);
                        nqp::splice($!rest, nqp::getattr($x, Parcel, '$!storage'), 0, 0);
                    
                    }
                    elsif nqp::not_i(nqp::istype($x, Nil)) {
                        nqp::push($rpa, $x);
                    }
                }
            }
            my $reified := pir__perl6_box_rpa__PP($rpa);
            $reified := $!list.REIFY($reified) if $!list.defined && !$sink;
            nqp::push(
                    nqp::getattr($reified, Parcel, '$!storage'),
                    nqp::bindattr(self, ListIter, '$!nextiter',
                                  nqp::p6listiter($!rest, $!list)))
                if $!rest;
            nqp::bindattr(self, ListIter, '$!reified', $reified);
            # free up $!list and $!rest
            nqp::bindattr(self, ListIter, '$!list', Mu);
            nqp::bindattr(self, ListIter, '$!rest', Mu);
        }
        $!reified;
    }

    method infinite() {
        $!rest 
          ?? nqp::istype(nqp::atpos($!rest, 0), Iterable)
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
