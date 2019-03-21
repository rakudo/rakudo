class Perl6::Metamodel::SubsetHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Documenting
    does Perl6::Metamodel::Stashing
{
    # The subset type or nominal type that we refine.
    has $!refinee;

    # The block implementing the refinement.
    has $!refinement;

    my $archetypes := Perl6::Metamodel::Archetypes.new( :nominalizable(1) );
    method archetypes() {
        $archetypes
    }

    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), |%named)
    }

    method BUILD(:$refinee, :$refinement) {
        $!refinee := $refinee;
        $!refinement := $refinement;
    }

    method new_type(:$name = '<anon>', :$refinee!, :$refinement!) {
        my $metasubset := self.new(:refinee($refinee), :refinement($refinement));
        my $type := nqp::settypehll(nqp::newtype($metasubset, 'Uninstantiable'), 'perl6');
        $metasubset.set_name($type, $name);
        nqp::settypecheckmode($type, 2);
        self.add_stash($type)
    }

    method set_of($obj, $refinee) {
        my $archetypes := $!refinee.HOW.archetypes;
        unless $archetypes.nominal || $archetypes.nominalizable {
            nqp::die("The 'of' type of a subset must either be a valid nominal " ~
                "type or a type that can provide one");
        }
        $!refinee := nqp::decont($refinee);
        if nqp::objprimspec($!refinee) {
            my %ex := nqp::gethllsym('perl6', 'P6EX');
            if nqp::existskey(%ex, 'X::NYI') {
                %ex{'X::NYI'}('Subsets of native types');
            }
            else {
                nqp::die("Subsets of native types NYI");
            }
        }
    }

    method refinee($obj) {
        $!refinee
    }

    method refinement($obj) {
        $!refinement
    }

    method isa($obj, $type) {
        $!refinee.isa($type)
            || nqp::hllboolfor(nqp::istrue($type.HOW =:= self), "perl6")
    }

    method nominalize($obj) {
        $!refinee.HOW.archetypes.nominal ??
            $!refinee !!
            $!refinee.HOW.nominalize($!refinee)
    }

    # Should have the same methods of the (eventually nominal) type
    # that we refine. (For the performance win, work out a way to
    # steal its method cache.)
    method find_method($obj, $name) {
        $!refinee.HOW.find_method($!refinee, $name)
    }

    # Do check when we're on LHS of smartmatch (e.g. Even ~~ Int).
    method type_check($obj, $checkee) {
        nqp::hllboolfor( nqp::istype($!refinee, $checkee), "perl6" )
    }

    # Here we check the value itself (when on RHS on smartmatch).
    method accepts_type($obj, $checkee) {
        note("accepts_type(", $obj.HOW.name($obj), ", ", $checkee.HOW.name($checkee), ")");
        note("refinement is: ", $!refinement.HOW.name($!refinement));
        # $!refinement.arep();

        # my &m := nqp::decont($!refinement.HOW.find_method($!refinement, 'ACCEPTS'));
        # nqp::say("Found ACCEPTS: " ~ &m.HOW.name(&m));
        # nqp::say("is dispatcher? " ~ &m.is_dispatcher);
        # my @cand := nqp::getattr(&m, $*W.find_symbol(['Routine']), '@!dispatchees');
        # nqp::say("candidates: " ~ +@cand);
        my $*DFBD := 1; # Debug Find Best Dispatchee
        my %mt := nqp::getattr($*W.find_symbol(['Code']).HOW, Perl6::Metamodel::ClassHOW, '%!methods');
        my $m := nqp::atkey(%mt, 'ACCEPTS');
        nqp::say("... From methods table: " ~ $m.HOW.name($m));
        nqp::say("... Method package: " ~ $m.package.HOW.name($m.package));
        nqp::say("... Is dispatcher? " ~ $m.is_dispatcher);
        nqp::say("ACCEPTS? " ~ nqp::callmethod($!refinement, 'ACCEPTS', $checkee));

        nqp::hllboolfor(
            nqp::istype($checkee, $!refinee) &&
            nqp::istrue($!refinement.ACCEPTS($checkee)),
            "perl6"
        )
    }
}
