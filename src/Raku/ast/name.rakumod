# A name. Names range from simple (a single identifier) up to rather more
# complex (including pseudo-packages, interpolated parts, etc.)
class RakuAST::Name
  is RakuAST::ImplicitLookups
{
    has List $!parts;
    has List $.colonpairs;

    method new(*@parts) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Name, '$!parts', @parts);
        nqp::bindattr($obj, RakuAST::Name, '$!colonpairs', []);
        $obj
    }

    method from-identifier(Str $identifier) {
        self.new(RakuAST::Name::Part::Simple.new($identifier))
    }

    method from-identifier-parts(*@identifiers) {
        my @parts;
        for @identifiers {
            unless nqp::istype($_, Str) || nqp::isstr($_) {
                nqp::die('Expected identifier parts to be Str, but got ' ~ $_.HOW.name($_));
            }
            @parts.push(RakuAST::Name::Part::Simple.new($_));
        }
        self.new(|@parts)
    }

    method add-colonpair(RakuAST::ColonPairish $pair) {
        $!colonpairs.push: $pair;
    }

    method set-colonpairs(@pairs) {
        nqp::bindattr(self, RakuAST::Name, '$!colonpairs', @pairs);
    }

    method colonpairs() {
        self.IMPL-WRAP-LIST($!colonpairs)
    }

    method parts() {
        self.IMPL-WRAP-LIST($!parts)
    }

    method last-part() {
        $!parts[nqp::elems($!parts) - 1]
    }

    method root-part() {
        nqp::die("Can't get root-part of empty name") unless nqp::elems($!parts);
        my $root := $!parts[0];
        $root := $!parts[1] if nqp::elems($!parts) > 1 && nqp::istype($root, RakuAST::Name::Part::Empty);
        $root
    }

    method is-multi-part() {
        nqp::elems($!parts) > 1 && !(nqp::elems($!parts) == 2 && nqp::istype($!parts[0], RakuAST::Name::Part::Empty))
    }

    method is-identifier() {
        nqp::elems($!parts) == 1 && (
            nqp::istype($!parts[0], RakuAST::Name::Part::Simple)
            || nqp::istype($!parts[0], RakuAST::Name::Part::Expression)
                && $!parts[0].has-compile-time-name
                && nqp::index($!parts[0].name, '::') == -1
        )
        || nqp::elems($!parts) == 2 && (
            nqp::istype($!parts[0], RakuAST::Name::Part::Empty)
            && (
                nqp::istype($!parts[1], RakuAST::Name::Part::Simple)
                || nqp::istype($!parts[1], RakuAST::Name::Part::Expression)
                    && $!parts[1].has-compile-time-name
                    && nqp::index($!parts[1].name, '::') == -1
            )
        )
    }

    method is-empty() {
        nqp::elems($!parts) == 0 || (nqp::elems($!parts) == 1 && $!parts[0].is-empty)
    }

    method is-anonymous() {
        nqp::elems($!parts) == 2 && $!parts[0].is-empty && $!parts[1].is-empty # name is just '::'
    }

    method is-simple() {
        for $!parts {
            return False unless nqp::istype($_, RakuAST::Name::Part::Simple);
        }
        True
    }

    # Return the identifier if there is a single element identifier, or return
    # the empty string.  Can be used to either just check if the object is a
    # simple identifier, or also when the actual identifier is needed.
    method simple-identifier() {
        nqp::isconcrete(self)
          && nqp::elems($!parts) == 1
          && nqp::istype((my $obj := $!parts[0]),RakuAST::Name::Part::Simple)
          && $obj.name
    }

    method is-package-lookup() {
        nqp::elems($!parts)
          && nqp::istype($!parts[nqp::elems($!parts) - 1],RakuAST::Name::Part::Empty)
    }

    method base-name() {
        my @parts := nqp::clone($!parts);
        @parts.pop if self.is-package-lookup;
        my $name := RakuAST::Name.new(|@parts);
        for $!colonpairs {
            $name.add-colonpair($_);
        }
        $name
    }

    method is-indirect-lookup() {
        for $!parts {
            return True if nqp::istype($_, RakuAST::Name::Part::Expression);
        }
    }

    method indirect-lookup-part() {
        nqp::istype($!parts[0], RakuAST::Name::Part::Empty)
            ?? $!parts[1]
            !! $!parts[0]
    }

    method has-colonpairs() {
        nqp::hllboolfor(nqp::elems($!colonpairs), 'Raku')
    }

    method has-colonpair($key) {
        for $!colonpairs {
            return True if $_.key eq $key;
        }
        False
    }

    method first-colonpair($key) {
        for $!colonpairs {
            return $_ if $_.key eq $key;
        }
        Nil
    }

    method without-colonpair($key) {
        my @parts := nqp::clone($!parts);
        my $type := RakuAST::Name.new(|@parts);
        for $!colonpairs {
            $type.add-colonpair($_) if !nqp::istype($_, RakuAST::ColonPair) || $_.key ne $key;
        }
        $type
    }

    method without-colonpairs() {
        my @parts := nqp::clone($!parts);
        my $type := RakuAST::Name.new(|@parts);
        for $!colonpairs {
            $type.add-colonpair($_)
              unless nqp::istype($_, RakuAST::ColonPair);
        }
        $type
    }

    method without-first-part() {
        my @parts := nqp::clone($!parts);
        @parts.shift;
        my $type := RakuAST::Name.new(|@parts);
        for $!colonpairs {
            $type.add-colonpair($_)
        }
        $type
    }

    method visit-children(Code $visitor) {
        if nqp::isconcrete(self) {
            for $!parts {
                $_.visit-children($visitor);
            }
            for $!colonpairs {
                $visitor($_);
            }
        }
    }

    method colonpair-suffix() {
        my $name := '';
        for $!colonpairs -> $cp {
            if nqp::istype($cp, RakuAST::ColonPairish) {
                $name := $name ~ ':' ~ $cp.canonicalize;
                CATCH {
                    if nqp::istype(nqp::getpayload($_), RakuAST::Exception::TooComplex) {
                        my $content := '';
                        $cp.visit-children(-> $child {
                            $content := $content ~ $child.DEPARSE;
                        });
                        nqp::getpayload($_).set-name($content);
                    }
                    nqp::rethrow($_);
                }
            }
            elsif nqp::istype($cp, RakuAST::Term::Name) && $cp.name.canonicalize eq 'Nil' {
                $name := $name ~ ':<>'
            }
            else {
                nqp::die('canonicalize NYI for non-simple colonpairs: ' ~ $cp.HOW.name($cp));
            }
        }
        $name
    }

    method canonicalize(:$colonpairs) {
        my $canon-parts := nqp::list_s();
        my $first := 1;
        for $!parts {
            if nqp::istype($_, RakuAST::Name::Part::Simple) {
                nqp::push_s($canon-parts, $_.name);
            }
            elsif nqp::istype($_, RakuAST::Name::Part::Empty) {
                nqp::push_s($canon-parts, '') unless $first;
            }
            elsif nqp::istype($_, RakuAST::Name::Part::Expression) {
                if $_.has-compile-time-name {
                    nqp::push_s($canon-parts, $_.name);
                }
                else {
                    nqp::push_s($canon-parts, '') if nqp::elems($!parts) == 1;
                    my $expr := $_.expr;
                    nqp::push_s($canon-parts, '(' ~ ($expr.origin ?? $expr.origin.Str !! $expr.DEPARSE) ~ ')');
                }
            }
            else {
                nqp::die('canonicalize NYI for non-simple name part ' ~ $_.HOW.name($_));
            }
            $first := 0;
        }
        my $name := nqp::join('::', $canon-parts);
        unless nqp::isconcrete($colonpairs) && !$colonpairs {
            $name := $name ~ self.colonpair-suffix;
        }
        $name
    }

    method is-pseudo-package() {
        nqp::istype($!parts[0], RakuAST::Name::Part::Simple) && $!parts[0].is-pseudo-package
        || nqp::istype($!parts[0], RakuAST::Name::Part::Empty)
    }

    method is-package-search() {
        nqp::istype($!parts[0], RakuAST::Name::Part::Empty)
    }

    method qualified-with(RakuAST::Name $target) {
        if self.is-global-lookup {
            self.without-first-part
        }
        else {
            my $qualified := nqp::clone(self);
            my @parts := nqp::clone(nqp::getattr($target, RakuAST::Name, '$!parts'));
            for $!parts {
                nqp::push(@parts, $_);
            }
            nqp::bindattr($qualified, RakuAST::Name, '$!parts', @parts);
            $qualified
        }
    }

    method is-global-lookup() {
        nqp::istype($!parts[0], RakuAST::Name::Part::Simple) && $!parts[0].name eq 'GLOBAL'
    }

    method contains-pseudo-package-illegal-for-declaration() {
        return 'GLOBAL' if self.is-identifier && $!parts[0].name eq 'GLOBAL';
        for $!parts {
            return $_.name
                if $_.is-pseudo-package
                || nqp::istype($_, RakuAST::Name::Part::Simple) && $_.name eq 'PROCESS';
        }
        Nil
    }

    method dump-markers() {
        '【' ~ self.canonicalize ~ '】'
    }

    method IMPL-IS-NQP-OP() {
        nqp::elems($!parts) == 2 && nqp::istype($!parts[0], RakuAST::Name::Part::Simple) && $!parts[0].name eq 'nqp'
            ?? $!parts[1].name
            !! ''
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('&INDIRECT_NAME_LOOKUP')),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('PseudoStash')),
        ])
    }

    method IMPL-LOOKUP-PARTS() {
        my @parts := nqp::clone($!parts);
        nqp::shift(@parts) if nqp::istype(@parts[0], RakuAST::Name::Part::Empty);
        if nqp::elems(@parts) && nqp::elems($!colonpairs) {
            my $final := nqp::pop(@parts);
            $final := RakuAST::Name.from-identifier($final.name);
            $final.set-colonpairs($!colonpairs);
            nqp::push(@parts, RakuAST::Name::Part::Simple.new($final.canonicalize));
        }
        @parts
    }

    method IMPL-QAST-PSEUDO-PACKAGE-LOOKUP(RakuAST::IMPL::QASTContext $context, str :$sigil) {
        my @parts := self.IMPL-LOOKUP-PARTS;
        my $final := @parts[nqp::elems(@parts) - 1];
        my $PseudoStash-lookup := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[1];
        my $result;
        if $*IMPL-COMPILE-DYNAMICALLY && $!parts[0].name eq 'CORE' {
            nqp::shift($!parts); #FIXME don't modify please
            my $PseudoStash := $PseudoStash-lookup.resolution.compile-time-value;
            my $package := Perl6::Metamodel::ModuleHOW.new_type(:name('CORE'));
            my $found-ctx := $context.setting;
            my $stash := $PseudoStash.new-from-ctx($found-ctx, :$package);
            $context.ensure-sc($stash);
            $result := QAST::WVal.new(:value($stash));
        }
        else {
            $result := QAST::Op.new(
              :op<callmethod>,
              :name<new>,
              $PseudoStash-lookup.IMPL-TO-QAST($context)
            );
        }
        my int $first := 1;
        for @parts {
            if $first { # don't call .WHO on the pseudo package itself, index into it instead
                $first := 0;
            }
            else { # get the Stash from all real packages
                $result := QAST::Op.new( :op('who'), $result );
            }
            $result := $_.IMPL-QAST-PSEUDO-PACKAGE-LOOKUP-PART($context, $result, $_ =:= $final, :$sigil);
        }
        $result
    }

    method IMPL-QAST-PACKAGE-LOOKUP(RakuAST::IMPL::QASTContext $context, Mu $start-package, RakuAST::Declaration :$lexical, str :$sigil, Bool :$global-fallback) {
        my $result;
        my $final := $!parts[nqp::elems($!parts) - 1];
        my int $first;
        if self.is-global-lookup {
            $result := QAST::Op.new(:op<getcurhllsym>, QAST::SVal.new(:value<GLOBAL>));
            $first := 1;
        }
        elsif $lexical {
            nqp::die('Mismatch between lexical and package name')
                unless $lexical.lexical-name eq $!parts[0].name;
            $first := 1;
            $result := $lexical.IMPL-LOOKUP-QAST($context);
        }
        else {
            $result := QAST::WVal.new(:value($start-package));
        }
        if self.is-pseudo-package {
            $result := self.IMPL-QAST-PSEUDO-PACKAGE-LOOKUP($context, :$sigil);
        }
        else {
            for $!parts {
                if $first { # skip GLOBAL or lexically found first part, already taken care of
                    $first := 0;
                }
                else { # get the Stash from all real packages
                    # We do .WHO on the current package, followed by the index into it.
                    $result := QAST::Op.new( :op('who'), $result );
                    $result := $_.IMPL-QAST-PACKAGE-LOOKUP-PART($context, $result, $_ =:= $final, :$sigil, :$global-fallback);
                }
            }
        }
        $result
    }

    method IMPL-QAST-INDIRECT-LOOKUP(RakuAST::IMPL::QASTContext $context, str :$sigil) {
        my @parts   := self.IMPL-LOOKUP-PARTS;
        my $final   := @parts[nqp::elems(@parts) - 1];
        my $lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups);
        my $result  := QAST::Op.new(
            :op<call>,
            $lookups[0].IMPL-TO-QAST($context),
            QAST::Op.new(
                :op<callmethod>,
                :name<new>,
                $lookups[1].IMPL-TO-QAST($context),
            ),
        );
        nqp::push($result, QAST::SVal.new(:value($sigil))) if $sigil;
        for @parts {
            nqp::push($result, $_.IMPL-QAST-INDIRECT-LOOKUP-PART($context, $result, $_ =:= $final));
        }
        $result
    }
}

# Marker role for a part of a name.
class RakuAST::Name::Part {
    method is-pseudo-package() {
        False
    }

    method visit-children(Code $visitor) {
    }

    method is-empty() { # returns Bool
        nqp::die("is-empty not implemented on " ~ self.HOW.name(self));
    }
}

# A simple name part, wrapping a string name.
class RakuAST::Name::Part::Simple
  is RakuAST::Name::Part
{
    has str $.name;

    method new(Str $name) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Name::Part::Simple, '$!name', $name);
        $obj
    }

    method is-pseudo-package() {
        my $name := $!name;
           $name eq 'CALLER'
        || $name eq 'CALLERS'
        || $name eq 'CLIENT'
        || $name eq 'DYNAMIC'
        || $name eq 'CORE'
        || $name eq 'LEXICAL'
        || $name eq 'MY'
        || $name eq 'OUR'
        || $name eq 'OUTER'
        || $name eq 'OUTERS'
        || $name eq 'SETTING'
        || $name eq 'UNIT'
        || $name eq 'COMPILING' # seems to be reserved
    }

    method is-empty() {
        $!name eq ''
    }

    method IMPL-QAST-PACKAGE-LOOKUP-PART(RakuAST::IMPL::QASTContext $context, Mu $stash-qast, Int $is-final, str :$sigil, Bool :$global-fallback) {
        my $op := QAST::Op.new(
            :op('callmethod'),
            :name($is-final ?? 'AT-KEY' !! 'package_at_key'),
            $stash-qast,
            QAST::SVal.new( :value($is-final && $sigil ?? $sigil ~ $!name !! $!name) )
        );
        $op.push(QAST::WVal.new(:value(True), :named<global_fallback>)) if $is-final && $global-fallback;
        $op
    }

    method IMPL-QAST-PSEUDO-PACKAGE-LOOKUP-PART(RakuAST::IMPL::QASTContext $context, Mu $stash-qast, Int $is-final, str :$sigil) {
        QAST::Op.new(
            :op('call'),
            :name('&postcircumfix:<{ }>'),
            $stash-qast,
            QAST::SVal.new( :value($is-final && $sigil ?? $sigil ~ $!name !! $!name) )
        )
    }

    method IMPL-QAST-INDIRECT-LOOKUP-PART(RakuAST::IMPL::QASTContext $context, Mu $stash-qast, Int $is-final, str :$sigil) {
        QAST::SVal.new( :value($is-final && $sigil ?? $sigil ~ $!name !! $!name) )
    }
}

class RakuAST::Name::Part::Expression
  is RakuAST::Name::Part
{
    has RakuAST::Expression $.expr;

    method new(RakuAST::Expression $expr) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Name::Part::Expression, '$!expr', $expr);
        $obj
    }

    method IMPL-QAST-PSEUDO-PACKAGE-LOOKUP-PART(RakuAST::IMPL::QASTContext $context, Mu $stash-qast, Int $is-final, str :$sigil) {
        QAST::Op.new(
            :op('call'),
            :name('&postcircumfix:<{ }>'),
            $stash-qast,
            $!expr.IMPL-TO-QAST($context),
        )
    }

    method IMPL-QAST-PACKAGE-LOOKUP-PART(RakuAST::IMPL::QASTContext $context, Mu $stash-qast, Int $is-final, str :$sigil, Bool :$global-fallback) {
        QAST::Op.new(
            :op('callmethod'),
            :name($is-final ?? 'AT-KEY' !! 'package_at_key'),
            $stash-qast,
            $!expr.IMPL-TO-QAST($context),
        )
    }

    method IMPL-QAST-INDIRECT-LOOKUP-PART(RakuAST::IMPL::QASTContext $context, Mu $stash-qast, Int $is-final, str :$sigil) {
        $!expr.IMPL-TO-QAST($context)
    }

    method visit-children(Code $visitor) {
        $visitor($!expr);
    }

    method has-compile-time-name() {
        nqp::defined(try $!expr.literalize)
    }

    method name() {
        $!expr.literalize // nqp::die('Name ' ~ $!expr.DEPARSE ~ ' is not compile-time known')
    }

    method is-empty() {
        my $name := $!expr.literalize;
        nqp::defined($!expr) && $!expr eq ''
    }
}

# An empty name part, implying .WHO
class RakuAST::Name::Part::Empty
  is RakuAST::Name::Part
{
    method new() {
        nqp::create(self);
    }

    method IMPL-QAST-PSEUDO-PACKAGE-LOOKUP-PART(RakuAST::IMPL::QASTContext $context, Mu $stash-qast, Int $is-final, str :$sigil) {
        $stash-qast
    }

    method IMPL-QAST-PACKAGE-LOOKUP-PART(RakuAST::IMPL::QASTContext $context, Mu $stash-qast, Int $is-final, str :$sigil, Bool :$global-fallback) {
        $stash-qast
    }

    method name() { "" }

    method is-empty() { True }
}
