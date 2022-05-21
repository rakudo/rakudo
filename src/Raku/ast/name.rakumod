# A name. Names range from simple (a single identifier) up to rather more
# complex (including pseudo-packages, interpolated parts, etc.)
class RakuAST::Name is RakuAST::ImplicitLookups {
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

    method add-colonpair(RakuAST::ColonPair $pair) {
        $!colonpairs.push: $pair;
    }

    method parts() {
        self.IMPL-WRAP-LIST($!parts)
    }

    method is-identifier() {
        nqp::elems($!parts) == 1 && nqp::istype($!parts[0], RakuAST::Name::Part::Simple)
    }

    method is-empty() {
        nqp::elems($!parts) ?? False !! True
    }

    method is-simple() {
        for $!parts {
            return False unless nqp::istype($_, RakuAST::Name::Part::Simple);
        }
        True
    }

    method is-package-lookup() {
        nqp::elems($!parts) && nqp::istype($!parts[nqp::elems($!parts) - 1], RakuAST::Name::Part::Empty)
    }

    method has-colonpair($key) {
        for $!colonpairs {
            return True if $_.key eq $key;
        }
        False
    }

    method visit-children(Code $visitor) {
        if nqp::isconcrete(self) {
            for $!colonpairs {
                $visitor($_);
            }
        }
    }

    method canonicalize() {
        my $canon-parts := nqp::list_s();
        for $!parts {
            if nqp::istype($_, RakuAST::Name::Part::Simple) {
                nqp::push_s($canon-parts, $_.name);
            }
            elsif nqp::istype($_, RakuAST::Name::Part::Empty) {
                nqp::push_s($canon-parts, '');
            }
            else {
                nqp::die('canonicalize NYI for non-simple name parts');
            }
        }
        my $name := nqp::join('::', $canon-parts);
        for $!colonpairs {
            if nqp::istype($_, RakuAST::ColonPair) {
                $name := $name ~ ':' ~ $_.named-arg-name ~ '<' ~ $_.named-arg-value ~ '>';
            }
            elsif nqp::istype($_, RakuAST::QuotedString) {
                $name := $name ~ ':<' ~ $_.literal-value ~ '>';
            }
            else {
                nqp::die('canonicalize NYI for non-simple colonpairs');
            }
        }
        $name
    }

    method is-pseudo-package() {
        nqp::istype($!parts[0], RakuAST::Name::Part::Simple) && $!parts[0].is-pseudo-package
    }

    method IMPL-IS-NQP-OP() {
        nqp::elems($!parts) == 2 && nqp::istype($!parts[0], RakuAST::Name::Part::Simple) && $!parts[0].name eq 'nqp'
            ?? $!parts[1].name
            !! ''
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST(
            self.is-simple && !self.is-pseudo-package
                ?? []
                !! [
                    RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('&INDIRECT_NAME_LOOKUP')),
                    RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('PseudoStash')),
                ]
        )
    }

    method IMPL-QAST-PACKAGE-LOOKUP(RakuAST::IMPL::QASTContext $context, Mu $start-package) {
        my $result := $start-package;
        my $final := $!parts[nqp::elems($!parts) - 1];
        if self.is-pseudo-package {
            my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups());
            my $PseudoStash := @lookups[1];
            $result := QAST::Op.new(
                :op<callmethod>,
                :name<new>,
                $PseudoStash.IMPL-TO-QAST($context),
            );
            my int $first := 1;
            for $!parts {
                if $first { # don't call .WHO on the pseudo package itself, index into it instead
                    $first := 0;
                }
                else { # get the Stash from all real packages
                    $result := QAST::Op.new( :op('who'), $result );
                }
                $result := $_.IMPL-QAST-PSEUDO-PACKAGE-LOOKUP-PART($context, $result, $_ =:= $final);
            }
        }
        else {
            for $!parts {
                # We do .WHO on the current package, followed by the index into it.
                $result := QAST::Op.new( :op('who'), $result );
                $result := $_.IMPL-QAST-PACKAGE-LOOKUP-PART($context, $result, $_ =:= $final);
            }
        }
        $result
    }
}

# Marker role for a part of a name.
class RakuAST::Name::Part {
}

# A simple name part, wrapping a string name.
class RakuAST::Name::Part::Simple is RakuAST::Name::Part {
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
    }

    method IMPL-QAST-PACKAGE-LOOKUP-PART(RakuAST::IMPL::QASTContext $context, Mu $stash-qast, Int $is-final, str :$sigil) {
        QAST::Op.new(
            :op('callmethod'),
            :name($is-final ?? 'AT-KEY' !! 'package_at_key'),
            $stash-qast,
            QAST::SVal.new( :value($is-final && $sigil ?? $sigil ~ $!name !! $!name) )
        )
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

class RakuAST::Name::Part::Expression is RakuAST::Name::Part {
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

    method IMPL-QAST-PACKAGE-LOOKUP-PART(RakuAST::IMPL::QASTContext $context, Mu $stash-qast, Int $is-final, str :$sigil) {
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
}

# An empty name part, implying .WHO
class RakuAST::Name::Part::Empty is RakuAST::Name::Part {

    method new() {
        nqp::create(self);
    }

    method IMPL-QAST-PSEUDO-PACKAGE-LOOKUP-PART(RakuAST::IMPL::QASTContext $context, Mu $stash-qast, Int $is-final, str :$sigil) {
        $stash-qast
    }

    method IMPL-QAST-PACKAGE-LOOKUP-PART(RakuAST::IMPL::QASTContext $context, Mu $stash-qast, Int $is-final, str :$sigil) {
        $stash-qast
    }
}
