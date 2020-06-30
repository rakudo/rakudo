# Done by anything that implies a lexical scope.
class RakuAST::LexicalScope is RakuAST::Node {
    has List $!declarations-cache;
    has Mu $!lexical-lookup-hash;

    method IMPL-QAST-DECLS(RakuAST::IMPL::QASTContext $context) {
        my $stmts := QAST::Stmts.new();

        # Visit code objects that need to make a declaration entry.
        my $inner-code := self.visit(-> $node {
            if nqp::istype($node, RakuAST::Code) {
                $stmts.push($node.IMPL-QAST-DECL-CODE($context));
            }
            !(nqp::istype($node, RakuAST::LexicalScope) || nqp::istype($node, RakuAST::IMPL::ImmediateBlockUser))
        });

        # Visit declarations.
        for self.IMPL-UNWRAP-LIST(self.lexical-declarations()) {
            $stmts.push($_.IMPL-QAST-DECL($context));
        }

        $stmts
    }

    method lexical-declarations() {
        unless nqp::isconcrete($!declarations-cache) {
            nqp::bindattr(self, RakuAST::LexicalScope, '$!declarations-cache',
                self.find-nodes(RakuAST::Declaration::Lexical,
                    stopper => RakuAST::LexicalScope));
        }
        $!declarations-cache
    }

    method find-lexical(Str $name) {
        my %lookup := $!lexical-lookup-hash;
        unless nqp::isconcrete(%lookup) {
            %lookup := {};
            for self.IMPL-UNWRAP-LIST(self.lexical-declarations) {
                %lookup{$_.lexical-name} := $_ unless $_.anonymous;
            }
            nqp::bindattr(self, RakuAST::LexicalScope, '$!lexical-lookup-hash', %lookup);
        }
        %lookup{$name} // Nil
    }
}

# Done by anything that is a declaration - that is, declares a symbol.
class RakuAST::Declaration is RakuAST::Node {
}

# Done by a declaration that installs a lexical symbol.
class RakuAST::Declaration::Lexical is RakuAST::Declaration {
    method lexical-name() {
        nqp::die("Lexical name not implemented for " ~ self.HOW.name(self))
    }

    # Some things that are typically lexical declarations may also come in
    # anonymous forms. In that case, they may override this method to return
    # True in such a case.
    method anonymous() { False }
}

# A lexical declaration that comes from an external symbol (for example, the
# setting or an EVAL). XXX May break out the setting one.
class RakuAST::Declaration::External is RakuAST::Declaration::Lexical {
    has str $.lexical-name;
    has Mu $!native-type;

    method new(str :$lexical-name, Mu :$native-type) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration::External, '$!lexical-name', $lexical-name);
        nqp::bindattr($obj, RakuAST::Declaration::External, '$!native-type', $native-type);
        $obj
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context, Mu :$rvalue) {
        my str $scope := 'lexical';
        unless $rvalue {
            # Potentially l-value native lookups need a lexicalref.
            if nqp::objprimspec($!native-type) {
                $scope := 'lexicalref';
            }
        }
        QAST::Var.new( :name($!lexical-name), :$scope, :returns($!native-type) )
    }
}

# A lexical declaration that comes with an external symbol, which has a fixed
# value available during compilation.
class RakuAST::Declaration::External::Constant is RakuAST::Declaration::External
        is RakuAST::CompileTimeValue {
    has Mu $.compile-time-value;

    method new(str :$lexical-name!, Mu :$compile-time-value!) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration::External, '$!lexical-name', $lexical-name);
        nqp::bindattr($obj, RakuAST::Declaration::External::Constant,
            '$!compile-time-value', $compile-time-value);
        $obj
    }
}

# Done by anything that is a lookup of a symbol. May or may not need resolution
# at compile time.
class RakuAST::Lookup is RakuAST::Node {
    has RakuAST::Declaration $!resolution;

    method needs-resolution() { True }

    method is-resolved() {
        nqp::isconcrete($!resolution) ?? True !! False
    }

    method resolution() {
        nqp::isconcrete($!resolution)
            ?? $!resolution
            !! nqp::die('This element has not been resolved')
    }

    method set-resolution(RakuAST::Declaration $resolution) {
        nqp::bindattr(self, RakuAST::Lookup, '$!resolution', $resolution)
    }
}

# Some program elements are not really lookups, but require the resolution
# of symbols as part of their compilation. For example, a positional regex
# access depends on `&postcircumfix:<[ ]>` and `$/`, while an `unless`
# statement depends on `Empty` (as that's what it evaluates to in the case
# there the condition is not matched).
class RakuAST::ImplicitLookups is RakuAST::Node {
    has List $!implicit-lookups-cache;

    # A node typically implements this to specify the implicit lookups
    # that it needs. This is called once per instance of a node and then
    # remains constant. Nodes that may be mutated must instead implement
    # get-implicit-lookups and handle the caching themselves.
    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST(nqp::list())
    }

    # Get a list of the implicit lookups.
    method get-implicit-lookups() {
        $!implicit-lookups-cache //
            nqp::bindattr(self, RakuAST::ImplicitLookups, '$!implicit-lookups-cache',
                self.PRODUCE-IMPLICIT-LOOKUPS())
    }

    # Resolve the implicit lookups if needed.
    method resolve-implicit-lookups-with(RakuAST::Resolver $resolver) {
        for self.IMPL-UNWRAP-LIST(self.get-implicit-lookups()) {
            unless $_.is-resolved {
                $_.resolve-with($resolver);
            }
        }
    }
}
