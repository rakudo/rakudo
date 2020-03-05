class RakuAST::Resolver {
    method resolve-infix(Str $operator-name) {
        self.resolve-lexical('&infix' ~ self.IMPL-CANONICALIZE-PAIR('', $operator-name))
    }

    method resolve-prefix(Str $operator-name) {
        self.resolve-lexical('&prefix' ~ self.IMPL-CANONICALIZE-PAIR('', $operator-name))
    }

    method resolve-postfix(Str $operator-name) {
        self.resolve-lexical('&postfix' ~ self.IMPL-CANONICALIZE-PAIR('', $operator-name))
    }

    method IMPL-CANONICALIZE-PAIR(Str $k, Str $v) {
        if $v ~~ /<[ < > ]>/ && !($v ~~ /<[ « » $ \\ " ' ]>/) {
            ':' ~ $k ~ '«' ~ $v ~ '»'
        }
        else {
            my $new := '';
            my int $i := 0;
            my int $e := nqp::chars($v);
            while $i < $e {
                my $ch := nqp::substr($v,$i,1);
                $new := $new ~ '\\' if $ch eq '<' || $ch eq '>';
                $new := $new ~ $ch;
                ++$i;
            }
            ':' ~ $k ~ '<' ~ $new ~ '>';
        }
    }
}

class RakuAST::Resolver::EVAL is RakuAST::Resolver {
    has Mu $!global;
    has Mu $!context; # XXX Should be PseudoStash

    method new(Mu :$global, Mu :$context) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Resolver::EVAL, '$!global', $global);
        nqp::bindattr($obj, RakuAST::Resolver::EVAL, '$!context', $context);
        $obj
    }

    method resolve-lexical(Str $name) {
        # TODO walk scopes we've encountered while walking

        # Look through the contexts for the name.
        my $ctx := $!context;
        while !nqp::isnull($ctx) {
            if nqp::existskey($ctx, $name) {
                return RakuAST::Declaration::External.new($name);
            }
            $ctx := nqp::ctxouter($ctx);
        }

        # Nothing found.
        return Nil;
    }
}
