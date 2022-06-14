# An argument list.
class RakuAST::ArgList is RakuAST::CaptureSource {
    has List $!args;

    method new(*@args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ArgList, '$!args', @args);
        $obj
    }

    method from-comma-list(RakuAST::ApplyListInfix $comma-apply) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ArgList, '$!args',
            self.IMPL-UNWRAP-LIST($comma-apply.operands));
        $obj
    }

    method args() {
        self.IMPL-WRAP-LIST($!args)
    }

    method visit-children(Code $visitor) {
        my @args := $!args;
        for @args {
            $visitor($_);
        }
    }

    method IMPL-ADD-QAST-ARGS(RakuAST::IMPL::QASTContext $context, QAST::Op $call) {
        # We need to remove duplicate named args, so make a first pass through to
        # collect those.
        my %named-counts;
        for $!args -> $arg {
            if nqp::istype($arg, RakuAST::NamedArg) {
                %named-counts{$arg.named-arg-name}++;
            }
        }

        # Now emit code to compile and pass each argument.
        for $!args -> $arg {
            if self.IMPL-IS-FLATTENING($arg) {
                # Flattening argument; evaluate it once and pass the array and hash
                # flattening parts.
                my $temp := QAST::Node.unique('flattening_');
                $call.push(QAST::Op.new(
                    :op('callmethod'), :name('FLATTENABLE_LIST'),
                    QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name($temp), :scope('local'), :decl('var') ),
                        $arg.operand.IMPL-TO-QAST($context)
                    ),
                    :flat(1)
                ));
                $call.push(QAST::Op.new(
                    :op('callmethod'), :name('FLATTENABLE_HASH'),
                    QAST::Var.new( :name($temp), :scope('local') ),
                    :flat(1), :named(1)
                ));
            }
            elsif nqp::istype($arg, RakuAST::NamedArg) {
                my $name := $arg.named-arg-name;
                if %named-counts{$name} == 1 {
                    # It's the final appearance of this name, so emit it as the
                    # named argument.
                    my $val-ast := $arg.named-arg-value.IMPL-TO-QAST($context);
                    $val-ast.named($name);
                    $call.push($val-ast);
                }
                else {
                    # It's a discarded value. If it has side-effects, then we
                    # must evaluate those.
                    my $value := $arg.named-arg-value;
                    unless $value.pure {
                        $call.push(QAST::Stmts.new(
                            :flat,
                            $value.IMPL-TO-QAST($context),
                            QAST::Op.new( :op('list') ) # flattens to nothing
                        ));
                    }
                    %named-counts{$name}--;
                }
            }
            else {
                # Positional argument.
                $call.push($arg.IMPL-TO-QAST($context))
            }
        }
    }

    method IMPL-IS-ONE-POS-ARG() {
        nqp::elems($!args) == 1 &&
            !nqp::istype($!args[0], RakuAST::NamedArg) &&
            !self.IMPL-IS-FLATTENING($!args[0])
    }

    method IMPL-IS-FLATTENING(RakuAST::Node $arg) {
        nqp::istype($arg, RakuAST::ApplyPrefix) &&
            nqp::istype($arg.prefix, RakuAST::Prefix) &&
            $arg.prefix.operator eq '|'
    }

    method IMPL-CAN-INTERPRET() {
        for $!args -> $arg {
            if self.IMPL-IS-FLATTENING($arg) {
                # Flattening args not implemented in the interpreter
                # (possible, maybe some ordering subtleties).
                return False;
            }
            elsif nqp::istype($arg, RakuAST::NamedArg) {
                return False unless $arg.named-arg-value.IMPL-CAN-INTERPRET;
            }
            else {
                return False unless $arg.IMPL-CAN-INTERPRET;
            }
        }
        True
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        my @pos;
        my %named;
        for $!args -> $arg {
            if nqp::istype($arg, RakuAST::NamedArg) {
                %named{$arg.named-arg-name} := $arg.named-arg-value.IMPL-INTERPRET($ctx);
            }
            else {
                nqp::push(@pos, $arg.IMPL-INTERPRET($ctx));
            }
        }
        [@pos, %named]
    }
}

# Base role for all kinds of calls (named sub calls, calling some term, and
# method calls).
class RakuAST::Call {
    has RakuAST::ArgList $.args;
}

# A call to a named sub.
class RakuAST::Call::Name is RakuAST::Term is RakuAST::Call is RakuAST::Lookup {
    has RakuAST::Name $.name;
    has Mu $!package;

    method new(RakuAST::Name :$name!, RakuAST::ArgList :$args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Call::Name, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Call, '$!args', $args // RakuAST::ArgList.new);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
        $visitor(self.args);
    }

    method needs-resolution() { $!name.is-identifier }

    method resolve-with(RakuAST::Resolver $resolver) {
        nqp::bindattr(self, RakuAST::Call::Name, '$!package', $resolver.current-package);
        my $resolved := $resolver.resolve-name($!name, :sigil('&'));
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method undeclared-symbol-details() {
        RakuAST::UndeclaredSymbolDescription::Routine.new($!name.canonicalize())
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $call := QAST::Op.new( :op('call') );
        if $!name.is-identifier {
            $call.name(self.resolution.lexical-name);
        }
        else {
            if my $op := $!name.IMPL-IS-NQP-OP {
                $call.op($op);

                if $op eq 'const' {
                    my @args := self.IMPL-UNWRAP-LIST(self.args.args);
                    unless nqp::elems(@args) == 1 && nqp::istype(@args[0], RakuAST::StrLiteral) {
                        nqp::die('Can only compile nqp::const with a string literal');
                    }
                    $call.name(@args[0].value);
                    return $call;
                }
                elsif $op eq 'dispatch' {
                    # We generally want to send unboxed string/int values in for dispatch
                    # arguments (although leave normal ones alone); we can't really
                    # know which are which, but if we're writing out an `nqp::op`
                    # just assume that they should all be unboxed; most situations
                    # will see the dispatch op generated anyway.
                    self.args.IMPL-ADD-QAST-ARGS($context, $call);
                    my int $i := 0;
                    my int $n := nqp::elems($call.list);
                    while $i < $n {
                        if nqp::istype($call[$i], QAST::Want) &&
                                ($call[$i][1] eq 'Ss' || $call[$i][1] eq 'Ii') {
                            $call[$i] := $call[$i][2];
                        }
                        $i++;
                    }
                    return $call
                }
            }
            elsif $!name.is-package-lookup {
                return $!name.IMPL-QAST-PACKAGE-LOOKUP($context, QAST::WVal.new(:value($!package)));
            }
            elsif $!name.is-indirect-lookup {
                return $!name.IMPL-QAST-INDIRECT-LOOKUP($context);
            }
            else {
                nqp::die('compiling complex call names NYI ' ~ $!name.canonicalize)
            }
        }
        self.args.IMPL-ADD-QAST-ARGS($context, $call);
        $call
    }

    method IMPL-CAN-INTERPRET() {
        $!name.is-identifier && self.is-resolved &&
            nqp::istype(self.resolution, RakuAST::CompileTimeValue) &&
            self.args.IMPL-CAN-INTERPRET
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        my $resolved := self.resolution.compile-time-value;
        my @args := self.args.IMPL-INTERPRET($ctx);
        my @pos := @args[0];
        my %named := @args[1];
        return $resolved(|@pos, |%named);
    }
}

# A call to any term (the postfix () operator).
class RakuAST::Call::Term is RakuAST::Call is RakuAST::Postfixish {
    method new(RakuAST::ArgList :$args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Call, '$!args', $args // RakuAST::ArgList.new);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor(self.args);
    }

    method IMPL-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $callee-qast) {
        my $call := QAST::Op.new( :op('call'), $callee-qast );
        self.args.IMPL-ADD-QAST-ARGS($context, $call);
        $call
    }
}

# The base of all method call like things.
class RakuAST::Call::Methodish is RakuAST::Call is RakuAST::Postfixish {
}

# A call to a method identified by a name. Some names (like WHAT and HOW) are
# compiled into primitive operations rather than really being method calls.
class RakuAST::Call::Method is RakuAST::Call::Methodish {
    has RakuAST::Name $.name;

    method new(RakuAST::Name :$name!, RakuAST::ArgList :$args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Call::Method, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Call, '$!args', $args // RakuAST::ArgList.new);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
        $visitor(self.args);
    }

    method IMPL-SPECIAL-OP(str $name) {
        my constant SPECIAL-OPS := nqp::hash(
            'WHAT',     'what',
            'HOW',      'how',
            'WHO',      'who',
            'VAR',      'p6var',
            'REPR',     'p6reprname',
            'DEFINITE', 'p6definite',
        );
        SPECIAL-OPS{$name}
    }

    method IMPL-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $invocant-qast) {
        if $!name.is-identifier {
            my $name := self.IMPL-UNWRAP-LIST($!name.parts)[0].name;
            my $op := self.IMPL-SPECIAL-OP($name);
            if $op {
                # Not really a method call, just using that syntax.
                QAST::Op.new( :$op, $invocant-qast )
            }
            else {
                # A standard method call.
                my $call := QAST::Op.new( :op('callmethod'), :$name, $invocant-qast );
                self.args.IMPL-ADD-QAST-ARGS($context, $call);
                $call
            }
        }
        else {
            nqp::die('Qualified method calls NYI');
        }
    }

    method can-be-used-with-hyper() {
        $!name.is-identifier && !self.IMPL-SPECIAL-OP($!name.canonicalize)
    }

    method IMPL-POSTFIX-HYPER-QAST(RakuAST::IMPL::QASTContext $context, Mu $operand-qast) {
        # TODO later expand this to the qualified case
        my $name := self.IMPL-UNWRAP-LIST($!name.parts)[0].name;
        my $call := QAST::Op.new:
            :op('callmethod'), :name('dispatch:<hyper>'),
            $operand-qast,
            QAST::SVal.new( :value('') ),
            QAST::SVal.new( :value($name) );
        self.args.IMPL-ADD-QAST-ARGS($context, $call);
        $call
    }

    method IMPL-CAN-INTERPRET() { $!name.is-identifier && self.args.IMPL-CAN-INTERPRET }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx, Mu $invocant-compiler) {
        my $invocant := $invocant-compiler();
        my $name := self.IMPL-UNWRAP-LIST($!name.parts)[0].name;
        if $name eq 'WHAT' {
            $invocant.WHAT
        }
        elsif $name eq 'HOW' {
            $invocant.HOW
        }
        elsif $name eq 'WHO' {
            $invocant.WHO
        }
        elsif $name eq 'VAR' {
            my $var := nqp::create(Scalar);
            nqp::bindattr_s($var, Scalar, '$!value', $invocant);
            $var
        }
        elsif $name eq 'REPR' {
            nqp::box_s(nqp::reprname($invocant), Str)
        }
        elsif $name eq 'DEFINITE' {
            nqp::isconcrete($invocant) ?? True !! False
        }
        else {
            my @args := self.args.IMPL-INTERPRET($ctx);
            my @pos := @args[0];
            my %named := @args[1];
            $invocant."$name"(|@pos, |%named)
        }
    }
}

# A call to a method with a quoted name.
class RakuAST::Call::QuotedMethod is RakuAST::Call::Methodish {
    has RakuAST::QuotedString $.name;

    method new(RakuAST::QuotedString :$name!, RakuAST::ArgList :$args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Call::QuotedMethod, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Call, '$!args', $args // RakuAST::ArgList.new);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
        $visitor(self.args);
    }

    method IMPL-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $invocant-qast) {
        my $name-qast := QAST::Op.new( :op<unbox_s>, $!name.IMPL-TO-QAST($context) );
        my $call := QAST::Op.new( :op('callmethod'), $invocant-qast, $name-qast );
        self.args.IMPL-ADD-QAST-ARGS($context, $call);
        $call
    }
}

# A call to a meta-method.
class RakuAST::Call::MetaMethod is RakuAST::Call::Methodish {
    has str $.name;

    method new(str :$name!, RakuAST::ArgList :$args) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Call::MetaMethod, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Call, '$!args', $args // RakuAST::ArgList.new);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor(self.args);
    }

    method IMPL-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $invocant-qast) {
        my $call := QAST::Op.new( :op('p6callmethodhow'), :name($!name), $invocant-qast );
        self.args.IMPL-ADD-QAST-ARGS($context, $call);
        $call
    }
}

# A safe call to a method, i.e. returns Nil if no method was found by that name.
class RakuAST::Call::MaybeMethod is RakuAST::Call::Methodish {
    has str $.name;

    method new(str :$name!, RakuAST::ArgList :$args) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Call::MaybeMethod, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Call, '$!args', $args // RakuAST::ArgList.new);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor(self.args);
    }

    method IMPL-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $invocant-qast) {
        my $call := QAST::Op.new(
            :op('callmethod'),
            :name('dispatch:<.?>'),
            $invocant-qast,
            QAST::SVal.new(:value($!name))
        );
        self.args.IMPL-ADD-QAST-ARGS($context, $call);
        $call
    }
}

class RakuAST::Call::VarMethod is RakuAST::Call::Methodish is RakuAST::Lookup {
    has RakuAST::Name $.name;

    method new(RakuAST::Name :$name!, RakuAST::ArgList :$args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Call::VarMethod, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Call, '$!args', $args // RakuAST::ArgList.new);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
        $visitor(self.args);
    }

    method needs-resolution() { $!name.is-identifier }

    method resolve-with(RakuAST::Resolver $resolver) {
        my $resolved := $resolver.resolve-name($!name, :sigil('&'));
        if $resolved {
            self.set-resolution($resolved);
        }
        Nil
    }

    method undeclared-symbol-details() {
        RakuAST::UndeclaredSymbolDescription::Routine.new($!name.canonicalize())
    }

    method IMPL-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $invocant-qast) {
        my $call := QAST::Op.new(:op<call>, $invocant-qast);
        if $!name.is-identifier {
            $call.name(self.resolution.lexical-name);
        }
        else {
            nqp::die('compiling complex call names NYI')
        }
        self.args.IMPL-ADD-QAST-ARGS($context, $call);
        $call
    }
}

# Base role for all stubs
class RakuAST::Stub is RakuAST::Term is RakuAST::Call is RakuAST::Lookup {
    method new(RakuAST::ArgList :$args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Call, '$!args', $args // RakuAST::ArgList);
        $obj
    }
}

# the ... stub
class RakuAST::Stub::Fail is RakuAST::Stub {
    method IMPL-STUB-NAME() { '...'   }
    method IMPL-FUNC-NAME() { 'fail' }
}

# the ??? stub
class RakuAST::Stub::Warn is RakuAST::Stub {
    method IMPL-STUB-NAME() { '???'   }
    method IMPL-FUNC-NAME() { 'warn' }
}

# the !!! stub
class RakuAST::Stub::Die is RakuAST::Stub {
    method IMPL-STUB-NAME() { '!!!'  }
    method IMPL-FUNC-NAME() { 'die' }
}
