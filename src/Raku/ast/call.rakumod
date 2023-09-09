# An argument list.
class RakuAST::ArgList
  is RakuAST::CaptureSource
{
    has List $!args;
    has RakuAST::Expression $.invocant;

    method new(*@args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ArgList, '$!args', []);
        for @args {
            $obj.push: $_;
        }
        $obj
    }

    method from-comma-list(RakuAST::ApplyListInfix $comma-apply) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ArgList, '$!args', []);
        for self.IMPL-UNWRAP-LIST($comma-apply.operands) {
            $obj.push: $_;
        }
        $obj
    }

    method from-invocant-list(RakuAST::ApplyListInfix $colon-apply) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ArgList, '$!args', []);
        my @args := nqp::clone(self.IMPL-UNWRAP-LIST($colon-apply.operands));
        nqp::bindattr($obj, RakuAST::ArgList, '$!invocant', nqp::shift(@args));
        for @args {
            $obj.push: $_;
        }
        $obj
    }

    method arg-at-pos(int $pos) { nqp::atpos($!args,$pos) }
    method set-arg-at-pos(int $pos, RakuAST::Expression $arg) {
        nqp::bindpos($!args,$pos,$arg);
        Nil
    }

    method push($arg) {
        if nqp::istype($arg, RakuAST::ColonPairs) {
            for $arg.colonpairs {
                self.push: $_
            }
        }
        else {
            nqp::push($!args, $arg)
        }
    }

    method has-args() { nqp::elems($!args) }

    method args() {
        self.IMPL-WRAP-LIST($!args)
    }

    method visit-children(Code $visitor) {
        my @args := $!args;
        $visitor($!invocant) if $!invocant;
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

    method IMPL-HAS-ONLY-COMPILE-TIME-VALUES() {
        for $!args -> $arg {
            return False unless nqp::istype($arg, RakuAST::CompileTimeValue);
        }
        True
    }

    method IMPL-COMPILE-TIME-VALUES() {
        my @pos;
        my %named;
        for $!args -> $arg {
            if nqp::istype($arg, RakuAST::NamedArg) {
                %named{$arg.named-arg-name} := $arg.named-arg-value.compile-time-value;
            }
            else {
                nqp::push(@pos, $arg.compile-time-value);
            }
        }
        [@pos, %named]
    }
}

# Base role for all kinds of calls (named sub calls, calling some term, and
# method calls).
class RakuAST::Call {
    has RakuAST::ArgList $.args;

    method add-colonpair(RakuAST::ColonPair $pair) {
        $!args.push($pair);
        Nil
    }
}

# A call to a named sub.
class RakuAST::Call::Name
  is RakuAST::Term
  is RakuAST::Call
  is RakuAST::Lookup
  is RakuAST::CheckTime
{
    has RakuAST::Name $.name;
    has Mu $!package;
    has int $!p5isms;
    has int $!has-terminator-or-infix;

    method new(RakuAST::Name :$name!, RakuAST::ArgList :$args, :$p5isms, :$has-terminator-or-infix) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Call::Name, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Call, '$!args', $args // RakuAST::ArgList.new);
        nqp::bindattr_i($obj, RakuAST::Call::Name, '$!p5isms', $p5isms ?? 1 !! 0);
        nqp::bindattr_i($obj, RakuAST::Call::Name, '$!has-terminator-or-infix', $has-terminator-or-infix ?? 1 !! 0);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
        $visitor(self.args);
    }

    # In some weird cases, this node can occur as an operator.  Give it
    # some operator properties that are most likely to be correct. This
    # need is possibly indicative of a deeper lying parsing issue.  But
    # until that is fixed, this appears to be the best stopgap measure.
    method properties { OperatorProperties.prefix }

    method needs-resolution() { $!name.is-identifier }

    method resolve-with(RakuAST::Resolver $resolver) {
        nqp::bindattr(self, RakuAST::Call::Name, '$!package', $resolver.current-package);
        my $resolved := $resolver.resolve-name($!name, :sigil('&'));
        if $resolved {
            self.set-resolution($resolved);
        }
        elsif $!name.is-package-lookup {
            my $name := $!name.base-name;
            if $name.canonicalize {
                $resolved := $resolver.resolve-name($name);
                if $resolved {
                    my $v := $resolved.compile-time-value;
                    self.set-resolution($resolved);
                }
            }
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
                return RakuAST::Nqp.new($op, self.args).IMPL-TO-QAST($context);
            }
            elsif $!name.is-package-lookup {
                return self.is-resolved
                    ?? $!name.IMPL-QAST-PACKAGE-LOOKUP(
                        $context,
                        $!package,
                        :lexical(self.resolution)
                    )
                    !! $!name.IMPL-QAST-PACKAGE-LOOKUP(
                        $context,
                        $!package
                    );
            }
            elsif $!name.is-indirect-lookup {
                return $!name.IMPL-QAST-INDIRECT-LOOKUP($context);
            }
            else {
                $call.push(
                    self.is-resolved
                        ?? self.resolution.IMPL-LOOKUP-QAST($context)
                        !! $!name.IMPL-QAST-PACKAGE-LOOKUP(
                            $context,
                            $!package,
                            :sigil<&>,
                            :global-fallback,
                        )
                );
            }
        }
        self.args.IMPL-ADD-QAST-ARGS($context, $call);
        $call
    }

    method IMPL-CAN-INTERPRET() {
        (
            (my $op := $!name.IMPL-IS-NQP-OP) && ($op eq 'hash' || $op eq 'list_s' || $op eq 'atpos_s')
            || $!name.is-identifier && self.is-resolved
                && nqp::istype(self.resolution, RakuAST::CompileTimeValue)
        )
        && self.args.IMPL-CAN-INTERPRET
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        my $op := $!name.IMPL-IS-NQP-OP;
        if $op eq 'hash' {
            my @args := self.args.IMPL-INTERPRET($ctx)[0];
            my $result := nqp::hash;
            while @args {
                my $key := nqp::shift(@args);
                my $val := nqp::shift(@args);
                $result{$key} := $val;
            }
            $result
        }
        elsif $op eq 'list_s' {
            my @args := self.args.IMPL-INTERPRET($ctx)[0];
            my $result := nqp::list_s;
            for @args {
                nqp::push_s($result, $_);
            }
            $result
        }
        elsif $op eq 'atpos_s' {
            my @args := self.args.IMPL-INTERPRET($ctx)[0];
            nqp::atpos_s(@args[0], @args[1]);
        }
        else {
            my $resolved := self.resolution.compile-time-value;
            my @args := self.args.IMPL-INTERPRET($ctx);
            my @pos := @args[0];
            my %named := @args[1];
            $resolved(|@pos, |%named);
        }
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my constant deftrap := nqp::hash(
            'say', 1, 'print', 1, 'abs', 1, 'chomp', 1, 'chop', 1, 'chr', 1, 'cos', 1,
            'defined', 1, 'exp', 1, 'lc', 1, 'log', 1, 'mkdir', 1, 'ord', 1, 'reverse', 1,
            'rmdir', 1, 'sin', 1, 'split', 1, 'sqrt', 1, 'uc', 1, 'unlink', 1, 'fc', 1,

            'WHAT', 2, 'WHICH', 2, 'WHERE', 2, 'HOW', 2, 'WHENCE', 2, 'WHO', 2, 'VAR', 2, 'any', 2,
            'all', 2, 'none', 2, 'one', 2, 'set', 2, 'bag', 2, 'tclc', 2, 'wordcase', 2, 'put', 2,
        );

        if !self.is-resolved {
            return;
        }

        my $call           := self.resolution;
        my $name           := nqp::can($call, 'name')
                             ?? $call.name
                             !! nqp::can($call, 'lexical-name')
                               ?? nqp::substr($call.lexical-name, 1)
                               !! '<anon>';
        if nqp::can($name, 'canonicalize') {
            $name := $name.canonicalize;
        }
        my $no-args        := !self.args.has-args;
        my $in-deftrap     := nqp::atkey(deftrap, $name) || 0;
        my $lexical-symbol := $resolver.resolve-lexical('&' ~ $name);
        if nqp::can($lexical-symbol, 'compile-time-value') {
            $lexical-symbol := $lexical-symbol.compile-time-value;
        }
        my $setting-symbol := $resolver.resolve-lexical-constant-in-setting('&' ~ $name).compile-time-value;
        if nqp::can($setting-symbol, 'compile-time-value') {
            $setting-symbol := $setting-symbol.compile-time-value;
        }
        my $same           := $lexical-symbol =:= $setting-symbol;

        if $same {
            if $in-deftrap && $no-args {
                my $orry := $!has-terminator-or-infix == 1 ?? "sorry" !! "worry";
                if $in-deftrap == 1 {
                    # probably misused P5ism
                    self."add-$orry"(
                        $resolver.build-exception(
                            'X::Comp::AdHoc',
                            payload => "Bare \"$name\", use .$name if you meant to call it as a method on \$_, or use an explicit invocant or argument, or use &$name to refer to the function as a noun"
                        )
                    );
                }
                elsif $in-deftrap == 2 {
                    # probably misused P6ism
                    self."add-$orry"(
                        $resolver.build-exception(
                            'X::Comp::AdHoc',
                            payload => "Function \"$name\" may not be called without arguments (please use () or whitespace to denote arguments, or &$name to refer to the function as a noun, or use .$name if you meant to call it as a method on \$_)"
                        )
                    );
                }
                elsif $orry eq 'worry' && $!p5isms == 0 {
                    self.add-sorry(
                        $resolver.build-exception(
                            'X::Comp::AdHoc',
                            payload => "Argument to \"$name\" seems to be malformed"
                        )
                    );
                }
            }
        }
    }
}

# A call to any term (the postfix () operator).
class RakuAST::Call::Term
  is RakuAST::Call
  is RakuAST::Postfixish
{
    method new(RakuAST::ArgList :$args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Call, '$!args', $args // RakuAST::ArgList.new);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor(self.args);
    }

    method default-operator-properties() {
        OperatorProperties.postfix('()')
    }

    method IMPL-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $callee-qast) {
        my $call := QAST::Op.new( :op('call'), $callee-qast );
        self.args.IMPL-ADD-QAST-ARGS($context, $call);
        $call
    }

    method IMPL-CAN-INTERPRET() {
        self.args.IMPL-CAN-INTERPRET
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx, Code $operand) {
        my $code := $operand();
        my @args := self.args.IMPL-INTERPRET($ctx);
        my @pos := @args[0];
        my %named := @args[1];
        $code(|@pos, |%named)
    }
}

# The base of all method call like things.
class RakuAST::Call::Methodish
  is RakuAST::Call
  is RakuAST::Postfixish
{
    method IMPL-CURRIES() { 3 }
}

# A call to a method identified by a name. Some names (like WHAT and HOW) are
# compiled into primitive operations rather than really being method calls.
class RakuAST::Call::Method
  is RakuAST::Call::Methodish
  is RakuAST::CheckTime
  is RakuAST::ImplicitLookups
{
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

    method default-operator-properties() {
        OperatorProperties.postfix('.')
    }

    method macroish() {
        $!name.is-identifier
          && (my $name := $!name.canonicalize)
          && nqp::istrue(self.IMPL-SPECIAL-OP($name))
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        my $name := $!name.canonicalize;
        my @lookups := [];
        if $name {
            my @parts := nqp::split('::', $name);
            if nqp::elems(@parts) > 1 {
                @parts.pop;
                @lookups.push: # joining @parts with '::' gives use the qualifying type of the name
                    RakuAST::Type::Simple.new: RakuAST::Name.from-identifier: nqp::join('::', @parts);
            }
        }
        self.IMPL-WRAP-LIST(@lookups)
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
        my $name := $!name.canonicalize;
        if $name {
            my @parts := nqp::split('::', $name);
            if nqp::elems(@parts) == 1 {
                my $op := self.IMPL-SPECIAL-OP($name);
                if $op {
                    # Not really a method call, just using that syntax.
                    QAST::Op.new(:$op, $invocant-qast)
                }
                else {
                    # A standard method call.
                    my $call := QAST::Op.new(:op('callmethod'), :$name, $invocant-qast);
                    self.args.IMPL-ADD-QAST-ARGS($context, $call);
                    $call
                }
            } else {
                # TODO: In base behavior, the attempt to dispatch is performed before determining whether the type
                #       actually exists in this scope (throwing a X::Method::InvalidQualifier).
                #       The resolution below will die if the type object cannot be found, deviating from base.
                my $Qualified := self.get-implicit-lookups.AT-POS(0).resolution.compile-time-value;
                $context.ensure-sc($Qualified);
                $name := @parts[ nqp::elems(@parts)-1 ];
                my $call := QAST::Op.new(:op('callmethod'), :name('dispatch:<::>'), $invocant-qast,  QAST::SVal.new(:value($name)), QAST::WVal.new(:value($Qualified)));
                self.args.IMPL-ADD-QAST-ARGS($context, $call);
                $call
            }
        }
        else {
            nqp::die('Qualified method calls NYI');
        }
    }

    method can-be-used-with-hyper() { !self.macroish }

    method IMPL-POSTFIX-HYPER-QAST(RakuAST::IMPL::QASTContext $context, Mu $operand-qast) {
        # TODO later expand this to the qualified case
        my $name := $!name.canonicalize;
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
        my $name := $!name.canonicalize;
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

    method PERFORM-CHECK(
      RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        if self.macroish && self.args.has-args {
            self.add-sorry(
              $resolver.build-exception(
                'X::Syntax::Argument::MOPMacro',
                macro => $!name.canonicalize
              )
            );
        }
    }
}

# A call to a method with a quoted name.
class RakuAST::Call::QuotedMethod
  is RakuAST::Call::Methodish
{
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

    method default-operator-properties() {
        OperatorProperties.postfix('.')
    }

    method IMPL-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $invocant-qast) {
        my $name-qast := QAST::Op.new( :op<unbox_s>, $!name.IMPL-TO-QAST($context) );
        my $call := QAST::Op.new( :op('callmethod'), $invocant-qast, $name-qast );
        self.args.IMPL-ADD-QAST-ARGS($context, $call);
        $call
    }
}

# A call to a private method.
class RakuAST::Call::PrivateMethod
  is RakuAST::Call::Methodish
  is RakuAST::Lookup
  is RakuAST::ImplicitLookups
{
    has RakuAST::Name $.name;
    has Mu $!package;

    method new(RakuAST::Name :$name!, RakuAST::ArgList :$args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Call::PrivateMethod, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Call, '$!args', $args // RakuAST::ArgList.new);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
        $visitor(self.args);
    }

    method default-operator-properties() {
        OperatorProperties.postfix('!')
    }

    method needs-resolution() { False }

    method resolve-with(RakuAST::Resolver $resolver) {
        nqp::bindattr(self, RakuAST::Call::PrivateMethod, '$!package', $resolver.current-package);
        Nil
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Var::Lexical::Constant.new('::?CLASS'),
        ]);
    }

    method IMPL-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $invocant-qast) {
        if $!name.is-identifier {
            my $name := self.IMPL-UNWRAP-LIST($!name.parts)[0].name;
            my $call := QAST::Op.new(
                :op('callmethod'),
                :name('dispatch:<!>'),
                $invocant-qast,
                RakuAST::StrLiteral.new($name).IMPL-EXPR-QAST($context),
                $!package.HOW.archetypes.parametric
                  ?? self.get-implicit-lookups.AT-POS(0).IMPL-EXPR-QAST($context)
                  !! QAST::WVal.new(:value($!package)),
            );
            self.args.IMPL-ADD-QAST-ARGS($context, $call);
            $call
        }
        else {
            nqp::die('Qualified private method calls NYI');
        }
    }
}

# A call to a meta-method.
class RakuAST::Call::MetaMethod
  is RakuAST::Call::Methodish
{
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

    method default-operator-properties() {
        OperatorProperties.postfix('.^')
    }

    method IMPL-POSTFIX-QAST(RakuAST::IMPL::QASTContext $context, Mu $invocant-qast) {
        my $call := QAST::Op.new( :op('p6callmethodhow'), :name($!name), $invocant-qast );
        self.args.IMPL-ADD-QAST-ARGS($context, $call);
        $call
    }
}

# A safe call to a method, i.e. returns Nil if no method was found by that name.
class RakuAST::Call::MaybeMethod
  is RakuAST::Call::Methodish
{
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

    method default-operator-properties() {
        OperatorProperties.postfix('.?')
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

class RakuAST::Call::VarMethod
  is RakuAST::Call::Methodish
  is RakuAST::Lookup
{
    has RakuAST::Name $.name;

    method new(RakuAST::Name :$name!, RakuAST::ArgList :$args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Call::VarMethod, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Call, '$!args', $args // RakuAST::ArgList.new);
        $obj
    }

    method can-be-used-with-hyper() { True }

    method visit-children(Code $visitor) {
        $visitor($!name);
        $visitor(self.args);
    }

    method default-operator-properties() {
        OperatorProperties.postfix('.&')
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

    method IMPL-POSTFIX-HYPER-QAST(RakuAST::IMPL::QASTContext $context, Mu $operand-qast) {
        my $name := $!name.canonicalize;
        my $call := QAST::Op.new:
            :op('callmethod'), :name('dispatch:<hyper>'),
            $operand-qast,
            self.resolution.IMPL-LOOKUP-QAST($context),
            QAST::SVal.new( :value('dispatch:<var>') ),
            self.resolution.IMPL-LOOKUP-QAST($context);
        self.args.IMPL-ADD-QAST-ARGS($context, $call);
        $call
    }
}

# Base role for all stubs
class RakuAST::Stub
  is RakuAST::ImplicitLookups
{
    has RakuAST::ArgList $.args;

    method new(RakuAST::ArgList :$args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Stub, '$!args', $args);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Simple.new(
              RakuAST::Name.from-identifier-parts('X','StubCode')
            )
        ])
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $qast := QAST::Op.new(
          :op<callmethod>, :name<new>,
          self.get-implicit-lookups.AT-POS(0).IMPL-TO-QAST($context)
        );
        if $!args {
            my @args := self.IMPL-UNWRAP-LIST($!args.args);
            if nqp::elems(@args) {
                my $value := @args[0].IMPL-EXPR-QAST($context);
                $value.named("message");
                nqp::push($qast, $value);
            }
        }

        QAST::Op.new(
          :op<callstatic>, :name('&' ~ self.IMPL-FUNC-NAME),
          $qast
        )
    }
}

# the ... stub
class RakuAST::Stub::Fail
  is RakuAST::Stub
{
    method name() { '...' }
    method IMPL-FUNC-NAME() { 'fail' }
}

# the ??? stub
class RakuAST::Stub::Warn
  is RakuAST::Stub
{
    method name() { '???' }
    method IMPL-FUNC-NAME() { 'warn' }
}

# the !!! stub
class RakuAST::Stub::Die
  is RakuAST::Stub
{
    method name() { '!!!' }
    method IMPL-FUNC-NAME() { 'die' }
}
