# An argument list.
class RakuAST::ArgList
  is RakuAST::CaptureSource
  is RakuAST::SinkPropagator
{
    has List                $!args;
    has RakuAST::Expression $.invocant;
    has Bool                $!on-return;

    # Create from given arguments array
    method CREATE(@args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ArgList, '$!args', []);
        for @args {
            $obj.push: $_;
        }
        $obj
    }

    # Create from given arguments
    method new(*@args) {
        self.CREATE(@args)
    }

    # Create from an ApplyListInfix object
    method from-comma-list(RakuAST::ApplyListInfix $infix-list) {
        self.CREATE(self.IMPL-UNWRAP-LIST($infix-list.operands))
    }

    # Create from ApplyListInfix with the first argument being the
    # invocant
    method from-invocant-list(RakuAST::ApplyListInfix $colon-apply) {
        my @args := nqp::clone(self.IMPL-UNWRAP-LIST($colon-apply.operands));

        my $invocant := nqp::shift(@args);
        my $obj      := self.CREATE(@args);
        nqp::bindattr($obj, RakuAST::ArgList, '$!invocant', $invocant);
        $obj
    }

    method arg-at-pos(int $pos) { nqp::atpos($!args,$pos) }
    method set-arg-at-pos(int $pos, RakuAST::Expression $arg) {
        nqp::bindpos($!args,$pos,$arg);
        Nil
    }

    method replace-args(List @args) {
        nqp::bindattr(
          self, RakuAST::ArgList, '$!args', self.IMPL-UNWRAP-LIST(@args)
        );
        Nil
    }

    method set-on-return(Bool $on-return) {
        nqp::bindattr(self, RakuAST::ArgList, '$!on-return', $on-return);
    }

    # Push argument: if it is a list of colonpairs, push each one of the
    # elements recursively
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

    method has-args() { nqp::elems($!args) ?? True !! False }
    method arity() { nqp::elems($!args) }

    method args() { self.IMPL-WRAP-LIST($!args) }

    method visit-children(Code $visitor) {
        $visitor($!invocant) if $!invocant;
        for $!args {
            $visitor($_);
        }
    }

    method propagate-sink(Bool $is-sunk) {
        $!invocant.apply-sink($is-sunk) if $!invocant;
        for $!args {
            $_.apply-sink($is-sunk);
        }
    }

    method IMPL-ADD-QAST-ARGS(
      RakuAST::IMPL::QASTContext $context,
                        QAST::Op $qast
    ) {
        # We need to remove duplicate named args, so make a first pass
        # through to collect those.
        my %named-counts;
        for $!args -> $arg {
            if nqp::istype($arg, RakuAST::NamedArg) {
                %named-counts{$arg.named-arg-name}++;
            }
        }

        # Now emit code to compile and pass each argument.
        for $!args -> $arg {

            # Argument is flattenable
            if self.IMPL-IS-FLATTENING($arg) {
                # Flattening argument; evaluate it once and pass the array
                # and hash flattening parts.
                my $temp := QAST::Node.unique('flattening_');

                # nqp::bind($temp,$operand).FLATTENABLE_LIST
                $qast.push(QAST::Op.new(
                  :op<callmethod>, :name<FLATTENABLE_LIST>, :flat,
                  QAST::Op.new(
                    :op<bind>,
                    QAST::Var.new(:name($temp), :scope<local>, :decl<var>),
                    $arg.operand.IMPL-TO-QAST($context)
                  )
                ));

                # $temp.FLATTENABLE_HASH
                $qast.push(QAST::Op.new(
                  :op<callmethod>, :name<FLATTENABLE_HASH>, :flat, :named,
                  QAST::Var.new(:name($temp), :scope<local>)
                ));
            }

            elsif nqp::istype($arg, RakuAST::NamedArg) && !$!on-return {
                my $name  := $arg.named-arg-name;
                my $value := $arg.named-arg-value;

                # It's the final appearance of this name, so emit it as the
                # named argument.
                if %named-counts{$name} == 1 {
                    my $val-ast := $value.IMPL-TO-QAST($context);
                    $val-ast.named($name);
                    $qast.push($val-ast);
                }

                # It's a discarded value
                else {
                    %named-counts{$name}--;

                    # If it has side-effects, then we must evaluate those.
                    unless $value.pure {
                        $qast.push(QAST::Stmts.new(
                          :flat,
                          $value.IMPL-TO-QAST($context),
                          QAST::Op.new( :op<list> ) # flattens to nothing
                        ));
                    }
                }
            }
            else {
                # Positional argument.
                $qast.push($arg.IMPL-TO-QAST($context))
            }
        }

        # Return the given QAST as a convenience, because this method is
        # often called as the last statement prior to return the QAST
        $qast
    }

    method IMPL-IS-ONE-POS-ARG() {
        nqp::elems($!args) == 1
          && !nqp::istype($!args[0], RakuAST::NamedArg)
          && !self.IMPL-IS-FLATTENING($!args[0])
    }

    method IMPL-IS-FLATTENING(RakuAST::Node $arg) {
        nqp::istype($arg, RakuAST::ApplyPrefix)
          && nqp::istype($arg.prefix, RakuAST::Prefix)
          && $arg.prefix.operator eq '|'
          && !$arg.IMPL-CURRIED
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
                %named{$arg.named-arg-name} :=
                  $arg.named-arg-value.IMPL-INTERPRET($ctx);
            }
            else {
                nqp::push(@pos, $arg.IMPL-INTERPRET($ctx));
            }
        }
        [@pos, %named]
    }

    method IMPL-HAS-ONLY-COMPILE-TIME-VALUES(
      :$allow-generic,
      :$allow-variable
    ) {
        for $!args -> $arg {
            if $arg.has-compile-time-value {
                return False
                  if !$allow-generic
                  && $arg.maybe-compile-time-value.HOW.archetypes.generic;
            }
            else {
                return False
                  unless $allow-variable
                  && nqp::istype($arg, RakuAST::Var::Lexical)
                  && $arg.is-resolved
                  && $arg.resolution.has-compile-time-value;
            }
        }
        True
    }

    method IMPL-COMPILE-TIME-VALUES() {
        my @pos;
        my %named;
        for $!args -> $arg {
            if nqp::istype($arg, RakuAST::NamedArg) {
                %named{$arg.named-arg-name} :=
                  $arg.named-arg-value.maybe-compile-time-value;
            }
            else {
                nqp::push(@pos, $arg.maybe-compile-time-value);
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

    method replace-args(RakuAST::ArgList $args) {
        nqp::bindattr(self, RakuAST::Call, '$!args', $args);
    }

    method CALL-WITH-ARGS($code, RakuAST::IMPL::InterpContext $ctx) {
        my @args := self.args.IMPL-INTERPRET($ctx);
        my @pos   := @args[0];
        my %named := @args[1];
        $code(|@pos, |%named)
    }
}

# A call to a named sub.
class RakuAST::Call::Name
  is RakuAST::Term
  is RakuAST::Call
  is RakuAST::ParseTime
  is RakuAST::BeginTime
  is RakuAST::CheckTime
  is RakuAST::Lookup
{
    has RakuAST::Name    $.name;
    has RakuAST::Code    $!block;
    has RakuAST::Routine $!routine;

    # For top-level package if we can only partially resolve
    has Mu $!lexical;
    has Mu $!package;

    method new(RakuAST::Name :$name!, RakuAST::ArgList :$args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Call::Name, '$!name', $name);
        nqp::bindattr(
          $obj, RakuAST::Call, '$!args', $args // RakuAST::ArgList.new
        );
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

    method undeclared-symbol-details() {
        RakuAST::UndeclaredSymbolDescription::Routine.new($!name.canonicalize())
    }

    method PERFORM-PARSE(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        nqp::bindattr(self, RakuAST::Call::Name, '$!block',
          $resolver.find-attach-target('block')
        );
        nqp::bindattr(self, RakuAST::Call::Name, '$!routine',
          $resolver.find-attach-target('routine')
        );
    }

    method PERFORM-BEGIN(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        nqp::bindattr(self, RakuAST::Call::Name, '$!package',
          $resolver.current-package
        );

        my $resolved := $!name.is-identifier
          ?? $resolver.resolve-lexical('&' ~ $!name.canonicalize)
          !! $resolver.resolve-name($!name, :sigil('&'));

        if $resolved {
            self.set-resolution($resolved);
        }
        elsif $!name.is-package-lookup {
            my $name := $!name.base-name;
            if $name.canonicalize {
                $resolved := $resolver.resolve-name($name);
                if $resolved {
                    $resolved.compile-time-value;
                    self.set-resolution($resolved);
                }
            }
        }

        if !$resolved && $!name.is-multi-part {
            my $resolved := $resolver.resolve-lexical-constant(
              $!name.IMPL-UNWRAP-LIST($!name.parts)[0].name
            );
            nqp::bindattr(self,RakuAST::Call::Name,'$!lexical',$resolved)
              if $resolved;
        }

        my $name := $!name.canonicalize;
        if $name eq 'return' {
            self.args.set-on-return(True);
        }

        my constant RETURNISH := nqp::hash(
          'return',    1,
          'return-rw', 1,
          'fail',      1,
          'nextsame',  1,
          'nextwith',  1,
          'EVAL',      1,
          'EVALFILE',  1
        );
        if nqp::existskey(RETURNISH, $name) {
            my $routine := $!routine;
            if $routine {
                $routine.set-may-use-return(True);
            }
        }
    }

    method PERFORM-CHECK(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        my $name  := $!name.canonicalize;
        my $block := $!block;

        self.add-sorry(
          $resolver.build-exception: 'X::Comp::AdHoc',
            payload => "No return arguments allowed when return value {
                $block.signature.returns.DEPARSE
            } is already specified in the signature"
        ) if ($name eq 'return' || $name eq 'return-rw')
          && $block
          && $block.signature
          && (my $ret := $block.signature.returns)
          && $ret.has-compile-time-value
          && (nqp::isconcrete($ret.maybe-compile-time-value)
               || nqp::istype($ret.maybe-compile-time-value, Nil)
             )
          && self.args
          && self.args.has-args;

        self.PERFORM-BEGIN($resolver, $context) unless self.is-resolved;

        # A `sub foo { }` declaration is hoisted to the start of its
        # lexical scope. When the existing resolution is an `External`
        # (typically `CORE`), re-query and adopt the closer non-`External`
        # match if one is present.
        if $!name.is-identifier
          && self.is-resolved
          && nqp::istype(self.resolution, RakuAST::Declaration::External) {
            my $lexical := $resolver.resolve-lexical('&' ~ $name);
            self.set-resolution($lexical)
              if $lexical
              && !nqp::istype($lexical, RakuAST::Declaration::External);
        }

        # Still not resolved, too bad.  For some reason this does not apply
        # to some names
        self.add-sorry(
          $resolver.build-exception: 'X::Undeclared',
            :what<Variable>,
            :symbol($name)
        ) if !self.is-resolved
          && ($name eq 'pi' || $name eq 'tau' || $name eq 'e'); # XXX any missing?

        if self.is-resolved
          && (nqp::istype(self.resolution, RakuAST::CompileTimeValue)
               || nqp::can(self.resolution, 'maybe-compile-time-value')
             ) {
            my $routine := nqp::istype(
              self.resolution,
              RakuAST::CompileTimeValue
            ) ?? self.resolution.compile-time-value
              !! self.resolution.maybe-compile-time-value;

            if nqp::isconcrete($routine)
              && nqp::istype($routine, Code)
              && nqp::can($routine, 'signature') {
                my @types;
                my @flags;

                my $sig := $routine.signature;
                my $ok   := 1;
                my @args := self.IMPL-UNWRAP-LIST(self.args.args);

                for @args {
                    my $type := $_.return-type;
                    nqp::push(@types, $type);
                    $ok := 0 if $type =:= Mu; # Don't know the type
                    last unless $ok;

                    # Avoid side-effects of comparing subset
                    $ok := 0
                      if nqp::istype($type.HOW, Perl6::Metamodel::SubsetHOW);

                    # These bind at instantiation time
                    $ok := 0
                      if nqp::istype($type.HOW, Perl6::Metamodel::GenericHOW);

                    nqp::push(@flags, nqp::objprimspec($type));
                }

                if $ok {
                    if nqp::elems(@types) == 1
                      && nqp::istype(@args[0], RakuAST::Literal) {
                        my $rev := @args[0].native-type-flag;
                        @flags[0] := nqp::defined($rev)
                          ?? $rev +| 32  # XXX ARG_IS_LITERAL ??
                          !! 0;
                    }

                    # Check if a bind would work
                    my $ct_result := nqp::p6trialbind($sig, @types, @flags);
                    my @ct_result_multi;
                    if nqp::can($routine,'is_dispatcher')
                      && $routine.is_dispatcher
                      && $routine.onlystar {
                        @ct_result_multi :=
                          $routine.analyze_dispatch(@types, @flags);
                    }

                    # Too bad, create a proper sorry
                    if $ct_result == -1
                      || @ct_result_multi
                      && @ct_result_multi[0] == -1 {

                        # Lookup for native type names
                        my constant NATIVE_TYPES := nqp::list_s(
                          '','int','num','str','','','','','','','uint'
                        );

                        my @arguments;
                        my int $i := -1;
                        while ++$i < +@types {
                            my $flag := @flags[$i];
                            my $type := @types[$i];
                            @arguments.push($flag
                              ?? nqp::atpos_s(NATIVE_TYPES,$flag)
                              !! $type.HOW.name($type)
                            );
                        }

                        my $protoguilt := @ct_result_multi
                          && $ct_result == -1
                          ?? True
                          !! False;

                        my $signature := nqp::can($routine,'is_dispatcher')
                          && $routine.is_dispatcher
                          && !$protoguilt
                          ?? self.IMPL-MULTI-SIG-LIST($routine)
                          !! [try $routine.signature.gist];

                        self.add-sorry(
                          $resolver.build-exception: 'X::TypeCheck::Argument',
                            :objname($name),
                            :$signature,
                            :@arguments,
                            :$protoguilt,
                        );
                    }
                }
            }
        }
    }

    method IMPL-MULTI-SIG-LIST($dispatcher) {
        my @sigs;
        for $dispatcher.dispatchees {
            @sigs.push("\n    " ~ $_.signature.gist);
        }
        @sigs
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $name := $!name;

        my $qast := QAST::Op.new(:op<call>);
        if $name.is-identifier {
            if !self.is-resolved && $!name.canonicalize eq 'die' {
                $qast.op('die');
            }
            else {
                $qast.name(self.is-resolved
                  || !$*COMPILING_CORE_SETTING
                  && !$*IMPL-COMPILE-DYNAMICALLY
                  ?? self.resolution.lexical-name
                  !! '&' ~ $!name.canonicalize
                );
            }
        }
        else {

            if my $op := $name.IMPL-IS-NQP-OP {
                return RakuAST::Nqp.new($op, self.args).IMPL-TO-QAST($context);
            }
            elsif $name.is-package-lookup {
                return self.is-resolved
                  && !$name.is-global-lookup
                  ?? QAST::Op.new(:op<who>,
                       self.resolution.IMPL-LOOKUP-QAST($context)
                     )
                  !! $name.IMPL-QAST-PACKAGE-LOOKUP($context, $!package);
            }
            elsif $name.is-indirect-lookup {
                return $name.IMPL-QAST-INDIRECT-LOOKUP($context);
            }
            else {
                $qast.push(
                  self.is-resolved
                    ?? self.resolution.IMPL-LOOKUP-QAST($context)
                    !! $!lexical
                      ?? $name.IMPL-QAST-PACKAGE-LOOKUP(
                           $context,
                           $!package,
                           :lexical($!lexical),
                           :sigil<&>,
                           :global-fallback,
                         )
                      !! $name.IMPL-QAST-PACKAGE-LOOKUP(
                           $context,
                           $!package,
                           :sigil<&>,
                           :global-fallback,
                         )
                );
            }
        }
        self.args.IMPL-ADD-QAST-ARGS($context, $qast);

        # Add return value from signature if this is a return without args
        my $block   := $!block;
        my $subname := $name.canonicalize;
        if ($subname eq 'return' || $subname eq 'return-rw')
          && $block
          && (my $signature := $block.signature)
          && (my $returns   := $signature.returns)
          && $returns.has-compile-time-value {
            my $value := $returns.maybe-compile-time-value;
            $qast.push(QAST::WVal.new(:$value))
              if nqp::isconcrete($value) || nqp::istype($value, Nil);
        }

        $qast
    }

    method IMPL-CAN-INTERPRET() {
        $!name.is-identifier
          && $!name.canonicalize ne 'EVAL'
          && self.is-resolved
          && nqp::istype(self.resolution, RakuAST::CompileTimeValue)
          && self.args.IMPL-CAN-INTERPRET
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.CALL-WITH-ARGS(self.resolution.compile-time-value(), $ctx)
    }
}

# The same as RakuAST::Call::Name, but will deparse without parentheses
class RakuAST::Call::Name::WithoutParentheses
  is RakuAST::Call::Name { }

# A call to any term (the postfix () operator).
class RakuAST::Call::Term
  is RakuAST::Call
  is RakuAST::Postfixish
{
    method new(RakuAST::ArgList :$args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Call, '$!args',
          $args // RakuAST::ArgList.new
        );
        $obj
    }

    method visit-children(Code $visitor) { $visitor(self.args) }

    method default-operator-properties() { OperatorProperties.postfix('()') }

    method can-be-used-with-hyper() { True }

    method IMPL-POSTFIX-QAST(
      RakuAST::IMPL::QASTContext $context,
                              Mu $callee-qast
    ) {
        self.args.IMPL-ADD-QAST-ARGS(
          $context,
          QAST::Op.new(:op<call>, $callee-qast)
        )
    }

    method IMPL-POSTFIX-HYPER-QAST(
      RakuAST::IMPL::QASTContext $context,
                              Mu $operand-qast
    ) {
        self.args.IMPL-ADD-QAST-ARGS(
          $context,
          QAST::Op.new(:op<call>, :name<&METAOP_HYPER_CALL>,
            $operand-qast
          )
        )
    }

    method IMPL-CAN-INTERPRET() { self.args.IMPL-CAN-INTERPRET }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx, Code $operand) {
        self.CALL-WITH-ARGS($operand(), $ctx)
    }
}

# The base of all method call like things.
class RakuAST::Call::Methodish
  is RakuAST::Call
  is RakuAST::Postfixish
{
    has str $!dispatcher;

    # expected to be '.?' | '.+' | '.*' | '.=' but custom ones will be
    # codegenned also
    method set-dispatcher($dispatch) {
        nqp::bindattr_s(self, RakuAST::Call::Methodish, '$!dispatcher',
          $dispatch && $dispatch ne '.' ?? "dispatch:<$dispatch>" !! ""
        );
    }
    method dispatcher() { $!dispatcher // "" }
    method dispatch() {
        $!dispatcher
          ?? nqp::substr($!dispatcher, 10, nqp::chars($!dispatcher) - 11)
          !! ""
    }

    method default-properties(str $default) {
        OperatorProperties.postfix(self.dispatch || $default)
    }

    method IMPL-CURRIES() { 3 }
}

# A call to a method identified by a name. Some names (like WHAT and HOW) are
# compiled into primitive operations rather than really being method calls.
class RakuAST::Call::Method
  is RakuAST::Call::Methodish
  is RakuAST::BeginTime
  is RakuAST::CheckTime
  is RakuAST::ImplicitLookups
{
    has RakuAST::Name $.name;

    method new(
         RakuAST::Name :$name!,
      RakuAST::ArgList :$args,
                   str :$dispatch
    ) {
        my $obj := nqp::create(self);

        nqp::bindattr($obj, RakuAST::Call::Method, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Call, '$!args',
          $args // RakuAST::ArgList.new
        );

        $obj.set-dispatcher($dispatch);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
        $visitor(self.args);
    }

    method default-operator-properties() { self.default-properties(".") }

    method macroish() {
        $!name.is-identifier
          && (my $name := $!name.canonicalize)
          && nqp::istrue(self.IMPL-SPECIAL-OP($name))
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        my @lookups := [];
        if $!name {
            my @parts := nqp::clone($!name.IMPL-UNWRAP-LIST($!name.parts));
            @parts.pop;
            @lookups.push(
              RakuAST::Type::Simple.new: RakuAST::Name.new: |@parts
            ) if @parts;
        }
        @lookups
    }

    method IMPL-SPECIAL-OP(str $name) {
        my constant SPECIAL-OPS := nqp::hash(
          'WHAT',     'what',
          'HOW',      'how',
          'WHO',      'who',
          'WHERE',    'where',
          'VAR',      'p6var',
          'REPR',     'p6reprname',
          'DEFINITE', 'p6definite',
        );
        nqp::atkey(SPECIAL-OPS,$name)
    }

    # Special-op postfixes (.WHAT, .HOW, .VAR, .DEFINITE, etc.) compile to
    # primitive nqp:: ops, not real method calls, so they sit outside the
    # currying machinery. `(5 ~~ *).WHAT` must therefore evaluate `(5 ~~ *)`
    # to a WhateverCode and then take its WHAT, rather than being absorbed
    # into a curried `{ (5 ~~ $_).WHAT }`.
    method IMPL-CURRIES() {
        $!name.is-identifier
          && self.IMPL-SPECIAL-OP($!name.canonicalize)
          ?? 0
          !! 3
    }

    method IMPL-POSTFIX-QAST(
      RakuAST::IMPL::QASTContext $context,
                              Mu $invocant-qast
    ) {
        my $name := $!name.canonicalize;
        my $qast;
        if $name {
            my @parts := nqp::split('::', $name);
            if nqp::elems(@parts) == 1 {
                my $dispatcher := self.dispatcher;
                if $dispatcher {
                    # A method call going through a dispatch: method
                    $qast := QAST::Op.new(:op<callmethod>,
                      :name($dispatcher),
                      $invocant-qast,
                      QAST::SVal.new(:value($name))
                    );
                }
                else {
                    my $op := self.IMPL-SPECIAL-OP($name);
                    $qast  := $op
                       # Not really a method call, just using that syntax
                       ?? QAST::Op.new(:$op, $invocant-qast)
                       # A standard method call
                       !! QAST::Op.new(:op<callmethod>, :$name, $invocant-qast);
                }
            }
            else {
# TODO: In base behavior, the attempt to dispatch is performed before
# determining whether the type actually exists in this scope (throwing
# a X::Method::InvalidQualifier).  The resolution below will die if the
# type object cannot be found, deviating from base.
                my $Qualified := self.IMPL-UNWRAP-LIST(
                  self.get-implicit-lookups
                )[0].resolution.compile-time-value;
                $context.ensure-sc($Qualified);

                $name := @parts[ nqp::elems(@parts)-1 ];
                my $dispatcher := self.dispatcher;

                if $dispatcher {
                    $qast := QAST::Op.new(:op<callmethod>,
                      :name($dispatcher),
                      QAST::SVal.new( :value('dispatch:<::>') ),
                      $invocant-qast,
                      QAST::SVal.new(:value($name)),
                      QAST::WVal.new(:value($Qualified))
                    );
                }

                # No dispatches, so codegen te dispatch logic
                else {
                    my $temp  := QAST::Node.unique('inv_once');
                    # nqp::bind($temp,nqp::dispatch(...))
                    my $stmts := QAST::Stmts.new(
                      QAST::Op.new(:op<bind>,
                        QAST::Var.new(:name($temp), :scope<local>, :decl<var>),
                        $invocant-qast
                      ),
                      $qast := QAST::Op.new(:op<dispatch>,
                        QAST::SVal.new(:value<raku-meth-call-qualified>),
                        QAST::Op.new(:op<decont>,
                          QAST::Var.new(:name($temp), :scope<local>),
                        ),
                        QAST::SVal.new(:value($name)),
                        QAST::WVal.new(:value($Qualified)),
                        QAST::Var.new(:name($temp), :scope<local>),
                      )
                    );
                    self.args.IMPL-ADD-QAST-ARGS($context, $qast);
                    return $stmts;
                }
            }
        }
        else {
            nqp::die('Qualified method calls NYI');
        }

        self.args.IMPL-ADD-QAST-ARGS($context, $qast)
    }

    method can-be-used-with-hyper() { True }

    method IMPL-POSTFIX-HYPER-QAST(
      RakuAST::IMPL::QASTContext $context,
                              Mu $operand-qast
    ) {
        my $name := $!name.canonicalize;
        my $qast;
        if $name {
            my @parts := nqp::split('::', $name);

            if nqp::elems(@parts) == 1 {
                my $dispatcher := self.dispatcher;

                $qast := $dispatcher
                  ?? QAST::Op.new(:op<callmethod>, :name('dispatch:<hyper>'),
                       $operand-qast,
                       QAST::SVal.new(:value($name)),
                       QAST::SVal.new(:value($dispatcher)),
                       QAST::SVal.new(:value($name))
                     )
                  !! QAST::Op.new(:op<callmethod>, :name('dispatch:<hyper>'),
                       $operand-qast,
                       QAST::SVal.new(:value('')),
                       QAST::SVal.new(:value($name))
                     );
            }
            else {
                my $Qualified := self.IMPL-UNWRAP-LIST(
                  self.get-implicit-lookups
                )[0].resolution.compile-time-value;
                $context.ensure-sc($Qualified);
                $name := @parts[ nqp::elems(@parts)-1 ];
                my $dispatcher := self.dispatcher;

                $qast := $dispatcher
                  ?? QAST::Op.new(:op<callmethod>, :name('dispatch:<hyper>'),
                       $operand-qast,
                       QAST::SVal.new(:value($name)),
                       QAST::SVal.new(:value($dispatcher)),
                       QAST::SVal.new(:value('dispatch:<::>')),
                       QAST::SVal.new(:value($name)),
                       QAST::WVal.new(:value($Qualified))
                     )
                  !! QAST::Op.new(:op<callmethod>, :name('dispatch:<hyper>'),
                       $operand-qast,
                       QAST::SVal.new(:value($name)),
                       QAST::SVal.new(:value('dispatch:<::>')),
                       QAST::SVal.new(:value($name)),
                       QAST::WVal.new(:value($Qualified))
                     );
            }
        }
        else {
            nqp::die('Qualified method calls NYI');
        }

        self.args.IMPL-ADD-QAST-ARGS($context, $qast)
    }

    method IMPL-CAN-INTERPRET() {
        $!name.is-identifier
          && $!name.canonicalize ne 'EVAL'
          && self.args.IMPL-CAN-INTERPRET
    }

    method IMPL-INTERPRET(
      RakuAST::IMPL::InterpContext $ctx,
                                Mu $invocant-compiler
    ) {
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
            # XXX perhaps use self.CALL-WITH-ARGS and find_method?
            my @args  := self.args.IMPL-INTERPRET($ctx);
            my @pos   := @args[0];
            my %named := @args[1];
            $invocant."$name"(|@pos, |%named)
        }
    }

    method PERFORM-BEGIN(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        my constant RETURNISH := nqp::hash(
          'return',    1,
          'return-rw', 1,
          'fail',      1,
          'nextsame',  1,
          'nextwith',  1,
          'EVAL',      1,
          'EVALFILE',  1
        );
        if nqp::existskey(RETURNISH, $!name.canonicalize) {
            my $routine := $resolver.find-attach-target('routine');
            if $routine {
                $routine.set-may-use-return(True);
            }
        }
    }

    method PERFORM-CHECK(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        if self.macroish && self.args.has-args {
            self.add-sorry:
              $resolver.build-exception: 'X::Syntax::Argument::MOPMacro',
                macro => $!name.canonicalize;
        }

        if self.macroish && self.dispatcher {
            self.add-sorry:
              $resolver.build-exception: 'X::AdHoc',
                payload => 'Cannot use ' ~ self.dispatch ~ ' on a non-identifier method call';
        }

        if $!name && $!name.is-multi-part {
            my $Qualified := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0];
            unless $Qualified.is-resolved {
                # Give it a second chance to resolve.
                #FIXME Calling IMPL-BEGIN seems like the wrong tool though.
                $Qualified.IMPL-BEGIN($resolver, $context);
            }
        }
    }
}

# A call to a method with a quoted name.
class RakuAST::Call::QuotedMethod
  is RakuAST::Call::Methodish
  is RakuAST::BeginTime
{
    has RakuAST::QuotedString $.name;
    has Mu $!package;

    method new(
      RakuAST::QuotedString :$name!,
           RakuAST::ArgList :$args,
                        str :$dispatch
    ) {
        my $obj := nqp::create(self);

        nqp::bindattr($obj, RakuAST::Call::QuotedMethod, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Call, '$!args',
          $args // RakuAST::ArgList.new
        );

        $obj.set-dispatcher($dispatch);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
        $visitor(self.args);
    }

    method default-operator-properties() { self.default-properties(".") }

    method can-be-used-with-hyper() { True }

    method PERFORM-BEGIN(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        nqp::bindattr(self, RakuAST::Call::QuotedMethod, '$!package',
          $resolver.current-package
        );
        my $routine := $resolver.find-attach-target('routine');
        $routine.set-may-use-return(True) if $routine;
    }

    method IMPL-POSTFIX-QAST(
      RakuAST::IMPL::QASTContext $context,
                              Mu $invocant-qast
    ) {
        my $name-qast := QAST::Op.new(:op<unbox_s>,
          $!name.IMPL-TO-QAST($context)
        );
        my $dispatcher := self.dispatcher;

        self.args.IMPL-ADD-QAST-ARGS($context,
          $dispatcher
            ?? $dispatcher eq 'dispatch:<!>'
              ?? QAST::Op.new(:op<callmethod>, :name($dispatcher),
                   $invocant-qast, $name-qast, QAST::WVal.new(:value($!package))
                 )
              !! QAST::Op.new(:op<callmethod>, :name($dispatcher),
                   $invocant-qast, $name-qast
                 )
            !! QAST::Op.new(:op<callmethod>,
                 $invocant-qast, $name-qast
               )
        )
    }

    method IMPL-POSTFIX-HYPER-QAST(
      RakuAST::IMPL::QASTContext $context,
                              Mu $operand-qast
    ) {
        my $name-qast := QAST::Op.new(:op<unbox_s>,
          $!name.IMPL-TO-QAST($context)
        );
        my $qast;
        my $dispatcher := self.dispatcher;

        if $dispatcher {
            my $name-var   := QAST::Node.unique: 'nodal-name';
            my $nodal-name := QAST::Op.new(:op<bind>,
              QAST::Var.new(:name($name-var), :scope<lexical>, :decl<var>),
              $name-qast
            );
            $qast := QAST::Op.new(:op<callmethod>, :name('dispatch:<hyper>'),
              $operand-qast,
              $nodal-name,
              QAST::SVal.new(:value($dispatcher)),
              QAST::Var.new(:name($name-var), :scope<lexical>)
            );
        }
        else {
            $qast := QAST::Op.new(:op<callmethod>, :name('dispatch:<hyper>'),
              $operand-qast,
              QAST::SVal.new( :value('') ),
              $name-qast
            );
        }

        self.args.IMPL-ADD-QAST-ARGS($context, $qast)
    }
}

# A call to a private method.
class RakuAST::Call::PrivateMethod
  is RakuAST::Call::Methodish
  is RakuAST::Lookup
  is RakuAST::ImplicitLookups
  is RakuAST::ParseTime
  is RakuAST::CheckTime
{
    has RakuAST::Name $.name;
    has Mu            $!package;

    method new(RakuAST::Name :$name!, RakuAST::ArgList :$args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Call::PrivateMethod, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Call, '$!args',
          $args // RakuAST::ArgList.new
        );
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
        $visitor(self.args);
    }

    method default-operator-properties() { OperatorProperties.postfix('!') }

    method needs-resolution() { False }

    method can-be-used-with-hyper() { $!name && $!name.is-multi-part }

    method PERFORM-PARSE(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        nqp::bindattr(self, RakuAST::Call::PrivateMethod, '$!package',
          $resolver.current-package
        );

        if $!name.is-multi-part {
            my @parts := nqp::clone($!name.IMPL-UNWRAP-LIST($!name.parts));
            my $name  := nqp::pop(@parts);
            my $package-name := RakuAST::Name.new(|@parts);
            my $resolution := $resolver.resolve-name-constant($package-name);
            self.set-resolution($resolution) if $resolution;
        }
        Nil
    }

    method PERFORM-CHECK(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        my $package := $!package;

        if self.is-resolved {
            my $methpkg := self.resolution.compile-time-value;
            if $!name.is-multi-part {
                self.add-sorry(
                  $resolver.build-exception: 'X::Method::Private::Permission',
                    :method($!name.last-part.name),
                    :source-package($methpkg.HOW.name($methpkg)),
                    :calling-package($package.HOW.name($package))
                ) unless nqp::can($methpkg.HOW,'is_trusted')
                  && $methpkg.HOW.is_trusted($methpkg,$package);
            }
            else {
                self.add-sorry(
                  $resolver.build-exception: 'X::Method::Private::Unqualified',
                    :method($!name.canonicalize)
                ) unless nqp::can($methpkg.HOW,'find_private_method');
            }
        }
        else {
            self.add-sorry(
              $resolver.build-exception: 'X::Method::Private::Unqualified',
                :method($!name.canonicalize)
            ) unless nqp::can($package.HOW,'find_private_method');
        }

        if $!name.is-identifier {
            my $name := self.IMPL-UNWRAP-LIST($!name.parts)[0].name;

            if nqp::can($package.HOW,'archetypes')
              && !$package.HOW.archetypes.generic
              && !$package.HOW.archetypes.parametric
              && nqp::can($package.HOW,'find_private_method') {
                self.add-sorry(
                  $resolver.build-exception: 'X::Method::NotFound',
                    :method($!name.canonicalize),
                    :typename($package.HOW.name($package)),
                    :private(True),
                    :invocant($package),
                    :in-class-call(True)
                ) unless $package.HOW.find_private_method($package, $name);
            }
        }
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
          RakuAST::Var::Lexical::Constant.new('::?CLASS'),
        ]
    }

    method IMPL-POSTFIX-QAST(
      RakuAST::IMPL::QASTContext $context,
                              Mu $invocant-qast
    ) {
        my $package := $!package;

        my $qast;
        if $!name.is-identifier {
            my $name := self.IMPL-UNWRAP-LIST($!name.parts)[0].name;

            if nqp::can($package.HOW,'archetypes')
              && !$package.HOW.archetypes.generic
              && !$package.HOW.archetypes.parametric
              && nqp::can($package.HOW,'find_private_method') {

                my $meth := $package.HOW.find_private_method($package, $name);
                if $meth {
                    $context.ensure-sc($meth);
                    return self.args.IMPL-ADD-QAST-ARGS(
                      $context,
                      QAST::Op.new(:op<call>,
                        QAST::WVal.new(:value($meth)),
                        $invocant-qast
                      )
                    );
                }
                else {
                    nqp::die("Private method $name not found on " ~ $package.HOW.name($package));
                }
            }
            $qast := QAST::Op.new(:op<dispatch>,
              QAST::SVal.new(:value<raku-meth-private>),
              $package.HOW.archetypes.parametric
                ?? self.IMPL-UNWRAP-LIST(
                     self.get-implicit-lookups
                   )[0].IMPL-EXPR-QAST($context)
                !! QAST::WVal.new(:value($!package)),
                QAST::SVal.new(:value($name)),
                $invocant-qast,
            );
        }
        else {
            $qast := QAST::Op.new(:op<callmethod>, :name('dispatch:<!>'),
              $invocant-qast,
              RakuAST::StrLiteral.new(
                $!name.last-part.name
              ).IMPL-EXPR-QAST($context),
              self.resolution.IMPL-TO-QAST($context),
            );
        }

        self.args.IMPL-ADD-QAST-ARGS($context, $qast)
    }

    method IMPL-POSTFIX-HYPER-QAST(
      RakuAST::IMPL::QASTContext $context,
                              Mu $operand-qast
    ) {
        my $name := $!name.canonicalize;
        my @parts := nqp::split('::', $name);
        $name := @parts[ nqp::elems(@parts)-1 ];

        self.args.IMPL-ADD-QAST-ARGS(
          $context,
          QAST::Op.new(:op<callmethod>, :name('dispatch:<hyper>'),
            $operand-qast,
            QAST::SVal.new(:value($name)),
            QAST::SVal.new(:value('dispatch:<!>')),
            QAST::SVal.new(:value($name)),
            self.resolution.IMPL-TO-QAST($context)
          )
        )
    }
}

# A call to a meta-method.
class RakuAST::Call::MetaMethod
  is RakuAST::Call::Methodish
{
    # Not a RakuAST::Name like the other RakuAST::Call::* classes, because
    # metamethod names can only be simple identifiers. Not multiple parts
    # and no colonpairs.
    has str $.name;

    method new(str :$name!, RakuAST::ArgList :$args) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Call::MetaMethod, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Call, '$!args',
          $args // RakuAST::ArgList.new
        );
        $obj
    }

    method visit-children(Code $visitor) { $visitor(self.args) }

    method default-operator-properties() { OperatorProperties.postfix('.^') }

    method IMPL-POSTFIX-QAST(
      RakuAST::IMPL::QASTContext $context,
                              Mu $invocant-qast
    ) {
        self.args.IMPL-ADD-QAST-ARGS(
          $context,
          QAST::Op.new(:op<p6callmethodhow>, :name($!name),
            $invocant-qast
          )
        )
    }
}

class RakuAST::Call::VarMethod
  is RakuAST::Call::Methodish
  is RakuAST::Lookup
  is RakuAST::BeginTime
  is RakuAST::CheckTime
{
    has RakuAST::Name $.name;

    method new(
         RakuAST::Name :$name!,
      RakuAST::ArgList :$args,
                   str :$dispatch
    ) {
        my $obj := nqp::create(self);

        nqp::bindattr($obj, RakuAST::Call::VarMethod, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Call, '$!args',
          $args // RakuAST::ArgList.new
        );
        $obj.set-dispatcher($dispatch);
        $obj
    }

    method can-be-used-with-hyper() { True }

    method visit-children(Code $visitor) {
        $visitor($!name);
        $visitor(self.args);
    }

    method default-operator-properties() { self.default-properties('.&') }

    method needs-resolution() { $!name.is-identifier }

    method PERFORM-BEGIN(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
      ) {
        my $resolution := $resolver.resolve-name($!name, :sigil('&'));
        self.set-resolution($resolution) if $resolution;

        my constant RETURNISH := nqp::hash(
          'return',    1,
          'return-rw', 1,
          'fail',      1,
          'nextsame',  1,
          'nextwith',  1,
          'EVAL',      1,
          'EVALFILE',  1
        );
        if nqp::existskey(RETURNISH, $!name.canonicalize) {
            my $routine := $resolver.find-attach-target('routine');
            $routine.set-may-use-return(True) if $routine;
        }

        Nil
    }

    method PERFORM-CHECK(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        self.PERFORM-BEGIN($resolver, $context) unless self.is-resolved;
    }

    method undeclared-symbol-details() {
        RakuAST::UndeclaredSymbolDescription::Routine.new($!name.canonicalize())
    }

    method IMPL-POSTFIX-QAST(
      RakuAST::IMPL::QASTContext $context,
                              Mu $invocant-qast
    ) {
        nqp::die('compiling complex call names NYI')
          unless $!name.is-identifier;

        my $name-qast  := self.resolution.IMPL-LOOKUP-QAST($context);
        my $dispatcher := self.dispatcher;

        self.args.IMPL-ADD-QAST-ARGS(
          $context,
          $dispatcher
            ?? QAST::Op.new(:op<callmethod>, :name($dispatcher),
                 $invocant-qast,
                 QAST::SVal.new(:value('dispatch:<var>')),
                 $name-qast
               )
            !! QAST::Op.new(:op<call>,
                 $name-qast,
                 $invocant-qast
               )
        )
    }

    method IMPL-POSTFIX-HYPER-QAST(
      RakuAST::IMPL::QASTContext $context,
                              Mu $operand-qast
    ) {
        my $dispatcher := self.dispatcher;

        self.args.IMPL-ADD-QAST-ARGS(
          $context,
          $dispatcher
            ?? QAST::Op.new(:op<callmethod>, :name('dispatch:<hyper>'),
                 $operand-qast,
                 self.resolution.IMPL-LOOKUP-QAST($context),
                 QAST::SVal.new( :value($dispatcher) ),
                 QAST::SVal.new( :value('dispatch:<var>') ),
                 self.resolution.IMPL-LOOKUP-QAST($context)
               )
            !! QAST::Op.new(:op<callmethod>, :name('dispatch:<hyper>'),
                  $operand-qast,
                  self.resolution.IMPL-LOOKUP-QAST($context),
                  QAST::SVal.new( :value('dispatch:<var>') ),
                  self.resolution.IMPL-LOOKUP-QAST($context)
               )
        )
    }
}

class RakuAST::Call::BlockMethod
  is RakuAST::Call::Methodish
{
    has RakuAST::Block $.block;

    method new(
        RakuAST::Block :$block!,
      RakuAST::ArgList :$args,
                   str :$dispatch
    ) {
        my $obj := nqp::create(self);

        nqp::bindattr($obj, RakuAST::Call::BlockMethod, '$!block', $block);
        nqp::bindattr($obj, RakuAST::Call, '$!args', $args // RakuAST::ArgList.new);

        $obj.set-dispatcher($dispatch);
        $obj
    }

    method can-be-used-with-hyper() { True }

    method visit-children(Code $visitor) {
        $visitor($!block);
        $visitor(self.args);
    }

    method default-operator-properties() { self.default-properties('.&') }

    method IMPL-POSTFIX-QAST(
      RakuAST::IMPL::QASTContext $context,
                              Mu $invocant-qast
    ) {
        my $dispatcher := self.dispatcher;

        self.args.IMPL-ADD-QAST-ARGS(
          $context,
          $dispatcher
            ?? QAST::Op.new(:op<callmethod>, :name($dispatcher),
                 $invocant-qast,
                 QAST::SVal.new( :value('dispatch:<var>')),
                 $!block.IMPL-EXPR-QAST($context),
               )
            !! QAST::Op.new(:op<callmethod>, :name('dispatch:<var>'),
                 $invocant-qast,
                 $!block.IMPL-EXPR-QAST($context),
               )
        )
    }

    method IMPL-POSTFIX-HYPER-QAST(
      RakuAST::IMPL::QASTContext $context,
                              Mu $operand-qast
    ) {
        my $dispatcher := self.dispatcher;

        self.args.IMPL-ADD-QAST-ARGS(
          $context,
          $dispatcher
            ?? QAST::Op.new(:op<callmethod>, :name('dispatch:<hyper>'),
                 $operand-qast,
                 $!block.IMPL-EXPR-QAST($context),
                 QAST::SVal.new(:value($dispatcher)),
                 QAST::SVal.new(:value('dispatch:<var>')),
                 $!block.IMPL-EXPR-QAST($context)
               )
            !! QAST::Op.new(:op<callmethod>, :name('dispatch:<hyper>'),
                 $operand-qast,
                 $!block.IMPL-EXPR-QAST($context),
                 QAST::SVal.new( :value('dispatch:<var>') ),
                 $!block.IMPL-EXPR-QAST($context)
               )
        )
    }
}

# Base role for all stubs
class RakuAST::Stub
  is RakuAST::ImplicitLookups
  is RakuAST::Term
{
    has RakuAST::ArgList $.args;

    method new(RakuAST::ArgList :$args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Stub, '$!args', $args);
        $obj
    }

    method PERFORM-CHECK(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        True
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [
            RakuAST::Type::Simple.new(
              RakuAST::Name.from-identifier-parts('X','StubCode')
            )
        ]
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $qast := QAST::Op.new(:op<callmethod>, :name<new>,
          self.IMPL-UNWRAP-LIST(
            self.get-implicit-lookups
          )[0].IMPL-TO-QAST($context)
        );
        if $!args {
            my @args := self.IMPL-UNWRAP-LIST($!args.args);
            if nqp::elems(@args) {
                my $value := @args[0].IMPL-EXPR-QAST($context);
                $value.named("message");
                nqp::push($qast, $value);
            }
        }

        QAST::Op.new(:op<callstatic>, :name('&' ~ self.IMPL-FUNC-NAME),
          $qast
        )
    }
}

# the ... stub
class RakuAST::Stub::Fail
  is RakuAST::Stub
  is RakuAST::BeginTime
{
    method name() { '...' }
    method IMPL-FUNC-NAME() { 'fail' }

    method PERFORM-BEGIN(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        my $routine := $resolver.find-attach-target('routine');
        $routine.set-may-use-return(True) if $routine;
    }
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
