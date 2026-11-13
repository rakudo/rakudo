# The commonalities for implicitly declared variables.
class RakuAST::VarDeclaration::Implicit
  is RakuAST::Node
  does RakuAST::VarDeclaration
{
    has str $.name;

    # Set by the lexical-to-local lowering analysis when nothing in the
    # declaring scope, and nothing reaching lexicals by name at run
    # time, uses this implicit, so the scope can skip setting it up.
    has int $!unused;

    method IMPL-SET-UNUSED() {
        nqp::bindattr_i(self, RakuAST::VarDeclaration::Implicit, '$!unused', 1)
    }

    method IMPL-UNUSED() { $!unused }

    # What an unused implicit declares. Normally nothing. Under
    # $*EMIT-BEGIN-SHAPE the block being re-formed must keep every
    # lexical name the early compilation serialized, since a context
    # serialized at that time rebinds its lexicals by name into this
    # frame at load. The frame keeps a bare slot under the name and
    # skips the per-call setup.
    method IMPL-UNUSED-DECL-QAST() {
        nqp::ifnull(nqp::getlexdyn('$*EMIT-BEGIN-SHAPE'), 0)
            ?? QAST::Var.new( :decl('var'), :scope('lexical'), :name(self.name) )
            !! QAST::Op.new( :op('null') )
    }

    method new(str :$name!, str :$scope) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name', $name);
        $obj.replace-scope($scope);
        $obj
    }

    method lexical-name() {
        $!name
    }

    method default-scope() {
        'my'
    }

    method generate-lookup() {
        my $lookup := RakuAST::Var::Lexical.new($!name);
        $lookup.set-resolution(self);
        $lookup
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        self.IMPL-LOOKUP-QAST($context)
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context, Mu :$rvalue) {
        QAST::Var.new( :name($!name), :scope('lexical') )
    }
}

# An implicitly declared special variable. Typically used for $/, $!, and $_ in
# routines.
class RakuAST::VarDeclaration::Implicit::Special
  is RakuAST::VarDeclaration::Implicit
  does RakuAST::Meta
{
    method PRODUCE-META-OBJECT(:$resolver, :$context) {
        # Reuse the container descriptor for the common cases that we expect
        # to have.
        # Mu nominals use Untyped so Scalar STORE skips the type check that
        # the regular descriptor would otherwise apply (mirrors
        # Perl6/World.nqp create_container_descriptor).
        my constant COMMON := nqp::list(
            nqp::hash(),
            nqp::hash(  # 6.c
                '$_', ContainerDescriptor::Untyped.new(:of(Mu), :default(Any), :dynamic, :name('$_')),
                '$/', ContainerDescriptor::Untyped.new(:of(Mu), :default(Nil), :dynamic, :name('$/')),
                '$!', ContainerDescriptor::Untyped.new(:of(Mu), :default(Nil), :dynamic, :name('$!'))
            ),
            nqp::hash(  # 6.d
                '$_', ContainerDescriptor::Untyped.new(:of(Mu), :default(Any), :!dynamic, :name('$_')),
                '$/', ContainerDescriptor::Untyped.new(:of(Mu), :default(Nil), :dynamic, :name('$/')),
                '$!', ContainerDescriptor::Untyped.new(:of(Mu), :default(Nil), :dynamic, :name('$!'))
            ),
            nqp::hash(  # 6.e
                '$_', ContainerDescriptor::Untyped.new(:of(Mu), :default(Any), :!dynamic, :name('$_')),
                # The IsolatedMatch type marks the container so method forms
                # such as .match leave it alone instead of writing the match
                # into their caller's $/.
                '$/', ContainerDescriptor::IsolatedMatch.new(:of(Mu), :default(Nil), :dynamic, :name('$/')),
                '$!', ContainerDescriptor::Untyped.new(:of(Mu), :default(Nil), :dynamic, :name('$!'))
            )
        );
        my $cont-desc := COMMON[self.IMPL-LANGUAGE-REVISION]{self.name} //
            RakuAST::IMPL::Containers.create-descriptor(
                :of(Mu), :default(Any), :dynamic(0), :name(self.name));
        my $container := nqp::create(Scalar);
        nqp::bindattr($container, Scalar, '$!descriptor', $cont-desc);
        nqp::bindattr($container, Scalar, '$!value', $cont-desc.default);
        $container
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        return self.IMPL-UNUSED-DECL-QAST() if self.IMPL-UNUSED;
        my $container := self.meta-object;
        $context.ensure-sc($container);
        QAST::Var.new(
            :scope('lexical'), :decl('contvar'), :name(self.name),
            :value($container)
        )
    }

    method can-be-bound-to() { True }

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, QAST::Node $source-qast) {
        QAST::Op.new(
          :op('bind'),
          QAST::Var.new( :name(self.name), :scope('lexical') ),
          $source-qast
        )
    }
}

# A special variable bound at scope entry to the enclosing frame's one of
# the same name, or left as its own container when no frame has one. An
# EVAL unit declares $/ this way so a closure it builds still finds one.
class RakuAST::VarDeclaration::Implicit::Outer
  is RakuAST::VarDeclaration::Implicit::Special
{
    # The scope gives this up to a my declaration of the name.
    method report-redeclaration() {
        False
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        return self.IMPL-UNUSED-DECL-QAST() if self.IMPL-UNUSED;
        my $container := self.meta-object;
        $context.ensure-sc($container);
        QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :scope('lexical'), :decl('var'), :name(self.name) ),
            QAST::Op.new(
                :op('ifnull'),
                QAST::Op.new(
                    :op('getlexrel'),
                    QAST::Op.new( :op('ctxouter'), QAST::Op.new( :op('ctx') ) ),
                    QAST::SVal.new( :value(self.name) )
                ),
                QAST::Op.new( :op('clone'), QAST::WVal.new( :value($container) ) )
            )
        )
    }
}

# The implicit match variable of a 6.e block: a fresh container per
# invocation, initialized at entry from the topic when the topic is a
# Match, and otherwise from the value of the enclosing $/. A regex
# operator in the block then writes the block's own container, so
# matching in one block never disturbs the match state of another frame,
# and a block invoked with a Match reads that Match's captures.
class RakuAST::VarDeclaration::Implicit::BlockMatch
  is RakuAST::VarDeclaration::Implicit::Special
{
    # The declaration is emitted after the topic parameter so its entry
    # initialization can read the topic, while lookups of $/ anywhere in
    # the block are its uses. That order is not a post-declaration worth
    # reporting.
    method report-redeclaration() {
        False
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        return self.IMPL-UNUSED-DECL-QAST() if self.IMPL-UNUSED;
        my $container := self.meta-object;
        $context.ensure-sc($container);
        # The topic check is against NQPMatchRole rather than Match: the
        # role is resolvable here at compile time, while a lexical lookup
        # of the name Match would bind to any user type of that name.
        QAST::Stmts.new(
            QAST::Var.new(
                :scope('lexical'), :decl('contvar'), :name(self.name),
                :value($container)
            ),
            QAST::Op.new(
                :op('p6assign'),
                QAST::Var.new( :scope('lexical'), :name(self.name) ),
                QAST::Op.new(
                    :op('if'),
                    QAST::Op.new(
                        :op('istype'),
                        QAST::Op.new(
                            :op('decont'),
                            QAST::Var.new( :scope('lexical'), :name('$_') )
                        ),
                        QAST::WVal.new( :value(NQPMatchRole) )
                    ),
                    QAST::Var.new( :scope('lexical'), :name('$_') ),
                    QAST::Op.new( :op('getlexouter'), QAST::SVal.new( :value(self.name) ) )
                )
            )
        )
    }
}

# Implicit block topic declaration. By default it is an optional parameter,
# but can be configured to be either a non-parameter (always from outer), a
# required parameter, or to obtain the current exception (for when it is a
# CATCH or CONTROL block);
class RakuAST::VarDeclaration::Implicit::BlockTopic
  is RakuAST::VarDeclaration::Implicit
{
    has Bool $.parameter;
    has Bool $.required;
    has Bool $.exception;
    has Bool $.loop;

    method new(Bool :$parameter, Bool :$required, Bool :$exception, Bool :$loop) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name', '$_');
        $obj.replace-scope('my');
        nqp::bindattr($obj, RakuAST::VarDeclaration::Implicit::BlockTopic, '$!parameter',
            $parameter // True);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Implicit::BlockTopic, '$!required',
            $required // False);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Implicit::BlockTopic, '$!exception',
            $exception // False);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Implicit::BlockTopic, '$!loop',
            $loop // False);
        $obj
    }

    # A topic bound from the enclosing scope is given up to a declaration
    # of the name. Only a topic the block takes as a parameter is kept.
    method IMPL-NOT-IF-DUPLICATE() {
        !$!parameter
    }

    method report-redeclaration() {
        $!parameter ?? True !! False
    }

    method set-parameter(Bool $parameter) {
        nqp::bindattr(self, RakuAST::VarDeclaration::Implicit::BlockTopic, '$!parameter',
            $parameter);
        Nil
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        return self.IMPL-UNUSED-DECL-QAST() if self.IMPL-UNUSED;
        if $!exception {
            QAST::Stmts.new(
                QAST::Var.new( :decl('param'), :scope('local'), :name('EXCEPTION') ),
                QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :decl('var'), :scope('lexical'), :name('$_') ),
                    QAST::Op.new(
                        :op('call'), :name('&EXCEPTION'),
                        QAST::Var.new( :scope('local'), :name('EXCEPTION') )
                    )
                ),
                QAST::Op.new(
                    :op('p6assign'),
                    QAST::Op.new( :op('getlexouter'), QAST::SVal.new( :value('$!') ) ),
                    QAST::Var.new( :scope('lexical'), :name('$_') )
                )
            )
        }
        elsif $!parameter {
            my $param := QAST::Var.new( :decl('param'), :scope('lexical'), :name('$_') );
            unless $!required {
                $param.default(QAST::Op.new( :op('getlexouter'), QAST::SVal.new( :value('$_') ) ));
            }
            $param
        }
        else {
            QAST::Op.new:
                :op('bind'),
                QAST::Var.new( :decl('var'), :scope('lexical'), :name('$_') ),
                QAST::Op.new( :op('getlexouter'), QAST::SVal.new( :value('$_') ) )
        }
    }
}

# An implicitly declared constant variable - that is, one with a value that is
# fixed at compile time. Used for $?PACKAGE and similar.
class RakuAST::VarDeclaration::Implicit::Constant
  is RakuAST::VarDeclaration::Implicit
  does RakuAST::TraitTarget
  does RakuAST::Meta
  does RakuAST::BeginTime
  does RakuAST::CheckTime
  does RakuAST::Declaration::Mergeable
{
    has Mu $.value;
    has int $!meta-object-of-package;

    method new(str :$name!, Mu :$value! is raw, str :$scope) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name', $name);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Implicit::Constant, '$!value', $value);
        $obj.replace-scope($scope);
        $obj
    }

    method set-value($value is raw) {
        nqp::bindattr(self, RakuAST::VarDeclaration::Implicit::Constant, '$!value', $value);
    }

    method compile-time-value() { $!value }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        $context.ensure-sc($!value);
        QAST::Var.new( :decl('static'), :scope('lexical'), :name(self.name), :value($!value) )
    }

    # A lookup of a package meta-object such as $?PACKAGE takes the value it
    # resolved to, since a lookup by name from the header of a package would
    # find that package's own.
    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context, Mu :$rvalue) {
        if $!meta-object-of-package {
            $context.ensure-sc($!value);
            QAST::WVal.new( :value($!value) )
        }
        else {
            nqp::findmethod(RakuAST::VarDeclaration::Implicit, 'IMPL-LOOKUP-QAST')(
              self, $context, :$rvalue)
        }
    }

    method IMPL-IS-META-OBJECT-OF-PACKAGE() { $!meta-object-of-package ?? True !! False }

    method IMPL-SET-META-OBJECT-OF-PACKAGE() {
        nqp::bindattr_i(self, RakuAST::VarDeclaration::Implicit::Constant,
          '$!meta-object-of-package', 1);
        self
    }

    method PRODUCE-META-OBJECT(:$resolver, :$context) {
        self.compile-time-value
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.apply-traits($resolver, $context, self, :SYMBOL(RakuAST::StrLiteral.new(self.name)));
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.add-trait-sorries;
    }
}

# The copy the body of a package keeps of a compile-time variable from around
# it, taken on each entry. Until then, or where nothing around it has the name,
# it holds the value the name resolved to.
class RakuAST::VarDeclaration::Implicit::HeaderValue
  is RakuAST::VarDeclaration::Implicit::Constant
{
    has str $.looked-up;

    method new(str :$name!, Mu :$value! is raw, str :$looked-up!) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name', $name);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Implicit::Constant, '$!value', $value);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit::HeaderValue,
          '$!looked-up', $looked-up);
        $obj.replace-scope('my');
        $obj
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        my $value := self.value;
        $context.ensure-sc($value);
        QAST::Stmts.new(
          QAST::Var.new(:name(self.name), :scope<lexical>, :decl<static>, :$value),
          QAST::Op.new(:op<bind>,
            QAST::Var.new(:name(self.name), :scope<lexical>),
            QAST::Op.new(:op<ifnull>,
              QAST::Op.new(:op<getlexrel>,
                QAST::Op.new(:op<ctxouter>, QAST::Op.new(:op<ctx>)),
                QAST::SVal.new(:value($!looked-up))),
              QAST::Var.new(:name(self.name), :scope<lexical>))))
    }
}

# The %?REQUIRE-SYMBOLS stash of a scope containing a require. Each entry
# into the scope gets a copy of it as BEGIN time left it, so packages merged
# by an earlier call do not shadow what a later indirect lookup finds.
class RakuAST::VarDeclaration::Implicit::RequireSymbols
  is RakuAST::VarDeclaration::Implicit::Constant
{
    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        my $value := self.value;
        $context.ensure-sc($value);
        QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :decl('static'), :scope('lexical'), :name(self.name), :$value ),
            QAST::Op.new( :op('callmethod'), :name('clone'), QAST::WVal.new( :$value ) )
        )
    }
}

# An enum value, e.g. "a" and "b" from enum Foo <a b>
# Is just a constant but we're using the subclass to identify them and
# apply special handling on conflicts.
class RakuAST::VarDeclaration::Implicit::EnumValue
  is RakuAST::VarDeclaration::Implicit::Constant
{
}

# An implicitly declared block (like an auto-generated proto)
class RakuAST::VarDeclaration::Implicit::Block
  is RakuAST::Node
  does RakuAST::VarDeclaration
  does RakuAST::CheckTime
{
    has Mu $.block;

    method new(Mu :$block!, str :$scope) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Implicit::Block, '$!block', $block);
        $obj.replace-scope($scope);
        $obj
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        if nqp::can($!block, 'sort_dispatchees') {
            $!block.sort_dispatchees();
        }
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        $!block.IMPL-QAST-DECL-CODE($context);
    }

    method lexical-name() { '' }
}

# The implicit `self` term declaration for the invocant.
class RakuAST::VarDeclaration::Implicit::Self
  is RakuAST::VarDeclaration::Implicit
{
    has int $!lowered-to-local;
    has Mu $!lowered-away-sentinel;
    has str $!lowered-local-name;

    method new() {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name', 'self');
        $obj.replace-scope('my');
        $obj
    }

    method IMPL-SET-LOWERED-TO-LOCAL(Mu $sentinel) {
        nqp::bindattr_i(self, RakuAST::VarDeclaration::Implicit::Self, '$!lowered-to-local', 1);
        nqp::bindattr(self, RakuAST::VarDeclaration::Implicit::Self, '$!lowered-away-sentinel', $sentinel);
    }

    # The frame-local name for a lowered invocant, minted on first
    # request so the binding and every lookup agree, or the empty string
    # when self stays a by-name lexical.
    method IMPL-LOWERED-LOCAL-NAME() {
        return $!lowered-local-name if $!lowered-local-name;
        return '' unless $!lowered-to-local;
        return '' if nqp::isnull($!lowered-away-sentinel);
        nqp::bindattr_s(self, RakuAST::VarDeclaration::Implicit::Self,
            '$!lowered-local-name',
            QAST::Node.unique('__lowered_self'));
        if nqp::atkey(nqp::getenvhash(), 'RAKUDO_LOWERING_DEBUG') {
            RakuAST::IMPL::VarLowering.IMPL-NOTE(
                'lex2local: minted ' ~ $!lowered-local-name ~ ' for self');
        }
        $!lowered-local-name
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        if self.IMPL-LOWERED-LOCAL-NAME {
            # The local is declared here, in the block's declaration
            # section, so it exists before the invocant binding writes
            # it. The by-name lexical stays declared for introspection,
            # static with the sentinel as its value so no per-entry
            # setup is paid for it.
            $context.ensure-sc($!lowered-away-sentinel);
            QAST::Stmts.new(
                QAST::Var.new(
                    :decl('static'), :scope('lexical'), :name(self.name),
                    :value($!lowered-away-sentinel)
                ),
                QAST::Var.new(
                    :decl('var'), :scope('local'), :name($!lowered-local-name)
                )
            )
        }
        else {
            QAST::Var.new( :decl('var'), :scope('lexical'), :name(self.name) )
        }
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context, Mu :$rvalue) {
        my str $local-name := self.IMPL-LOWERED-LOCAL-NAME;
        $local-name
            ?? QAST::Var.new( :name($local-name), :scope('local') )
            !! QAST::Var.new( :name(self.name), :scope('lexical') )
    }

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, Mu $source-qast) {
        my str $local-name := self.IMPL-LOWERED-LOCAL-NAME;
        QAST::Op.new(
            :op('bind'),
            $local-name
                ?? QAST::Var.new( :name($local-name), :scope('local') )
                !! QAST::Var.new( :name(self.name), :scope('lexical') ),
            $source-qast
        )
    }
}

# The implicit `$¢` declaration for the cursor.
class RakuAST::VarDeclaration::Implicit::Cursor
  is RakuAST::VarDeclaration::Implicit
  does RakuAST::Meta
{
    method new() {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name', '$¢');
        $obj.replace-scope('my');
        $obj
    }

    method PRODUCE-META-OBJECT(:$resolver, :$context) {
        my $cont-desc := RakuAST::IMPL::Containers.create-descriptor(
            :of(Mu), :default(Nil), :dynamic(0), :name('$¢'));
        my $container := nqp::create(Scalar);
        nqp::bindattr($container, Scalar, '$!descriptor', $cont-desc);
        nqp::bindattr($container, Scalar, '$!value', $cont-desc.default);
        $container
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        return self.IMPL-UNUSED-DECL-QAST() if self.IMPL-UNUSED;
        my $container := self.meta-object;
        $context.ensure-sc($container);
        QAST::Var.new(
            :decl('contvar'), :scope('lexical'), :name(self.name),
            :value($container)
        )
    }
}

# We install regex captures in the lexpad and look it up when we need it.
# This means we can avoid closure-cloning it per time we enter it, for example
# if it is quantified.
class RakuAST::VarDeclaration::Implicit::RegexCapture
  is RakuAST::VarDeclaration::Implicit
{
    method new() {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name', QAST::Node.unique('!__REGEX_CAPTURE_'));
        $obj;
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        QAST::Var.new(
            :scope('lexical'), :decl('var'), :name(self.name),
        )
    }

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, QAST::Node $source-qast) {
        QAST::Op.new(
          :op('bind'),
          QAST::Var.new( :name(self.name), :scope('lexical') ),
          $source-qast
        )
    }
}

class RakuAST::VarDeclaration::Implicit::RoleConcretization
  is RakuAST::VarDeclaration::Implicit
{
    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        my $conc-name := '$?CONCRETIZATION';
        QAST::Stmts.new(
            QAST::Var.new( :name($conc-name), :decl<static>, :scope<lexical> ),
            # The symbol would be set from $*MOP-ROLE-CONCRETIZATION provided by MOP specialization code.
            # NOTE The fallback to VMNull is ok here because it won't be revealed to user code which would
            # only be run after the concretization.
            QAST::Stmt.new(
                QAST::Op.new(
                    :op<bind>,
                    QAST::Var.new( :name($conc-name), :scope<lexical> ),
                    QAST::VarWithFallback.new(
                        :name<$*MOP-ROLE-CONCRETIZATION>,
                        :scope<contextual>,
                        :fallback( QAST::Op.new( :op<null> ) )
                    )
                )
            )
        )
    }
}

# The implicit `&?ROUTINE` term declaration for the routine.
class RakuAST::VarDeclaration::Implicit::Routine
  is RakuAST::VarDeclaration::Implicit
{
    method new() {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name', '&?ROUTINE');
        $obj.replace-scope('my');
        $obj
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        return self.IMPL-UNUSED-DECL-QAST() if self.IMPL-UNUSED;
        # We cannot just put the AST node's meta-object into a WVal for this, because
        # we may be running a clone of that object.
        QAST::Op.new:
            :op('bind'),
            QAST::Var.new( :decl('var'), :scope('lexical'), :name(self.name) ),
            QAST::Op.new( :op('getcodeobj'), QAST::Op.new( :op('curcode') ) )
    }
}

# The implicit `&?BLOCK` term declaration for a block. Bound to the block's own
# code object, so a lexical reference to `&?BLOCK` from an inlined control-flow
# branch inside the block resolves to it.
class RakuAST::VarDeclaration::Implicit::CurrentBlock
  is RakuAST::VarDeclaration::Implicit::Routine
{
    method new() {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name', '&?BLOCK');
        $obj.replace-scope('my');
        $obj
    }
}

# Used for constructs that generate state variables
class RakuAST::VarDeclaration::Implicit::State
  is RakuAST::VarDeclaration::Implicit
  does RakuAST::ImplicitLookups
  does RakuAST::Meta
{
    has int $!init-to-zero;
    has Mu $!sentinel-value;

    method new(str $name, Bool :$init-to-zero, Bool :$sentinel) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name', $name);
        $obj.replace-scope('state');
        nqp::bindattr_i($obj, RakuAST::VarDeclaration::Implicit::State, '$!init-to-zero', ?$init-to-zero);
        # A private initial value no user code can produce, so a first read of
        # the variable is recognizable by value.
        nqp::bindattr($obj, RakuAST::VarDeclaration::Implicit::State, '$!sentinel-value',
            $sentinel ?? nqp::create(Mu) !! Mu);
        $obj
    }

    # Whether the variable still holds its sentinel.
    method IMPL-SENTINEL-TEST-QAST(RakuAST::IMPL::QASTContext $context) {
        nqp::die('A state variable made without :sentinel has none to test')
          unless nqp::isconcrete($!sentinel-value);
        $context.ensure-sc($!sentinel-value);
        QAST::Op.new(:op<eqaddr>,
          QAST::Op.new(:op<decont>,
            QAST::Var.new( :name(self.name), :scope<lexical> )),
          QAST::WVal.new( :value($!sentinel-value) )
        )
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        $!init-to-zero
          ?? [RakuAST::Type::Setting.from-identifier('Int')]
          !! []
    }

    method PRODUCE-META-OBJECT(:$resolver, :$context) {
        my str $name := nqp::getattr_s(self, RakuAST::VarDeclaration::Implicit, '$!name');

        # Create a container descriptor and attach it to a Scalar container, then set it to Int.new(0)
        my $descriptor := RakuAST::IMPL::Containers.create-descriptor(
            :of(Mu), :default(Any), :dynamic(0), :$name);
        my $container := nqp::create(Scalar);
        nqp::bindattr($container, Scalar, '$!descriptor', $descriptor);
        if $!init-to-zero {
            my $Int := self.get-implicit-lookups[0].compile-time-value;
            nqp::bindattr($container, Scalar, '$!value', nqp::box_i(0, $Int));
        } elsif nqp::isconcrete($!sentinel-value) {
            nqp::bindattr($container, Scalar, '$!value', $!sentinel-value);
        } else {
            nqp::bindattr($container, Scalar, '$!value', $descriptor.default);
        }

        $container
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        my $container := self.meta-object;
        $context.ensure-sc($container);
        QAST::Var.new: :decl<statevar>, :scope<lexical>, :name(self.name), :value($container)
    }
}

# commonalities for doc variables
class RakuAST::VarDeclaration::Implicit::Doc
  is RakuAST::VarDeclaration::Implicit
  does RakuAST::CheckTime
{
    has Mu $.value;

    method new() {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::VarDeclaration::Implicit, '$!name',
          self.name);
        $obj.replace-scope('my');
        $obj
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) { ... }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        my $value := $!value;
        $context.ensure-sc($value);
        QAST::Op.new:
          :op('bind'),
          QAST::Var.new(:decl('static'), :scope('lexical'), :name(
            nqp::getattr_s(self, RakuAST::VarDeclaration::Implicit, '$!name')
          )),
          QAST::WVal.new(:$value);
    }
}

# The implicit `$=pod` term declaration for legacy pod access
class RakuAST::VarDeclaration::Implicit::Doc::Pod
  is RakuAST::VarDeclaration::Implicit::Doc
{
    method name() { '$=pod' }

    method PERFORM-CHECK(
      RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        nqp::bindattr(self, RakuAST::VarDeclaration::Implicit::Doc, '$!value',
          $resolver.find-attach-target('compunit').pod-content
        );
    }
}

# The implicit `$=data` term declaration for =data access
class RakuAST::VarDeclaration::Implicit::Doc::Data
  is RakuAST::VarDeclaration::Implicit::Doc
{
    method name() { '$=data' }

    method PERFORM-CHECK(
      RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        nqp::bindattr(self, RakuAST::VarDeclaration::Implicit::Doc, '$!value',
          nqp::ifnull(
            $resolver.find-attach-target('compunit').data-content,
            Mu
          )
        );
    }
}

# The implicit `$=finish` term declaration for trailing documentation
class RakuAST::VarDeclaration::Implicit::Doc::Finish
  is RakuAST::VarDeclaration::Implicit::Doc
{
    method name() { '$=finish' }

    method PERFORM-CHECK(
      RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        nqp::bindattr(self, RakuAST::VarDeclaration::Implicit::Doc, '$!value',
          $resolver.find-attach-target('compunit').finish-content,
        );
    }
}

# The implicit `$=rakudoc` term declaration for RakuDoc access
class RakuAST::VarDeclaration::Implicit::Doc::Rakudoc
  is RakuAST::VarDeclaration::Implicit::Doc
{
    method name() { '$=rakudoc' }

    method fetch-blocks(RakuAST::StatementList $statement-list) {
        my $RAKUDOC := $*RAKUDOC;  # lexicals are faster

        for nqp::getattr(
          $statement-list,RakuAST::StatementList,'$!statements'
        ) {
            if nqp::istype($_,RakuAST::Doc::Block) {
                nqp::push($RAKUDOC,$_);
            }
            elsif nqp::istype($_,RakuAST::Statement::Expression) {
                my $expression := $_.expression;
                # A declarand node must not go into a precompilation's SC.
                # Its backlinks (a routine's $!outer, a BEGIN prefix's cached
                # thunk) reach live compile-time frames, and serializing those
                # snapshots leaves deserialized BEGIN closures with an outer
                # chain that dead-ends before the setting. Declarator docs on
                # a precompiled unit stay reachable via .WHY on the meta
                # objects and via $=pod.
                if nqp::istype($expression,RakuAST::Doc::DeclaratorTarget)
                  && $expression.WHY
                  && !$*RAKUDOC-SKIP-DECLARANDS {
                    nqp::push($RAKUDOC,$expression);
                }
                # Descend into a package or routine/block body so docs declared
                # inside, such as in a `unit class`, are collected too.
                if nqp::can($expression,'body') {
                    my $body := $expression.body;
                    $body := $body.body if nqp::istype($body,RakuAST::Block);
                    self.fetch-blocks($body.statement-list)
                      if nqp::istype($body,RakuAST::Blockoid);
                }
            }
            elsif nqp::istype($_,RakuAST::Blockoid) {
                self.fetch-blocks($_.statement-list);
            }
        }
    }

    method PERFORM-CHECK(
      RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        my $compunit := $resolver.find-attach-target('compunit');
        my $*RAKUDOC := [];
        my $*RAKUDOC-SKIP-DECLARANDS := $compunit.precompilation-mode;
        self.fetch-blocks($compunit.statement-list);

        nqp::bindattr(self, RakuAST::VarDeclaration::Implicit::Doc, '$!value',
          self.IMPL-WRAP-LIST($*RAKUDOC)
        );
    }
}
