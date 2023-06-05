# The base of all statement prefixes.
class RakuAST::StatementPrefix
  is RakuAST::Term
{
    has RakuAST::Blorst $.blorst;

    method new(RakuAST::Blorst $blorst) {
        my $obj := nqp::create(self);
        unless self.allowed-on-for-statement {
            if nqp::istype($blorst, RakuAST::Statement::For) {
                nqp::die('Do not use this statement prefix on a RakuAST::Statement::For; ' ~
                    'instead, set the mode on that node');
            }
        }
        nqp::bindattr($obj, RakuAST::StatementPrefix, '$!blorst', $blorst);
        $obj
    }

    method allowed-on-for-statement() { True }

    method IMPL-CALLISH-QAST(RakuAST::IMPL::QASTContext $context) {
        if nqp::istype($!blorst, RakuAST::Block) {
            $!blorst.IMPL-QAST-BLOCK($context, :blocktype<declaration_static>);
            QAST::Op.new( :op('call'), $!blorst.IMPL-TO-QAST($context) )
        }
        else {
            $!blorst.IMPL-TO-QAST($context)
        }
    }

    method visit-children(Code $visitor) {
        $visitor($!blorst);
    }
}

# The `do` statement prefix.
class RakuAST::StatementPrefix::Do
  is RakuAST::StatementPrefix
  is RakuAST::SinkPropagator
{
    method type() { "do" }

    method propagate-sink(Bool $is-sunk) {
        self.blorst.apply-sink($is-sunk);
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        self.IMPL-CALLISH-QAST($context)
    }
}

# The `quietly` statement prefix.
class RakuAST::StatementPrefix::Quietly
  is RakuAST::StatementPrefix
  is RakuAST::SinkPropagator
{
    method type() { "quietly" }

    method propagate-sink(Bool $is-sunk) {
        self.blorst.apply-sink($is-sunk);
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(
            :op('handle'),
            self.IMPL-CALLISH-QAST($context),
            'WARN',
            QAST::Op.new( :op('resume'), QAST::Op.new( :op('exception') ) )
        )
    }
}

# Base class for statement prefixes that call a method by "type" name
class RakuAST::StatementPrefix::CallMethod
  is RakuAST::StatementPrefix
{
    method type() { nqp::die('Must have a type specified') }

    # default for these cases is False
    method allowed-on-for-statement() { False }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(
          :op('callmethod'), :name(self.type),
          self.IMPL-CALLISH-QAST($context),
        )
    }
}

# The `race` statement prefix.
class RakuAST::StatementPrefix::Race
  is RakuAST::StatementPrefix::CallMethod
{
    method type() { "race" }
}

# The `hyper` statement prefix.
class RakuAST::StatementPrefix::Hyper
  is RakuAST::StatementPrefix::CallMethod
{
    method type() { "hyper" }
}

# The `lazy` statement prefix.
class RakuAST::StatementPrefix::Lazy
  is RakuAST::StatementPrefix::CallMethod
{
    method type() { "lazy" }
}

# The `eager` statement prefix.
class RakuAST::StatementPrefix::Eager
  is RakuAST::StatementPrefix::CallMethod
{
    method type() { "eager" }

    method allowed-on-for-statement() { True }
}

# The `eager` statement prefix.
class RakuAST::StatementPrefix::Sink
  is RakuAST::StatementPrefix::CallMethod
{
    method type() { "sink" }
}

# The `try` statement prefix.
class RakuAST::StatementPrefix::Try
  is RakuAST::StatementPrefix
  is RakuAST::SinkPropagator
  is RakuAST::ImplicitLookups
{
    method type() { "try" }

    method propagate-sink(Bool $is-sunk) {
        self.blorst.apply-sink($is-sunk);
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Nil')),
            RakuAST::Var::Lexical.new('$!'),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Failure')),
        ])
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        # If it's a block that already has a CATCH handler, just run it.
        my $blorst := self.blorst;
        if nqp::istype($blorst, RakuAST::Block) && $blorst.IMPL-HAS-CATCH-HANDLER {
            self.IMPL-CALLISH-QAST($context)
        }

        # Otherwise, need to wrap it in exception handler logic.
        else {
            my $lookups := self.get-implicit-lookups;
            my $nil     := $lookups.AT-POS(0).IMPL-TO-QAST($context);
            my $bang    := $lookups.AT-POS(1).IMPL-TO-QAST($context);
            my $Failure := $lookups.AT-POS(2).IMPL-TO-QAST($context);

            my $tmp := QAST::Node.unique('fatalizee');
            my $qast := self.IMPL-CALLISH-QAST($context);
            QAST::Op.new(
                :op('handle'),

                # Success path puts Nil into $! and evaluates to the block.
                QAST::Stmt.new(
                    :resultchild(0),
                    QAST::Stmts.new(
                        :resultchild(0),
                        QAST::Op.new(
                            :op('bind'),
                            QAST::Var.new( :name($tmp), :scope('local'), :decl('var') ),
                            $qast
                        ),
                        QAST::Op.new(
                            :op('if'),
                            QAST::Op.new(
                                :op('istype'),
                                QAST::Var.new( :name($tmp), :scope('local') ),
                                $Failure,
                            ),
                            QAST::Op.new(
                                :op('callmethod'), :name('sink'),
                                QAST::Var.new( :name($tmp), :scope('local') )
                            )
                        )),
                    QAST::Op.new( :op('p6store'), $bang, $nil )
                ),

                # On failure, capture the exception object into $!.
                'CATCH', QAST::Stmts.new(
                    QAST::Op.new(
                        :op('p6store'),
                        $bang,
                        QAST::Op.new(
                            :name<&EXCEPTION>, :op<call>,
                            QAST::Op.new( :op('exception') )
                        ),
                    ),
                    $nil
                )
            )
        }
    }
}

# Done by statement prefixes that insist on thunking expressions into a code
# object.
class RakuAST::StatementPrefix::Thunky
  is RakuAST::StatementPrefix
  is RakuAST::Meta
  is RakuAST::Code
  is RakuAST::BeginTime
{
    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.IMPL-STUB-CODE($resolver, $context);
        Nil
    }

    method PRODUCE-META-OBJECT() {
        if nqp::istype(self.blorst, RakuAST::Block) {
            # Block, already has a meta-object.
            self.blorst.meta-object
        }
        else {
            # Create code object with default empty signature.
            my $signature := nqp::create(Signature);
            nqp::bindattr($signature, Signature, '@!params', nqp::list());
            my $code := nqp::create(Code);
            nqp::bindattr($code, Code, '$!signature', $signature);
            $code
        }
    }

    method IMPL-QAST-FORM-BLOCK(RakuAST::IMPL::QASTContext $context,
            str :$blocktype, RakuAST::Expression :$expression) {
        if nqp::istype(self.blorst, RakuAST::Block) {
            self.blorst.IMPL-QAST-FORM-BLOCK($context, :$blocktype, :$expression)
        }
        else {
            my $block := QAST::Block.new(
                :blocktype('declaration_static'),
                QAST::Stmts.new(
                    self.blorst.IMPL-TO-QAST($context)
                ));
            $block.arity(0);
            $block
        }
    }

    method IMPL-QAST-BLOCK(RakuAST::IMPL::QASTContext $context, str :$blocktype,
            RakuAST::Expression :$expression) {
        if nqp::istype(self.blorst, RakuAST::Block) {
            self.blorst.IMPL-QAST-BLOCK($context, :$blocktype, :$expression)
        }
        else {
            unless (nqp::getattr(self, RakuAST::Code, '$!qast-block')) {
                self.IMPL-FINISH-CODE-OBJECT($context, :$blocktype, :$expression);
            }
            nqp::getattr(self, RakuAST::Code, '$!qast-block')
        }
    }

    method IMPL-QAST-DECL-CODE(RakuAST::IMPL::QASTContext $context) {
        if nqp::istype(self.blorst, RakuAST::Block) {
            # Block already, so we need add nothing.
            QAST::Op.new( :op('null') )
        }
        else {
            self.IMPL-QAST-BLOCK($context, :blocktype('declaration_static'))
        }
    }
}

# The `gather` statement prefix.
class RakuAST::StatementPrefix::Gather
  is RakuAST::StatementPrefix::Thunky
  is RakuAST::SinkPropagator
{
    method type() { "gather" }

    method propagate-sink(Bool $is-sunk) {
        self.blorst.apply-sink(True);
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new( :op('call'), :name('&GATHER'), self.IMPL-CLOSURE-QAST($context) )
    }
}

# Statement prefix base class for generic blorst handling
class RakuAST::StatementPrefix::Blorst
  is RakuAST::StatementPrefix::Thunky
  is RakuAST::SinkPropagator
  is RakuAST::ImplicitBlockSemanticsProvider
{
    method propagate-sink(Bool $is-sunk) {
        self.blorst.apply-sink(False);
    }

    method apply-implicit-block-semantics() {
        self.blorst.set-fresh-variables(:match, :exception)
          if nqp::istype(self.blorst, RakuAST::Block);
    }

    method IMPL-QAST-FORM-BLOCK(
      RakuAST::IMPL::QASTContext  $context,
                             str :$blocktype,
             RakuAST::Expression :$expression
    ) {
        if nqp::istype(self.blorst, RakuAST::Block) {
            self.blorst.IMPL-QAST-FORM-BLOCK($context, :$blocktype, :$expression)
        }
        else {
            my $block := QAST::Block.new(
              :blocktype('declaration_static'),
              QAST::Stmts.new(
                RakuAST::VarDeclaration::Implicit::Special.new(:name('$/')).IMPL-QAST-DECL($context),
                RakuAST::VarDeclaration::Implicit::Special.new(:name('$!')).IMPL-QAST-DECL($context),
                self.blorst.IMPL-TO-QAST($context)
              ));
            $block.arity(0);
            $block
        }
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(:op<call>,
          :name('&' ~ nqp::uc(self.type)),
          self.IMPL-CLOSURE-QAST($context)
        )
    }
}

# The `once` statement prefix.
class RakuAST::StatementPrefix::Once
  is RakuAST::StatementPrefix::Blorst
  is RakuAST::ImplicitDeclarations
{
    has str $!state-name;

    method type() { "once" }

    method PRODUCE-IMPLICIT-DECLARATIONS() {
        my $state-name := QAST::Node.unique('once_');
        nqp::bindattr_s(self, RakuAST::StatementPrefix::Once, '$!state-name', $state-name);

        self.IMPL-WRAP-LIST([
            RakuAST::VarDeclaration::Implicit::State.new($state-name)
        ])
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(:op<decont>,
          QAST::Op.new(:op<if>,
            QAST::Op.new(:op<p6stateinit>),
            QAST::Op.new(:op<p6store>,
              QAST::Var.new(:name($!state-name), :scope<lexical>),
              QAST::Op.new(:op<call>, self.IMPL-CLOSURE-QAST($context))
            ),
            QAST::Var.new(:name($!state-name), :scope<lexical>)
          )
        )
    }
}

# The `start` statement prefix.
class RakuAST::StatementPrefix::Start
  is RakuAST::StatementPrefix::Blorst
  is RakuAST::ImplicitLookups
{
    method type() { "start" }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Promise')),
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('True')),
        ])
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $lookups := self.get-implicit-lookups;
        my $qast := QAST::Op.new(
            :op('callmethod'), :name('start'),
            $lookups.AT-POS(0).IMPL-TO-QAST($context),
            self.IMPL-CLOSURE-QAST($context)
        );
        unless $context.lang-version eq 'c' {
            my $true := $lookups.AT-POS(1).IMPL-TO-QAST($context);
            $true.named('report-broken-if-sunk');
            $qast.push($true);
        }
        $qast
    }
}

# # Base class for prefixes that can have whenevers in them
class RakuAST::StatementPrefix::Wheneverable
  is RakuAST::StatementPrefix::Blorst
{
    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my str $name := '&' ~ nqp::uc(self.type);
        my $blorst   := self.blorst;
        if nqp::istype($blorst, RakuAST::Statement::Whenever) {
            $name := $name ~ '-ONE-WHENEVER'
              unless $blorst.body.any-whenevers;
        }
        else {
            $name := $name ~ '-ONE-WHENEVER'
              if $blorst.body.statement-list.single-last-whenever;
        }

        QAST::Op.new(
          :op<call>, :$name, self.IMPL-CLOSURE-QAST($context)
        )
    }
}

# The `react` statement prefix.
class RakuAST::StatementPrefix::React
  is RakuAST::StatementPrefix::Wheneverable
{
    method type() { "react" }
}

# The `supply` statement prefix.
class RakuAST::StatementPrefix::Supply
  is RakuAST::StatementPrefix::Wheneverable
{
    method type() { "supply" }
}

# Done by all phasers. Serves as little more than a marker for phasers, for
# easing locating them all.
class RakuAST::StatementPrefix::Phaser
  is RakuAST::StatementPrefix {
    method dump-markers() { '🛸' }
}

# Done by all phasers that don't produce a result.
class RakuAST::StatementPrefix::Phaser::Sinky
  is RakuAST::StatementPrefix::Phaser
  is RakuAST::ImplicitLookups
  is RakuAST::SinkPropagator
{
    method propagate-sink(Bool $is-sunk) {
        self.blorst.apply-sink(True);
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Nil')),
        ])
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        self.get-implicit-lookups.AT-POS(0).IMPL-TO-QAST($context);
    }
}

# The BEGIN phaser.
class RakuAST::StatementPrefix::Phaser::Begin
  is RakuAST::StatementPrefix::Phaser
  is RakuAST::StatementPrefix::Thunky
  is RakuAST::BeginTime
{
    has Mu $!value;

    method type() { "BEGIN" }

    # Perform BEGIN-time evaluation.
    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.IMPL-STUB-CODE($resolver, $context);

        nqp::bindattr_i(self, RakuAST::BeginTime, '$!begin-performed', 1); # avoid infinite loop
        my $producer := self.IMPL-BEGIN-TIME-EVALUATE(self,$resolver,$context);
        nqp::bindattr(self, RakuAST::StatementPrefix::Phaser::Begin,
          '$!value', $producer());
        Nil
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := $!value;
        $context.ensure-sc($value);
        QAST::WVal.new(:$value)
    }
}

# The CHECK phaser.
class RakuAST::StatementPrefix::Phaser::Check
  is RakuAST::StatementPrefix::Phaser
  is RakuAST::CheckTime
{
    has Mu $!value;

    method type() { "CHECK" }

    method new(RakuAST::Blorst $blorst) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::StatementPrefix, '$!blorst', $blorst);
        $obj
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $producer := RakuAST::BeginTime.IMPL-BEGIN-TIME-EVALUATE(
          nqp::getattr(self, RakuAST::StatementPrefix, '$!blorst'),
          $resolver, $context);

        nqp::bindattr(self, RakuAST::StatementPrefix::Phaser::Check, '$!value',
          nqp::istype($producer,Code) ?? $producer() !! $producer);
        Nil
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := $!value;
        $context.ensure-sc($value);
        QAST::WVal.new(:$value)
    }
}

# The INIT phaser.
class RakuAST::StatementPrefix::Phaser::Init
  is RakuAST::StatementPrefix::Phaser
  is RakuAST::StatementPrefix::Thunky
  is RakuAST::BeginTime
{
    has Scalar $.container;

    method type() { "INIT" }

    method new(RakuAST::Blorst $blorst) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::StatementPrefix, '$!blorst', $blorst);
        nqp::bindattr($obj, RakuAST::StatementPrefix::Phaser::Init, '$!container', nqp::create(Scalar));
        $obj
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        $resolver.find-attach-target('compunit').add-init-phaser(self);
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $container := $!container;
        $context.ensure-sc($container);
        QAST::WVal.new( :value($container) )
    }

    method IMPL-CALLISH-QAST(RakuAST::IMPL::QASTContext $context) {
        my $block := self.meta-object;
        $context.ensure-sc($block);
        QAST::Op.new( :op('call'), QAST::WVal.new(:value($block)))
    }
}

# The ENTER phaser.
class RakuAST::StatementPrefix::Phaser::Enter
  is RakuAST::StatementPrefix::Phaser
  is RakuAST::StatementPrefix::Thunky
  is RakuAST::BeginTime
{
    has str $!result-name;

    method type() { "ENTER" }

    method new(RakuAST::Blorst $blorst) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::StatementPrefix, '$!blorst', $blorst);
        $obj
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        nqp::bindattr_s(self, RakuAST::StatementPrefix::Phaser::Enter, '$!result-name',
            ($resolver.find-attach-target('block')
              // $resolver.find-attach-target('compunit')
            ).add-enter-phaser(self)
        );
        nqp::bindattr(self, RakuAST::Code, '$!resolver', $resolver.clone);
    }

    method IMPL-RESULT-NAME() {
        $!result-name
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        nqp::die("ENTER phaser not attached but result accessed") unless $!result-name;
        QAST::Var.new(:name($!result-name), :scope<local>)
    }
}

# The END phaser.
class RakuAST::StatementPrefix::Phaser::End
  is RakuAST::StatementPrefix::Phaser::Sinky
  is RakuAST::StatementPrefix::Thunky
  is RakuAST::BeginTime
{
    method type() { "END" }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        $resolver.find-attach-target('compunit').add-end-phaser(self);
        nqp::bindattr(self, RakuAST::Code, '$!resolver', $resolver.clone);
    }
}

# The QUIT phaser.
class RakuAST::StatementPrefix::Phaser::Quit
  is RakuAST::StatementPrefix::Phaser::Sinky
  is RakuAST::BeginTime
{
    method type() { "QUIT" }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        $resolver.find-attach-target('block').add-phaser("QUIT", self);
    }

    method meta-object() {
        self.blorst.meta-object
    }
}

# base class for all other phasers that are connect to the current block
class RakuAST::StatementPrefix::Phaser::Block
  is RakuAST::StatementPrefix::Phaser::Sinky
  is RakuAST::StatementPrefix::Thunky
  is RakuAST::ParseTime
{
    method PERFORM-PARSE(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        $resolver.find-attach-target('block').add-phaser(
          self.type, self, :has-exit-handler(self.exit-handler));
        nqp::bindattr(self, RakuAST::Code, '$!resolver', $resolver.clone);
    }

    method exit-handler() { False }
}

# The FIRST phaser.
class RakuAST::StatementPrefix::Phaser::First
  is RakuAST::StatementPrefix::Phaser::Block
  is RakuAST::BeginTime
{
    method type() { "FIRST" }

    # To give it return value semantics (my $once = FIRST unique-id),
    # we need to use two lexical state variables attached to the surrounding
    # (attach) block. Two are required because the return value of the FIRST
    # blorst could very well be undefined, meaning that the truthiness (or even
    # defined-ness) of the blorst's value cannot be used to record the 'triggered'
    # state.
    # Though we use two, only one's name needs to be remembered outside of the
    # synthetic AST generation in PERFORM-BEGIN.
    has str $!value-var-name;

    # Because we are going to preserve our initial blorst for presentation / round trip
    # via AST/DEPARSE/.raku. In EVAL, $!original-blorst will not be defined, because
    # our original blorst is in $!blorst
    has RakuAST::Blorst $!original-blorst;
    method original-blorst() {
        $!original-blorst // nqp::getattr(self, RakuAST::StatementPrefix, '$!blorst')
    }

    # We do a lot of things like other RakuAST::StatementPrefix::Phaser::Block nodes,
    # but being sinky isn't one of those things.
    method propagate-sink(Bool $is-sunk) {
        self.blorst.apply-sink(False)
    }

    method PERFORM-BEGIN(Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $blorst := nqp::getattr(self, RakuAST::StatementPrefix, '$!blorst');
        nqp::bindattr(self, RakuAST::StatementPrefix::Phaser::First, '$!original-blorst', $blorst);

        my $True := RakuAST::Term::Name.new(RakuAST::Name.from-identifier('True'));

        my $attach-block := $resolver.find-attach-target('block');
        my $trigger-name := QAST::Node.unique('!first_block_triggered');
        my $trigger-var := RakuAST::VarDeclaration::Implicit::State.new: $trigger-name;
        my $trigger-lookup := $trigger-var.generate-lookup;
        $attach-block.add-generated-lexical-declaration($trigger-var);

        my $value-name := QAST::Node.unique('!first_block_value');
        my $value-var := RakuAST::VarDeclaration::Implicit::State.new: $value-name;
        my $value-lookup := $value-var.generate-lookup;
        $attach-block.add-generated-lexical-declaration($value-var);
        nqp::bindattr_s(self, RakuAST::StatementPrefix::Phaser::First, '$!value-var-name', $value-name);

        $blorst := $blorst.as-block;
        $blorst :=
            RakuAST::Block.new:
                :body(RakuAST::Blockoid.new:
                    RakuAST::StatementList.new:
                        RakuAST::Statement::Unless.new:
                            :condition($trigger-lookup),
                            :body(RakuAST::Block.new:
                                :body(RakuAST::Blockoid.new:
                                    RakuAST::StatementList.new:
                                        RakuAST::Statement::Expression.new(
                                            :expression(RakuAST::ApplyInfix.new:
                                                :infix(RakuAST::Assignment.new(:item)),
                                                :left($value-lookup),
                                                :right(RakuAST::ApplyPostfix.new:
                                                    :postfix(RakuAST::Call::Term.new),
                                                    :operand($blorst)))), # 🛸 ... the actual FIRST code
                                        RakuAST::Statement::Expression.new(
                                            :expression(RakuAST::ApplyInfix.new:
                                                :infix(RakuAST::Assignment.new(:item)),
                                                :left($trigger-lookup),
                                                :right($True))))));

        $blorst.IMPL-BEGIN($resolver, $context);
        nqp::bindattr(self, RakuAST::StatementPrefix, '$!blorst', $blorst);
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Var.new(:name($!value-var-name), :scope<lexical>);
    }
}

# The NEXT phaser.
class RakuAST::StatementPrefix::Phaser::Next
  is RakuAST::StatementPrefix::Phaser::Block
{
    method type() { "NEXT" }
}

# The LAST phaser.
class RakuAST::StatementPrefix::Phaser::Last
  is RakuAST::StatementPrefix::Phaser::Block
{
    method type() { "LAST" }
}

# The LEAVE phaser.
class RakuAST::StatementPrefix::Phaser::Leave
  is RakuAST::StatementPrefix::Phaser::Block
{
    method type() { "LEAVE" }
    method exit-handler() { True }
}

# The KEEP phaser.
class RakuAST::StatementPrefix::Phaser::Keep
  is RakuAST::StatementPrefix::Phaser::Block
{
    method type() { "KEEP" }
    method exit-handler() { True }
}

# The PRE phaser.
class RakuAST::StatementPrefix::Phaser::Pre
  is RakuAST::StatementPrefix::Phaser::Block
{

    method type() { "PRE" }

    method new(RakuAST::Blorst $blorst, Str $condition?) {
        my $obj := nqp::create(self);

        # The PRE phaser needs extra code to get the required
        # functionality, so this converts a given
        #     foo
        # into
        #     X::Phaser::PrePost.new(:value(~foo)).throw unless foo
        nqp::bindattr($obj, RakuAST::StatementPrefix, '$!blorst',
          RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyPostfix.new(
              operand => RakuAST::ApplyPostfix.new(
                operand => RakuAST::Type::Simple.new(
                  RakuAST::Name.from-identifier-parts(
                    'X','Phaser','PrePost'
                  )
                ),
                postfix => RakuAST::Call::Method.new(
                  name => RakuAST::Name.from-identifier('new'),
                  args => RakuAST::ArgList.new(
                    RakuAST::ColonPair::Value.new(
                      key   => 'condition',
                      value => RakuAST::StrLiteral.new(
                        $condition
                          ?? nqp::hllizefor($condition, 'Raku')
                          !! $blorst.origin ?? $blorst.origin.Str !! $blorst.DEPARSE
                      )
                    )
                  )
                )
              ),
              postfix => RakuAST::Call::Method.new(
                name => RakuAST::Name.from-identifier('throw')
              )
            ),
            condition-modifier => RakuAST::StatementModifier::Unless.new(
              nqp::istype($blorst, RakuAST::Block)
                ?? RakuAST::ApplyPostfix.new(
                     operand => $blorst,
                     postfix => RakuAST::Call::Term.new
                   )
                !! $blorst
            )
          )
        );

        $obj
    }
}

# The POST phaser.
class RakuAST::StatementPrefix::Phaser::Post
  is RakuAST::StatementPrefix::Phaser::Block
{
    method type() { "POST" }
    method exit-handler() { True }

    method new(RakuAST::Blorst $blorst, Str $condition?) {
        my $obj  := nqp::create(self);

        # The POST phaser needs extra code to get the required
        # functionality, so this converts a given
        #     foo
        # into
        #     {
        #         X::Phaser::PrePost.new(:value(~foo), :phaser<POST>).throw
        #           unless foo
        #     }
        nqp::bindattr($obj, RakuAST::StatementPrefix, '$!blorst',
          RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
              RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::ApplyPostfix.new(
                    operand => RakuAST::ApplyPostfix.new(
                      operand => RakuAST::Type::Simple.new(
                        RakuAST::Name.from-identifier-parts(
                          'X','Phaser','PrePost'
                        )
                      ),
                      postfix => RakuAST::Call::Method.new(
                        name => RakuAST::Name.from-identifier('new'),
                        args => RakuAST::ArgList.new(
                          RakuAST::ColonPair::Value.new(
                            key   => 'phaser',
                            value => RakuAST::StrLiteral.new(
                              nqp::hllizefor("POST", 'Raku')
                            )
                          ),
                          RakuAST::ColonPair::Value.new(
                            key   => 'condition',
                            value => RakuAST::StrLiteral.new(
                              $condition
                                ?? nqp::hllizefor($condition, 'Raku')
                                !! $blorst.origin
                                  ?? $blorst.origin.Str
                                  !! $blorst.DEPARSE
                            )
                          )
                        )
                      )
                    ),
                    postfix => RakuAST::Call::Method.new(
                      name => RakuAST::Name.from-identifier('throw')
                    )
                  ),
                  condition-modifier => RakuAST::StatementModifier::Unless.new(
                    nqp::istype($blorst, RakuAST::Block)
                      ?? RakuAST::ApplyPostfix.new(
                           operand => $blorst,
                           postfix => RakuAST::Call::Term.new
                         )
                      !! nqp::istype($blorst, RakuAST::Statement::Expression)
                        ?? $blorst.expression
                        !! $blorst
                  )
                )
              )
            )
          )
        );

        $obj
    }
}

# The UNDO phaser.
class RakuAST::StatementPrefix::Phaser::Undo
  is RakuAST::StatementPrefix::Phaser::Block
{
    method type() { "UNDO" }
    method exit-handler() { True }
}

# The CLOSE phaser.
class RakuAST::StatementPrefix::Phaser::Close
  is RakuAST::StatementPrefix::Phaser::Block
{
    method type() { "CLOSE" }
}
