# A compilation unit is the main lexical scope of a program.
class RakuAST::CompUnit
  is RakuAST::LexicalScope
  is RakuAST::SinkBoundary
  is RakuAST::ImplicitLookups
  is RakuAST::ImplicitDeclarations
  is RakuAST::AttachTarget
  is RakuAST::ScopePhaser
{
    has RakuAST::StatementList $.statement-list;
    has RakuAST::Block $.mainline;
    has Str $.comp-unit-name;
    has Str $.setting-name;
    has Mu $.language-revision; # Same type as in CORE-SETTING-REV
    has Mu $!sc;
    has int $!is-sunk;
    has int $!is-eval;
    has Mu $!global-package-how;
    has Mu $!init-phasers;
    has Mu $!end-phasers;
    has RakuAST::VarDeclaration::Implicit::Doc::Pod    $.pod;
    has RakuAST::VarDeclaration::Implicit::Doc::Data   $.data;
    has RakuAST::VarDeclaration::Implicit::Doc::Finish $.finish;
    has Mu $.pod-content;
    has Mu $.data-content;
    has Mu $.finish-content;
    has Mu $!singleton-whatever;
    has Mu $!singleton-hyper-whatever;
    has int $.precompilation-mode;
    has Mu $!export-package;
    has Mu $.herestub-queue;
    has RakuAST::IMPL::QASTContext $.context;

    method new(          Str :$comp-unit-name!,
                         Str :$setting-name,
                        Bool :$eval,
                          Mu :$global-package-how,
                         int :$language-revision,
                        Bool :$precompilation-mode,
                          Mu :$export-package,
      RakuAST::StatementList :$statement-list,
           RakuAST::CompUnit :$outer-cu
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!statement-list',
            $statement-list // RakuAST::StatementList.new);

        my $mainline := RakuAST::Block.new();
        $mainline.set-implicit-topic(0);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!mainline', $mainline);

        nqp::bindattr($obj, RakuAST::CompUnit, '$!comp-unit-name',
          $comp-unit-name);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!setting-name',
          $setting-name // Str);
        nqp::bindattr_i($obj, RakuAST::CompUnit, '$!is-eval',
          $eval ?? $outer-cu ?? 2 !! 1 !! 0);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!global-package-how',
          $global-package-how =:= NQPMu
            ?? Perl6::Metamodel::PackageHOW
            !! $global-package-how
        );
        nqp::bindattr($obj, RakuAST::CompUnit, '$!export-package',
          $export-package =:= NQPMu ?? Mu !! $export-package);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!init-phasers', []);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!end-phasers', []);

        nqp::bindattr_i($obj, RakuAST::CompUnit, '$!precompilation-mode',
          $precompilation-mode ?? 1 !! 0);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!pod-content', Array.new);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!data-content', nqp::null);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!herestub-queue', []);

        # If CompUnit's language revision is not set explicitly then guess it
        nqp::bindattr($obj, RakuAST::CompUnit, '$!language-revision',
          $language-revision
            ?? Perl6::Metamodel::Configuration.language_revision_object($language-revision)
            !! nqp::isconcrete(
                 my $setting-rev := nqp::getlexrelcaller(
                   nqp::ctxcallerskipthunks(nqp::ctx()), 'CORE-SETTING-REV'
                 )
               ) ?? $setting-rev
                 !! Perl6::Metamodel::Configuration.language_revision_object(
                      nqp::getcomp("Raku").language_revision)
                    );

        my $sc;
        if $outer-cu {
            my $context             := $outer-cu.context.create-nested;
            $sc                     := $context.sc;
            my $precompilation-mode := $outer-cu.precompilation-mode;

            nqp::bindattr($obj, RakuAST::CompUnit, '$!sc', $sc);
            nqp::bindattr($obj, RakuAST::CompUnit, '$!context', $context);
            nqp::bindattr_i($obj, RakuAST::CompUnit, '$!precompilation-mode',
              $precompilation-mode);
        }
        else {
            $sc := nqp::createsc($comp-unit-name);
            nqp::pushcompsc($sc);
            nqp::bindattr($obj, RakuAST::CompUnit, '$!sc', $sc);
            nqp::bindattr($obj, RakuAST::CompUnit, '$!context',
              RakuAST::IMPL::QASTContext.new(:$sc, :$precompilation-mode));
            nqp::bindattr($obj, RakuAST::CompUnit, '$!pod',
              RakuAST::VarDeclaration::Implicit::Doc::Pod.new);
            nqp::bindattr($obj, RakuAST::CompUnit, '$!data',
              RakuAST::VarDeclaration::Implicit::Doc::Data.new);
            nqp::bindattr($obj, RakuAST::CompUnit, '$!finish',
              RakuAST::VarDeclaration::Implicit::Doc::Finish.new);
        }

        my $file := nqp::getlexdyn('$?FILES');
        nqp::scsetdesc($sc, $file) unless nqp::isnull($file);

        $obj
    }

    # Perform all CHECK-time activities on the compilation unit. This includes
    # doing any symbol resolution, any leftover sink marking, and performing
    # any CHECK-time error checking. This may also produce information useful
    # during optimization, though will not do any transforms in and of itself.
    method check(RakuAST::Resolver $resolver) {
        if @*LEADING-DOC {
            $resolver.add-worry:
              $resolver.build-exception:
                'X::Syntax::Doc::Declarator::MissingDeclarand',
                :position<leading>;
        }

        $!mainline.IMPL-CHECK($resolver, $!context, False);
        self.IMPL-CHECK($resolver, $!context, False);

        # Not all RakuAST::Doc objects actually have their PERFORM-CHECK
        # method called on them, causing holes to occur in $=pod (albeit
        # that the blocks that *are* generated, are in the correct order).
        # So run over the array and filter out any undefined elements.
        my int $elems := $!pod-content.elems;
        my int $i     := $elems;
        my $pod := $!pod-content;
        while --$i >= 0 {
            $pod.splice($i,1) unless $pod.EXISTS-POS($i);
        }
    }

    # Add the AST for handling legacy doc generation
    method add-INIT-phaser-for-doc-handling($type) {
# use Pod::To::$type;
# say Pod::To::$type.render($=pod);
# exit
        my $name := RakuAST::Name.from-identifier-parts("Pod","To",$type);
        $!statement-list.add-statement: RakuAST::Statement::Expression.new(
          expression => RakuAST::StatementPrefix::Phaser::Init.new(
            RakuAST::Block.new(
              body => RakuAST::Blockoid.new(
                RakuAST::StatementList.new(
                  RakuAST::Statement::Use.new(module-name => $name),
                  RakuAST::Statement::Expression.new(
                    expression => RakuAST::Call::Name.new(
                      name => RakuAST::Name.from-identifier("say"),
                      args => RakuAST::ArgList.new(
                        RakuAST::ApplyPostfix.new(
                          operand => RakuAST::Type::Simple.new($name),
                          postfix => RakuAST::Call::Method.new(
                            name => RakuAST::Name.from-identifier("render"),
                            args => RakuAST::ArgList.new(
                              RakuAST::Var::Doc.new("pod")
                            )
                          )
                        )
                      )
                    )
                  ),
                  RakuAST::Statement::Expression.new(
                    expression => RakuAST::Call::Name.new(
                      name => RakuAST::Name.from-identifier("exit")
                    )
                  )
                )
              )
            )
          )
        )
    }

    # Replace the statement list of the compilation unit.
    method replace-statement-list(RakuAST::StatementList $statement-list) {
        nqp::bindattr(self, RakuAST::CompUnit, '$!statement-list', $statement-list);
        Nil
    }

    # Replace the finish content (text after =finish) of the compilation unit.
    method replace-finish-content(Mu $finish-content) {
        nqp::bindattr(self, RakuAST::CompUnit, '$!finish-content',
          nqp::hllizefor($finish-content, 'Raku'));
        Nil
    }

    # Indicate that this compilation unit is sunk as a whole (that, is, the result of
    # its last statement is unused).
    method mark-sunk() {
        nqp::bindattr_i(self, RakuAST::CompUnit, '$!is-sunk', 1);
        Nil
    }

    method is-boundary-sunk() {
        $!is-sunk ?? True !! False
    }

    method get-boundary-sink-propagator() {
        $!statement-list
    }

    # Checks if the compilation unit was created in EVAL mode, meaning that it
    # does not declare its own GLOBAL and so forth.
    method is-eval() { $!is-eval ?? True !! False }

    method attach-target-names() {
        self.IMPL-WRAP-LIST(['compunit'])
    }

    method clear-attachments() {
        nqp::setelems($!init-phasers, 0);
        nqp::setelems($!end-phasers, 0);
        self.clear-handler-attachments();
        Nil
    }

    # Set the pod content at the indicated position
    method set-pod-content(int $i, $pod) {

        # don't know where to set, so just push
        if $i == -1 {
            $!pod-content.push($pod);
        }

        # set in assigned position
        else {
            $!pod-content.ASSIGN-POS($i,$pod);
        }
    }

    # fetch =data content object, or create if doesn't exist yet
    method data-content() {
        nqp::ifnull(
          $!data-content,
          nqp::bindattr(self,RakuAST::CompUnit,'$!data-content',
            RakuAST::Doc::Block.Hashray
          )
        )
    }

    # helper method for a given phasers list
    method add-phaser($phasers, $phaser) {
        # Cannot rely on clear-attachments here as a node's attach can be
        # called multiple times while going up and down the tree and
        # clear-attachments will only be called once.
        for $phasers {
            return Nil if $_ =:= $phaser;
        }
        nqp::push($phasers, $phaser);
        Nil
    }

    method add-init-phaser(RakuAST::StatementPrefix::Phaser::Init $phaser) {
        self.add-phaser($!init-phasers, $phaser);
    }

    method add-end-phaser(RakuAST::StatementPrefix::Phaser::End $phaser) {
        self.add-phaser($!end-phasers, $phaser);
    }

    method queue-heredoc($herestub) {
        nqp::push($!herestub-queue, $herestub);
    }

    method ensure-singleton-whatever(RakuAST::Resolver $resolver) {
        unless nqp::isconcrete($!singleton-whatever) {
            my $Whatever := $resolver.resolve-lexical-constant-in-setting('Whatever');
            nqp::bindattr(self, RakuAST::CompUnit, '$!singleton-whatever',
                nqp::create($Whatever.compile-time-value));
        }
        Nil
    }

    method singleton-whatever() { $!singleton-whatever }

    method ensure-singleton-hyper-whatever(RakuAST::Resolver $resolver) {
        unless nqp::isconcrete($!singleton-hyper-whatever) {
            my $HyperWhatever := $resolver.resolve-lexical-constant-in-setting('HyperWhatever');
            nqp::bindattr(self, RakuAST::CompUnit, '$!singleton-hyper-whatever',
                nqp::create($HyperWhatever.compile-time-value));
        }
        Nil
    }

    method singleton-hyper-whatever() { $!singleton-hyper-whatever }

    method cleanup() {
        for $!context.cleanup-tasks {
            $_()
        }
    }

    method record-precompilation-dependencies() {
        $!precompilation-mode ?? True !! False
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier-parts(
                'CompUnit', 'RepositoryRegistry',
            ))
        ])
    }

    method PRODUCE-IMPLICIT-DECLARATIONS() {
        # If we're not in an EVAL, we should produce a GLOBAL package and set
        # it as the current package.
        my @decls;
        unless $!is-eval {
            my $global := RakuAST::Package.new:
                    declarator => 'package',
                    how => $!global-package-how,
                    name => RakuAST::Name.from-identifier('GLOBAL');
            nqp::push(@decls, $global);
            nqp::push(@decls, RakuAST::VarDeclaration::Implicit::Constant.new(
                name => 'GLOBALish', value => $global.compile-time-value));
            nqp::push(@decls, RakuAST::VarDeclaration::Implicit::Constant.new(
                name => 'EXPORT', value => $!export-package)) unless $!export-package =:= Mu;
            nqp::push(@decls, RakuAST::VarDeclaration::Implicit::Constant.new(
                name => '$?PACKAGE', value => $global.compile-time-value
            ));
            nqp::push(@decls, RakuAST::VarDeclaration::Implicit::Special.new(:name('$/')));
            nqp::push(@decls, RakuAST::VarDeclaration::Implicit::Special.new(:name('$!')));
            nqp::push(@decls, RakuAST::VarDeclaration::Implicit::Special.new(:name('$_')));
        }
        nqp::push(@decls, RakuAST::VarDeclaration::Implicit::Constant.new(
            name => '!UNIT_MARKER', value => 1
        ));
        nqp::push(@decls, RakuAST::VarDeclaration::Implicit::Constant.new(
            name => '!RAKUAST_MARKER', value => 1 #TODO remove this when old frontend is gone
        ));
        nqp::push(@decls, RakuAST::VarDeclaration::Implicit::Constant.new(
            name => '!EVAL_MARKER', value => 1
        )) if $!is-eval;
        self.IMPL-WRAP-LIST(@decls)
    }

    method IMPL-TO-QAST-COMP-UNIT(*%options) {
        # Create compilation context.
        my $context := $!context;
        my $top-level :=
            self.IMPL-SET-NODE:
                $!mainline.IMPL-QAST-FORM-BLOCK($context, :blocktype<declaration_static>);
        $top-level.name('<unit>');
        $top-level.annotate('IN_DECL', $!is-eval ?? 'eval' !! 'mainline');
        my @pre-deserialize;
        nqp::push(@pre-deserialize, self.IMPL-SETTING-LOADING-QAST($top-level, $!setting-name))
            if $!setting-name;

        unless $!is-eval {
            my $global_install := QAST::Op.new(
                :op<ifnull>,
                QAST::Op.new(
                    :op<getcurhllsym>,
                    QAST::SVal.new(:value('GLOBAL'))
                ),
                QAST::Op.new(
                    :op('bindcurhllsym'),
                    QAST::SVal.new( :value('GLOBAL') ),
                    QAST::WVal.new( :value(self.generated-global) )
                )
            );
            $context.add-fixup-and-deserialize-task(QAST::Stmt.new($global_install));
        }

        # Compile into a QAST::CompUnit.
        $top-level.push(self.IMPL-TO-QAST($context));
        $!mainline.IMPL-LINK-META-OBJECT($context, $top-level);
        self.add-phasers-to-code-object($!mainline.meta-object);
        self.add-phasers-handling-code($context, $top-level);

        if !$!precompilation-mode
            && !$*INSIDE-EVAL
            && +(@*MODULES // []) == 0
            && (my $main := self.find-lexical('&MAIN'))
        {
            $top-level.set_children([QAST::Op.new(
              :op('call'),
              :name('&RUN-MAIN'),
              QAST::WVal.new(:value($main.meta-object)),
              QAST::Stmts.new(|$top-level.list) # run the mainline and get its result
            )]);
        }

        QAST::CompUnit.new:
            $top-level,
            :hll('Raku'),
            :sc($!sc),
            :is_nested($!is-eval == 2),
            :code_ref_blocks($context.code-ref-blocks),
            :compilation_mode($!precompilation-mode),
            :pre_deserialize(@pre-deserialize),
            :post_deserialize($context.post-deserialize()),
            :repo_conflict_resolver(QAST::Op.new(
                :op('callmethod'), :name('resolve_repossession_conflicts'),
                self.get-implicit-lookups.AT-POS(0).IMPL-TO-QAST($context) )),
            # If this unit is loaded as a module, we want it to automatically
            # execute the mainline code above after all other initializations
            # have occurred.
            :load(QAST::Op.new(
                :op('call'),
                QAST::BVal.new( :value($top-level) ),
            )),
    }

    method IMPL-SETTING-LOADING-QAST(Mu $top-level, Str $name) {
        # Use the NQP module loader to load Perl6::ModuleLoader, which
        # is a normal NQP module.
        my $module-loading := QAST::Stmt.new(
            QAST::Op.new(
                :op('loadbytecode'),
                QAST::VM.new(
                    :jvm(QAST::SVal.new( :value('ModuleLoader.class') )),
                    :moar(QAST::SVal.new( :value('ModuleLoader.moarvm') )),
                    :js(QAST::SVal.new( :value('ModuleLoader') ))
                )),
            QAST::Op.new(
                :op('callmethod'), :name('load_module'),
                QAST::Op.new(
                    :op('gethllsym'),
                    QAST::SVal.new( :value('nqp') ),
                    QAST::SVal.new( :value('ModuleLoader') )
                ),
                QAST::SVal.new( :value('Perl6::ModuleLoader') )
            ));

        # Load and put in place the setting after deserialization and on fixup.
        QAST::Stmt.new(
            $module-loading,
            QAST::Op.new(
                :op('forceouterctx'),
                QAST::BVal.new( :value($top-level) ),
                QAST::Op.new(
                    :op('callmethod'), :name('load_setting'),
                    QAST::Op.new(
                        :op('getcurhllsym'),
                        QAST::SVal.new( :value('ModuleLoader') )
                    ),
                    QAST::SVal.new( :value($name) )
                )
            ));
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $*CU := self;
        self.IMPL-SET-NODE:
            QAST::Stmts.new(
                QAST::Var.new( :name('__args__'), :scope('local'), :decl('param'), :slurpy(1) ),
                self.IMPL-QAST-DECLS($context),
                self.IMPL-QAST-INIT-PHASERS($context),
                self.IMPL-QAST-END-PHASERS($context),
                self.IMPL-QAST-CTXSAVE($context),
                self.IMPL-WRAP-SCOPE-HANDLER-QAST($context, $!statement-list.IMPL-TO-QAST($context)),
            )
    }

    method IMPL-QAST-INIT-PHASERS(RakuAST::IMPL::QASTContext $context) {
        my $setup := QAST::Stmts.new;
        for $!init-phasers {
            my $container := $_.container;
            $context.ensure-sc($container);
            $setup.push(
                QAST::Op.new(
                    :op<bindattr>,
                    QAST::WVal.new( :value($container) ),
                    QAST::WVal.new( :value(Scalar) ),
                    QAST::SVal.new( :value('$!value') ),
                    $_.IMPL-CALLISH-QAST($context)
                )
            );
        }
        $setup
    }

    method IMPL-QAST-END-PHASERS(RakuAST::IMPL::QASTContext $context) {
        my $end-setup := QAST::Stmts.new;
        for $!end-phasers {
            $end-setup.push(QAST::Op.new(
                :op('callmethod'), :name('unshift'),
                QAST::Op.new(
                    :op('getcurhllsym'),
                    QAST::SVal.new( :value('@END_PHASERS') ),
                ),
                QAST::WVal.new( :value($_.meta-object) )
            ));
        }
        $end-setup
    }

    method IMPL-QAST-CTXSAVE(RakuAST::IMPL::QASTContext $context) {
        QAST::Stmts.new(
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('ctxsave'), :scope('local'), :decl('var') ),
                QAST::Var.new( :name('$*CTXSAVE'), :scope('contextual') )
            ),
            QAST::Op.new(
                :op('unless'),
                QAST::Op.new(
                    :op('isnull'),
                    QAST::Var.new( :name('ctxsave'), :scope('local') )
                ),
                QAST::Op.new(
                    :op('if'),
                    QAST::Op.new(
                        :op('can'),
                        QAST::Var.new( :name('ctxsave'), :scope('local') ),
                        QAST::SVal.new( :value('ctxsave') )
                    ),
                    QAST::Op.new(
                        :op('callmethod'), :name('ctxsave'),
                        QAST::Var.new( :name('ctxsave'), :scope('local')
                    )))))
    }

    method generated-global() {
        nqp::die('No generated global in an EVAL-mode compilation unit') if $!is-eval;
        self.IMPL-UNWRAP-LIST(self.get-implicit-declarations)[0].compile-time-value
    }

    method visit-children(Code $visitor) {
        $visitor($!statement-list);
        $visitor($!pod)    if $!pod;
        $visitor($!data)   if $!data;
        $visitor($!finish) if $!finish;
    }
}

# Builds and interns numeric and string literals.
class RakuAST::LiteralBuilder {
    has RakuAST::Resolver $!resolver;
    has Mu $!cached-rat;
    has int $!has-cached-rat;
    has Mu $!cached-complex;
    has int $!has-cached-complex;

    method new(RakuAST::Resolver :$resolver) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::LiteralBuilder, '$!resolver', $resolver) if $resolver;
        $obj
    }

    method set-resolver(RakuAST::Resolver $resolver) {
        nqp::bindattr(self, RakuAST::LiteralBuilder, '$!resolver', $resolver);
    }

    # Build an Int constant and intern it.
    method intern-int(str $chars, int $base, Mu $error-reporter?) {
        # TODO interning
        self.build-int($chars, $base, $error-reporter)
    }

    # Build an Int constant, but do not intern it.
    method build-int(str $source, int $base, Mu $error-reporter?) {
        my $res := nqp::radix_I($base, $source, 0, 2, Int);
        unless nqp::iseq_i(nqp::unbox_i(nqp::atpos($res, 2)), nqp::chars($source)) {
            $error-reporter ??
                $error-reporter()
                !! nqp::die("'$source' is not a valid number");
        }
        nqp::atpos($res, 0)
    }

    # Gets the type object used for Int objects
    method int-type() { Int }

    # Build a Num constant and intern it.
    method intern-num(str $chars) {
        # TODO interning
        self.build-num($chars)
    }

    # Build a Num constant, but do not intern it.
    method build-num(str $chars) {
        nqp::box_n(nqp::numify($chars), Num)
    }

    # Gets the type object used for Num objects
    method num-type() { Num }

    # Build a Str constant and intern it.
    method intern-str(str $chars) {
        # TODO interning
        self.build-str($chars)
    }

    # Build a Str constant, but do not intern it.
    method build-str(str $chars) {
        nqp::box_s($chars, Str)
    }

    # Build a Rat constant and intern it.
    method intern-rat($whole-part, $fractional-part) {
        # TODO interning
        self.build-rat($whole-part, $fractional-part)
    }

    # Build a Rat constant, but do not intern it.
    method build-rat(Mu $whole-part, Mu $fractional-part) {
        # Whole part may be provided as an Int already, or may be missing.
        my $parti;
        if nqp::isconcrete($whole-part) {
            if nqp::istype($whole-part, Int) {
                $parti := $whole-part;
            }
            else {
                $parti := self.intern-int($whole-part, 10);
            }
        }
        else {
            $parti := self.intern-int('0', 10);
        }

        # Now deal with the fractional part; this may also come as an Int
        # denominator.
        my $partf;
        if nqp::istype($fractional-part, Int) {
            $partf := $fractional-part;
        }
        elsif nqp::chars($fractional-part) {
            $partf := nqp::radix_I(10, $fractional-part, 0, 4, Int);
            my $base := nqp::pow_I(nqp::box_i(10, Int), $partf[1], Num, Int);
            $parti := nqp::mul_I($parti, $base, Int);
            $parti := nqp::add_I($parti, $partf[0], Int);
            $partf := $base;
        } else {
            $partf := self.intern-int(1, 10);
        }

        # Produce the Rat object.
        unless $!has-cached-rat {
            nqp::bindattr(self, RakuAST::LiteralBuilder, '$!cached-rat',
              $!resolver.resolve-lexical-constant-in-setting('Rat').compile-time-value);
            nqp::bindattr_i(self, RakuAST::LiteralBuilder, '$!has-cached-rat', 1);
        }
        $!cached-rat.new($parti, $partf)
    }

    # Build a Complex constant and intern it.
    method intern-complex($real-part, $imaginary-part) {
        # TODO interning
        self.build-complex($real-part, $imaginary-part)
    }

    # Build a Complex constant, but do not intern it.
    method build-complex(str $real-part, str $imaginary-part) {
        my num $real := $real-part
          ?? nqp::box_n(nqp::numify($real-part), Num)
          !! 0e0;
        my num $imaginary := $imaginary-part
          ?? nqp::box_n(nqp::numify($imaginary-part), Num)
          !! 1e0;

        # Produce the Complex object.
        unless $!has-cached-complex {
            nqp::bindattr(self, RakuAST::LiteralBuilder, '$!cached-complex',
              $!resolver.resolve-lexical-constant-in-setting('Complex').compile-time-value);
            nqp::bindattr_i(self, RakuAST::LiteralBuilder, '$!has-cached-complex', 1);
        }
        $!cached-complex.new($real, $imaginary)
    }
}
