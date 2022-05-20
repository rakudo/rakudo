# A compilation unit is the main lexical scope of a program.
class RakuAST::CompUnit is RakuAST::LexicalScope is RakuAST::SinkBoundary
                        is RakuAST::ImplicitLookups
                        is RakuAST::ImplicitDeclarations is RakuAST::AttachTarget {
    has RakuAST::StatementList $.statement-list;
    has Str $.comp-unit-name;
    has Str $.setting-name;
    has Mu $.finish-content;
    has Mu $!sc;
    has int $!is-sunk;
    has int $!is-eval;
    has Mu $!global-package-how;
    has Mu $!end-phasers;
    has Mu $!singleton-whatever;
    has Mu $!singleton-hyper-whatever;
    has int $.precompilation-mode;
    has Mu $!export-package;
    has Mu $.herestub-queue;

    method new(RakuAST::StatementList :$statement-list, Str :$comp-unit-name!,
            Str :$setting-name, Bool :$eval, Mu :$global-package-how,
            Bool :$precompilation-mode, Mu :$export-package) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!statement-list',
            $statement-list // RakuAST::StatementList.new);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!comp-unit-name', $comp-unit-name);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!setting-name', $setting-name // Str);
        my $sc := nqp::createsc($comp-unit-name);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!sc', $sc);
        my $file := nqp::getlexdyn('$?FILES');
        nqp::scsetdesc($sc, $file) unless nqp::isnull($file);
        nqp::bindattr_i($obj, RakuAST::CompUnit, '$!is-eval', $eval ?? 1 !! 0);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!global-package-how',
            $global-package-how =:= NQPMu ?? Perl6::Metamodel::PackageHOW !! $global-package-how);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!export-package',
            $export-package =:= NQPMu ?? Mu !! $export-package);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!end-phasers', []);
        nqp::bindattr_i($obj, RakuAST::CompUnit, '$!precompilation-mode', $precompilation-mode ?? 1 !! 0);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!herestub-queue', []);
        $obj
    }

    # Perform all CHECK-time activities on the compilation unit. This includes
    # doing any symbol resolution, any leftover sink marking, and performing
    # any CHECK-time error checking. This may also produce information useful
    # during optimization, though will not do any transforms in and of itself.
    method check(RakuAST::Resolver $resolver) {
        self.IMPL-CHECK($resolver, False);
    }

    # Replace the statement list of the compilation unit.
    method replace-statement-list(RakuAST::StatementList $statement-list) {
        nqp::bindattr(self, RakuAST::CompUnit, '$!statement-list', $statement-list);
        Nil
    }

    # Replace the finish content (text after =finish) of the compilation unit.
    method replace-finish-content(Mu $finish-content) {
        nqp::bindattr(self, RakuAST::CompUnit, '$!finish-content', $finish-content);
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
        nqp::setelems($!end-phasers, 0);
        self.clear-handler-attachments();
        Nil
    }

    method add-end-phaser(RakuAST::StatementPrefix::Phaser::End $phaser) {
        nqp::push($!end-phasers, $phaser);
        Nil
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
                    package-declarator => 'package',
                    how => $!global-package-how,
                    name => RakuAST::Name.from-identifier('GLOBALish');
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
        self.IMPL-WRAP-LIST(@decls)
    }

    method IMPL-TO-QAST-COMP-UNIT(*%options) {
        # Create compilation context.
        nqp::pushcompsc($!sc);
        my $context := RakuAST::IMPL::QASTContext.new(:sc($!sc));
        my $top-level := QAST::Block.new;
        self.IMPL-ADD-SETTING-LOADING($context, $top-level, $!setting-name) if $!setting-name;

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
        QAST::CompUnit.new:
            $top-level,
            :hll('Raku'),
            :sc($!sc),
            :code_ref_blocks($context.code-ref-blocks),
            :compilation_mode($!precompilation-mode),
            :pre_deserialize(self.IMPL-UNWRAP-LIST([])),
            :post_deserialize($context.post-deserialize()),
            :repo_conflict_resolver(QAST::Op.new(
                :op('callmethod'), :name('resolve_repossession_conflicts'),
                self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].IMPL-TO-QAST($context) )),
            # If this unit is loaded as a module, we want it to automatically
            # execute the mainline code above after all other initializations
            # have occurred.
            :load(QAST::Op.new(
                :op('call'),
                QAST::BVal.new( :value($top-level) ),
            )),
    }

    method IMPL-ADD-SETTING-LOADING(RakuAST::IMPL::QASTContext $context, Mu $top-level, Str $name) {
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
        $context.add-fixup-and-deserialize-task(QAST::Stmt.new(
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
            )));
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Stmts.new(
            QAST::Var.new( :name('__args__'), :scope('local'), :decl('param'), :slurpy(1) ),
            self.IMPL-QAST-DECLS($context),
            self.IMPL-QAST-END-PHASERS($context),
            self.IMPL-QAST-CTXSAVE($context),
            self.IMPL-WRAP-SCOPE-HANDLER-QAST($context, $!statement-list.IMPL-TO-QAST($context)),
        )
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
    }
}

# Builds and interns numeric and string literals.
class RakuAST::LiteralBuilder {
    has RakuAST::Resolver $!resolver;
    has Mu $!cached-rat;
    has int $!has-cached-rat;

    method new(RakuAST::Resolver :$resolver!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::LiteralBuilder, '$!resolver', $resolver);
        $obj
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
}
