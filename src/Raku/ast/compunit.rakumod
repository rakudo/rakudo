# A compilation unit is the main lexical scope of a program.
class RakuAST::CompUnit is RakuAST::LexicalScope is RakuAST::SinkBoundary
                        is RakuAST::ImplicitDeclarations is RakuAST::AttachTarget {
    has RakuAST::StatementList $.statement-list;
    has Str $.comp-unit-name;
    has Str $.setting-name;
    has Mu $!sc;
    has int $!is-sunk;
    has int $!is-eval;
    has Mu $!global-package-how;
    has Mu $!end-phasers;
    has Mu $!singleton-whatever;
    has Mu $!singleton-hyper-whatever;

    method new(RakuAST::StatementList :$statement-list, Str :$comp-unit-name!,
            Str :$setting-name, Bool :$eval, Mu :$global-package-how) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!statement-list',
            $statement-list // RakuAST::StatementList.new);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!comp-unit-name', $comp-unit-name);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!setting-name', $setting-name // Str);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!sc', nqp::createsc($comp-unit-name));
        nqp::bindattr_i($obj, RakuAST::CompUnit, '$!is-eval', $eval ?? 1 !! 0);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!global-package-how',
            $global-package-how =:= NQPMu ?? Perl6::Metamodel::PackageHOW !! $global-package-how);
        nqp::bindattr($obj, RakuAST::CompUnit, '$!end-phasers', []);
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

        # Compile into a QAST::CompUnit.
        $top-level.push(self.IMPL-TO-QAST($context));
        QAST::CompUnit.new: $top-level, :hll('Raku'), :sc($!sc),
            :post_deserialize($context.post-deserialize()),
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
            self.IMPL-WRAP-SCOPE-HANDLER-QAST($context, $!statement-list.IMPL-TO-QAST($context))
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

    # Build a Str constant and intern it.
    method intern-str(str $chars) {
        # TODO interning
        self.build-str($chars)
    }

    # Build an Str constant, but do not intern it.
    method build-str(str $chars) {
        nqp::box_s($chars, Str)
    }
}
