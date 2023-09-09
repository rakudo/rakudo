use NQPP6QRegex;
use NQPP5QRegex;

#-------------------------------------------------------------------------------
# The classes of the AST nodes come from the Raku setting bootstrap, so
# we need to load them from there.  We also need the OperatorProperties
# class, so fetch that as well.
my $RakuAST-WHO;         # RakuAST.WHO
my $OperatorProperties;  # OperatorProperties class

# Logic for setting up RakuAST-WHO and OperatorProperties
sub setup-RakuAST-WHO() {
    unless nqp::isconcrete($RakuAST-WHO) {
        my $loader := nqp::gethllsym('Raku','ModuleLoader');
        my $unit   := $loader.load_module('Perl6::BOOTSTRAP::v6c',{},GLOBALish);
        my $export := $unit<EXPORT>.WHO<DEFAULT>.WHO;
        $RakuAST-WHO := nqp::existskey($export,'RakuAST')
          ?? nqp::atkey($export,'RakuAST').WHO
          !! nqp::die('Cannot find RakuAST nodes');
        $OperatorProperties := nqp::atkey($export,'OperatorProperties');
    }
}

# Provide easy lookup of RakuAST:: classes at runtime of the
# actions.  These classes can **NOT** be referenced directly
# as they may not yet be known when the grammar / actions
# run.  Note that direct specification of RakuAST classes
# *will* compile, but may cause compile time issues of Raku
# code, typically resulting in error messages stating a method
# having been called on VMNull.
sub Nodify(*@todo) {
    my $parts := nqp::join('::',@todo);

    my $res := nqp::atkey($RakuAST-WHO,nqp::shift(@todo));
    while @todo && !nqp::isnull($res) {
        $res := nqp::atkey($res.WHO,nqp::shift(@todo));
    }
    nqp::ifnull($res,nqp::die("No such node RakuAST::$parts"))
}

#-------------------------------------------------------------------------------
# Role for all Action classes associated with Raku grammar slangs

role Raku::CommonActions {
    # Some AST nodes need symbol resolution or attachment of position
    # information as we go. This factors out that process and attaches
    # the AST to the match object.
    method attach($/, $node, :$as-key-origin) {
        if nqp::istype($node, Nodify('ImplicitLookups')) {
            $node.resolve-implicit-lookups-with($*R);
        }
        if nqp::istype($node, Nodify('Attaching')) {
            $node.attach($*R);
        }

        self.SET-NODE-ORIGIN($/, $node, :$as-key-origin);

        make $node;
    }

    method SET-NODE-ORIGIN($/, $node, :$as-key-origin) {
        # XXX This is a temporary stub to avoid unimplemented nodes.
        # Must be replaced with exception throwing when RakuAST is
        # considered ready for this.
        unless nqp::isconcrete($node) {
            return
        }
        if nqp::istype($node, Nodify('Node')) {
            unless nqp::isconcrete($node.origin) {
                $node.set-origin(
                    Nodify('Origin').new(
                        :from($/.from),
                        :to($/.to),
                        :source($*ORIGIN-SOURCE)));
            }
            if $as-key-origin {
                my $nestings := @*ORIGIN-NESTINGS;
                unless nqp::istype($node, Nodify('CompUnit')) {
                    @*PARENT-NESTINGS.push($node)
                }
                $node.origin.set-nestings($nestings);
            }
        }
    }

    method key-origin($/) {
        self.SET-NODE-ORIGIN($/, $/.ast, :as-key-origin);
    }

    method quibble($/) {
        self.attach: $/, $<nibble>.ast;
    }

    # Grammars also need to be able to lookup RakuAST nodes.  Historically
    # this was done with the "r" method.  Since it is apparently impossible
    # to reliably export Nodify, this interface is kept alive.
    method r(*@parts) { Nodify(|@parts) }
}

#-------------------------------------------------------------------------------
# The actions associated with the base Raku grammar

class Raku::Actions is HLL::Actions does Raku::CommonActions {
    method  OperatorProperties() { $OperatorProperties }

#-------------------------------------------------------------------------------
# Compilation unit, language version and other entry point bits

    # Thread-safely produce a unique serialization context ID
    my $count := -1;
    my $lock  := NQPLock.new;
    sub next-id() { $lock.protect({ ++$count }) }

    # Given a package, returns a low-level hash for its stash
    sub stash-hash($package) {
        my $hash := $package.WHO;
        nqp::ishash($hash)
          ?? $hash
          !! $hash.FLATTENABLE_HASH
    }

    # Perform all actions that are needed before any actual parsing can
    # be done by a grammar.
    method comp-unit-prologue($/) {

        # Be ready to do Nodify lookups
        setup-RakuAST-WHO();

        # Be ready to report locations in the source.
        $*ORIGIN-SOURCE := Nodify('Origin', 'Source').new(:orig($/.target()));

        # Set up the literals builder, so we can produce and intern literal
        # values.
        $*LITERALS := Nodify('LiteralBuilder').new;

        # Set up the base resolver
        my %OPTIONS       := %*OPTIONS;
        my $context       := %OPTIONS<outer_ctx>;
        my $resolver-type := Nodify('Resolver', 'Compile');
        my $RESOLVER := $*R := nqp::isconcrete($context)
          ?? $resolver-type.from-context(
               :$context, :global(%OPTIONS<global>), :resolver($*OUTER-RESOLVER)
             )
          !! $resolver-type.from-setting(
               :setting-name(%OPTIONS<setting> // 'CORE.d')
             );

        # Make debugging a *lot* easier
        &*DD := $RESOLVER.setting-constant('&dd');
    }

    # Perform all actions related to "use vxxx" and loading appropriate
    # (default) settings and configuring the compilation unit and resolver
    method lang-setup($/) {
        # Look up these dynamic vars only once
        my $HLL-COMPILER := $*HLL-COMPILER;
        my %OPTIONS      := %*OPTIONS;
        my $LANG         := $*LANG;
        my $RESOLVER     := $*R;

        # Some shortcuts;
        my $language-revision := $HLL-COMPILER.language_revision;
        my $is-EVAL           := nqp::isconcrete(%OPTIONS<outer_ctx>);
        my $setting-name      := %OPTIONS<setting>;

        # Helper sub to configure the resolver with selected language revision
        my sub resolver-from-revision() {
            $setting-name := 'CORE.' ~ $HLL-COMPILER.lvs.p6rev($language-revision);
            $RESOLVER.set-setting(:$setting-name);
        }

        # Not EVALling and explicit setting requested
        if $setting-name && !$is-EVAL {
            # TODO This branch is for when we start compiling the CORE.
            if nqp::eqat($setting-name, 'NULL.', 0) {
                $*COMPILING_CORE_SETTING := 1;
                if $setting-name ne 'NULL.c' {
                    my $loader := nqp::gethllsym('Raku', 'ModuleLoader');
                    $*R.set-setting(:setting-name($loader.previous_setting_name($setting-name)));
                }
                else {
                    # TODO CORE.c is being compiled. What resolver is to be used?
                    nqp::die("Can't compiler CORE.c yet");
                }
            }

            # Setting name is explicitly set. Use it to determine the
            # default language revision.
            else {
                $RESOLVER.set-setting(:setting-name($setting-name));
                $language-revision :=
                  nqp::unbox_i($RESOLVER.setting-constant('CORE-SETTING-REV'));
                $HLL-COMPILER.set_language_revision($language-revision);
            }
        }

        # Seen a -use vxxx- statement
        my $version := $<version>
          ?? ~$<version>
          !! nqp::getenvhash()<RAKU_LANGUAGE_VERSION> || "";
        if $version {
            my @vparts         := $HLL-COMPILER.lvs.from-public-repr($version);
            my %lang-revisions := $HLL-COMPILER.language_revisions;
            my @final-version;
            my $modifier-deprecated;

            # Globbed version needs a bit of research needs to be done first.
            if nqp::index($version,'*') >= 0 || nqp::index($version,'+') >= 0 {
                my $Version := $RESOLVER.setting-constant('Version');
                my $ver-requested := $Version.new(
                  $HLL-COMPILER.lvs.from-public-repr($version, :as-str)
                );
                my @can-versions := $HLL-COMPILER.can_language_versions;
                my $can-version;
                my $i := nqp::elems(@can-versions);

                # Iterate over the version candidates from higher to lower
                # ones, skip these that don't match the requested version
                # glob, and these without a modifier but one is required.
                # Like 6.e would be a valid version in the future, but for
                # now it has to be 6.e.PREVIEW.
                while --$i >= 0 {
                    $can-version := $Version.new(@can-versions[$i]);
                    next unless $ver-requested.ACCEPTS($can-version);

                    # If version candidate
                    my $can-parts := $can-version.parts;
                    my $can-revision := nqp::unbox_i($can-parts.head);
                    last
                      unless $can-parts.elems == 1
                          && nqp::existskey(%lang-revisions{$can-revision},'require');
                }

                if $i < 0 {
                    $/.typed-panic: 'X::Language::Unsupported', :$version;
                }

                # Are there any easier way to unbox boxable types?
                my $Int := $RESOLVER.setting-constant('Int');
                my $Str := $RESOLVER.setting-constant('Str');
                my @can-parts := nqp::getattr($can-version, $Version, '$!parts');
                for @can-parts -> $part {
                    @final-version.push: nqp::isint($part) || nqp::isstr($part)
                      ?? $part
                      !! nqp::istype($part, $Int)
                        ?? nqp::unbox_i($part)
                        !! nqp::istype($part, $Str)
                          ?? nqp::unbox_s($part)
                          !! nqp::die(
                               "Don't know how to handle version part of '"
                                 ~ $part.HOW.name($part)
                                 ~ "' type"
                             );
                }
            }

            # A non-globbed version can be used as-is, make sure it is valid
            else {
                my $revision := @vparts[0];
                # Consider version to have a language modifier if the last
                # part of is a string of non-zero length.
                my $modifier := @vparts > 1 && nqp::objprimspec(@vparts[-1]) == 3
                  ?? @vparts[-1]
                  !! nqp::null();

                # Do we know this language version?
                unless nqp::existskey(%lang-revisions, $revision)
                  && (!$modifier || nqp::existskey(%lang-revisions{$revision}<mods>, $modifier))
                {
                    $/.typed-panic: 'X::Language::Unsupported', :$version;
                }

                my %config := %lang-revisions{$revision};
                # If version is known, is it used with a required modifier?
                if nqp::existskey(%config,'require')
                  && (!$modifier || %config<require> ne $modifier) {
                    $/.typed-panic: 'X::Language::ModRequired',
                      :$version, :modifier(%config<require>);
                }

                # We can't issue a worry immediately because the current
                # resolver is temporary, so just set a flag
                if $modifier && %config<mods>{$modifier}<deprecate> {
                    $modifier-deprecated := $modifier;
                }

                @final-version := @vparts;
            }

            $HLL-COMPILER.set_language_version(@final-version);
            $language-revision := @final-version[0];
            resolver-from-revision();

            # Now the resolver is final, express our modifier concern!
            if $modifier-deprecated {
                # At this point our compiler version is final.
                $/.worry:
                  "$modifier-deprecated modifier is deprecated for Raku v"
                    ~ $HLL-COMPILER.language_version;
            }
        }

        # No version seen and not in an EVAL
        elsif !$is-EVAL {
            resolver-from-revision();
        }

        # Locate an EXPORTHOW and set those mappings on our current language.
        my $EXPORTHOW :=
          $RESOLVER.resolve-lexical-constant('EXPORTHOW').compile-time-value;
        for stash-hash($EXPORTHOW) {
            $LANG.set_how($_.key, $_.value);
        }

        my $package-how    := $LANG.how('package');
        my $export-package := $package-how.new_type(name => 'EXPORT');
        $export-package.HOW.compose($export-package);
        $RESOLVER.set-export-package($export-package);
        $*EXPORT := $export-package;

        # Create a compilation unit.
        my $comp-unit-name := $*ORIGIN-SOURCE.original-file ~ $/.target;

        # It's an EVAL. We'll take our GLOBAL, $?PACKAGE, etc. from that.
        if $is-EVAL {
            $*CU := Nodify('CompUnit').new(
              :comp-unit-name($comp-unit-name ~ next-id),  # uniqify
              :$setting-name,
              :eval,
              :outer-cu($*OUTER-CU),
              :$language-revision
            );
        }

        # Top-level compilation.
        else {
            $*CU := Nodify('CompUnit').new(
              :$comp-unit-name,
              :$setting-name,
              :global-package-how($package-how),
              :precompilation-mode(%OPTIONS<precomp>),
              :$export-package,
              :$language-revision
            );

            # Create a GLOBAL using the correct package meta-object.
            my $global := $*CU.generated-global;
            $RESOLVER.set-global($global);
            nqp::bindhllsym('Raku','GLOBAL',$global);
        }

        $*LITERALS.set-resolver($RESOLVER);
    }

    method comp-unit($/) {
        # Do dynamic lookups once
        my $COMPUNIT := $*CU;
        my %OPTIONS  := %*OPTIONS;
        my $RESOLVER := $*R;

        # Put the body in place.
        $COMPUNIT.replace-statement-list($<statementlist>.ast);

        # Sort out sinking; the compilation unit is sunk as a whole if we are
        # not in a REPL or EVAL context.
        $COMPUNIT.mark-sunk() unless nqp::existskey(%OPTIONS,'outer_ctx');
        $COMPUNIT.calculate-sink();

        # if --(raku)doc specified, add INIT phaser that handles that
        if nqp::existskey(%OPTIONS,'doc') {
            $COMPUNIT.add-INIT-phaser-for-doc-handling(
              'Pod', %OPTIONS<doc> || 'Text'
            );
        }
        elsif nqp::existskey(%OPTIONS,'rakudoc') {
            $COMPUNIT.add-INIT-phaser-for-doc-handling(
              'RakuDoc', %OPTIONS<rakudoc> || 'Text'
            );
        }

        # Have check time.
        $COMPUNIT.check($RESOLVER);
        my $exception := $RESOLVER.produce-compilation-exception;
        if nqp::isconcrete($exception) {
            if $RESOLVER.has-compilation-errors {
                # Really has errors, so report them.
                $exception.throw;
            }
            else {
                # Only potential difficulties, just just print them.
                stderr().print($exception.gist);
            }
        }

        self.attach: $/, $COMPUNIT, :as-key-origin;
    }

    # Action method to load any modules specified with -M
    method load-M-modules($/) {
        my $M := %*OPTIONS<M>;
        unless nqp::defined($M) {
            return;  # nothing to do here
        }

        # Create a RakuAST statement list with -use- statements
        # of the specified module names and attach that
        my $ast := Nodify('StatementList').new;
        for nqp::islist($M) ?? $M !! [$M] -> $longname {
            my $use := Nodify('Statement', 'Use').new(
              module-name => Nodify('Name').from-identifier-parts(
                |nqp::split('::', $longname)
              )
            );
            $use.ensure-begin-performed($*R, $*CU.context);
            $ast.add-statement: $use;
        }
        self.attach: $/, $ast;
    }

#-------------------------------------------------------------------------------
# Statement level handling

    # Helper method to collect statements and potentially and declarator
    # docs for an actual StatementList, StatementSequence or SemiList.
    method collect-statements($/, $typename) {
        my $statements := Nodify($typename).new;
        for $<statement> {
            $_.ast.add-to-statements($statements);
        }
        self.attach: $/, $statements;
        $statements
    }

    # Action methods for the various collectors of statements
    method statementlist($/) {
        my $statements := self.collect-statements($/, 'StatementList');

        # Add any uncollected doc blocks.  This can happen if there
        # are no statements in a statementlist, e.g. in a rakudoc
        # only file.
        for $*DOC-BLOCKS-COLLECTED {
            $statements.add-doc-block($_);
        }
        $*DOC-BLOCKS-COLLECTED := [];
    }
    method semilist($/) { self.collect-statements($/, 'SemiList')          }
    method sequence($/) { self.collect-statements($/, 'StatementSequence') }

    # Action method for handling an actual statement
    method statement($/) {

        # Setting label on already created statement
        if $<label> {
            my $statement := $<statement>.ast;
            $statement.add-label($<label>.ast);
            make $statement;
            return;       # nothing left to do here
        }

        # Statement ID must be captured before creation of statement object
        my $statement-id := $*STATEMENT-ID;
        my $statement;

        # Handle expression with optional condition/loop modifiers
        if $<EXPR> {
            my $expression := $<EXPR>.ast;
            if nqp::istype($expression, Nodify('ColonPairs')) {
                $expression := Nodify('ApplyListInfix').new:
                  :infix(Nodify('Infix').new(',')),
                  :operands($expression.colonpairs);
            }
            $statement := Nodify('Statement','Expression').new(:$expression);
            $statement.replace-condition-modifier($<statement-mod-cond>.ast)
              if $<statement-mod-cond>;
            $statement.replace-loop-modifier($<statement-mod-loop>.ast)
              if $<statement-mod-loop>;
        }

        # Handle statement control (if / for / given / when / etc.)
        elsif $<statement-control> {
            $statement := $<statement-control>.ast;
        }

        # Handle an empty statement
        else {
            $statement := Nodify('Statement','Empty').new;
        }

        # Final statement tweaks
        $statement.set-trace($/.pragma('trace') ?? 1 !! 0);
        $statement.set-statement-id($statement-id);
        $statement.attach-doc-blocks unless $*PARSING-DOC-BLOCK;

        self.attach: $/, $statement;
    }

    # Action method for handling labels attached to a statement
    method label($/) {
        my $name := ~$<identifier>;
        my $decl := Nodify('Label').new($name);
        $/.typed-panic('X::Redeclaration', :symbol($name))
          if $*R.declare-lexical($decl);
        self.attach: $/, $decl;
    }

    # Helper method for attaching (pointy) blocks
    method attach-block($/, $signature?) {
        my $block := $*BLOCK;
        $block.replace-signature($signature.ast) if $signature;
        $block.replace-body($<blockoid>.ast);
        $block.ensure-begin-performed($*R, $*CU.context);
        self.attach: $/, $block;
    }

    # Action methods for handling (pointy) blocks
    method pointy-block($/) { self.attach-block($/, $<signature>) }
    method        block($/) { self.attach-block($/)               }

    # Action method for handling the inside of (pointy) blocks
    method blockoid($/) {
        self.attach: $/,
          Nodify('Blockoid').new($<statementlist>.ast),
          :as-key-origin;
    }

    # Action method for handling "unit" scoped packages
    method unit-block($/) {
        my $block := $*BLOCK;
        # Wrap the statements into a (non-existing) blockoid
        $block.replace-body(Nodify('Blockoid').new($<statementlist>.ast));
        $block.ensure-begin-performed($*R, $*CU.context);
        self.attach: $/, $block;
    }

    # Action method handling {*}
    method onlystar($/) {
        self.attach: $/, Nodify('OnlyStar').new;
    }

    # Helper method to connect any leading declarator doc that was
    # collected already to the given declarand
    method set-declarand($/, $it) {

        # Ignoring this one
        if $*IGNORE-NEXT-DECLARAND {
            $*IGNORE-NEXT-DECLARAND := 0;
        }

        # Should handle
        else {
            my $from    := $/.from;
            my $worries := $*DECLARAND-WORRIES;
            for $worries {
                $_.value.typed-worry:
                  'X::Syntax::Doc::Declarator::MissingDeclarand'
                  if $_.key < $from;
                nqp::deletekey($worries, $_.key);
            }

            $*DECLARAND          := $it;
            $*LAST-TRAILING-LINE := +$*ORIGIN-SOURCE.original-line($from);

            if @*LEADING-DOC -> @leading {
                $it.set-leading(@leading);
                @*LEADING-DOC := [];
            }
            $*IGNORE-NEXT-DECLARAND := nqp::istype($it,Nodify('Package'));
        }
        $it
    }

    # Helper methof to steal the information of the current declarand
    # into the given declarand, and make *that* the declarand.  Needed
    # for cases like subsets with a where, where the where block is
    # seen *before* the subset, causing leading declarator doc to be
    # attached to the where block, rather than to the subset.
    method steal-declarand($/, $it) {
        $it.set-WHY($*DECLARAND.cut-WHY);
        $*DECLARAND          := $it;
        $*LAST-TRAILING-LINE := +$*ORIGIN-SOURCE.original-line($/.from);
    }

    # Action method when entering a scope (package, sub, phaser etc.)
    # Assume the $*BLOCK dynamic var is appropriately localized as it
    # will set that with the RakuAST:: object being created.
    method enter-block-scope($/) {
        my $block := $*MULTINESS
          ?? Nodify($*SCOPE-KIND).new(:multiness($*MULTINESS))
          !! Nodify($*SCOPE-KIND).new;
        $*R.enter-scope($block);
        $*BLOCK := $block;

        self.set-declarand($/, $block)
          if nqp::istype($block,Nodify('Doc','DeclaratorTarget'));
    }

    # Action method when leaving a scope.
    method leave-block-scope($/) {
        $*R.leave-scope();
    }

#-------------------------------------------------------------------------------
# Statement control

    # Helper method for control statements taking a block without any
    # intervening expression
    method takes-none($/, $name) {
        self.attach: $/, Nodify('Statement',$name).new(body => $<block>.ast);
    }

    # Helper method for control statements taking a source and a pointy
    # block
    method takes-source($/, $name) {
        self.attach: $/, Nodify('Statement',$name).new(
          source => $<EXPR>.ast, body => $<pointy-block>.ast
        );
    }

    # Helper method for conditional statements taking a condition and
    # a pointy block
    method takes-cond($/, $name) {
        self.attach: $/, Nodify('Statement',$name).new(
          condition => $<EXPR>.ast, body => $<pointy-block>.ast
        );
    }

    # Helper method for looping statements taking a condition and
    # a pointy block
    method takes-loop($/, $name) {
        self.attach: $/, Nodify('Statement','Loop',$name).new(
          condition => $<EXPR>.ast, body => $<pointy-block>.ast
        );
    }

    # Handling of simple control statements that take a block
    method statement-control:sym<default>($/) { self.takes-none($/,'Default') }
    method statement-control:sym<CATCH>($/)   { self.takes-none($/,'Catch')   }
    method statement-control:sym<CONTROL>($/) { self.takes-none($/,'Control') }

    # Handling of simple control statements that take a pointy block
    method statement-control:sym<for>($/)   { self.takes-source($/,'For')   }
    method statement-control:sym<given>($/) { self.takes-source($/,'Given') }
    method statement-control:sym<unless>($/)  { self.takes-cond($/,'Unless')  }
    method statement-control:sym<when>($/)    { self.takes-cond($/,'When')    }
    method statement-control:sym<without>($/) { self.takes-cond($/,'Without') }

    # Handling of all forms of loops
    method statement-control:sym<repeat>($/) {
        self.takes-loop($/, 'Repeat' ~ nqp::tclc(~$<wu>))
    }
    method statement-control:sym<while>($/) {
        self.takes-loop($/, nqp::tclc(~$<sym>))
    }

    # Handling of whenever
    method statement-control:sym<whenever>($/) {
        self.attach: $/,
          Nodify('Statement', 'Whenever').new:
            trigger => $<EXPR>.ast, body => $<pointy-block>.ast;
    }

    # Dummy control statement to set a trait on a target
    method statement-control:sym<also>($/) {
        if $*ALSO-TARGET -> $target {
            for $<trait> {
                $target.add-trait($_.ast);
            }
            $target.apply-traits($*R, $*CU.context, $target);
            self.attach: $/, Nodify('Statement','Empty').new;
        }
        else {
            $/.panic("Could not find target for 'also'");
        }
    }

    # Basic if / with handling with all of the elsifs / orelses
    method statement-control:sym<if>($/) {

        # collect the if and all of the elsifs / orelses
        my @elsifs;
        my $index := 0;
        for $<sym> {
            @elsifs.push:
              Nodify('Statement', nqp::tclc(~$_)).new:
                condition => $<condition>[$index].ast,
                then      => $<then>[$index].ast;
            ++$index;
        }

        # the first is the main part, add others if appropriate
        my $ast := @elsifs.shift;
        $ast.set-elsifs(@elsifs)   if @elsifs;
        $ast.set-else($<else>.ast) if $<else>;
        self.attach: $/, $ast;
    }

    # Basic loop handling
    method statement-control:sym<loop>($/) {
        my %parts;
        %parts<setup>     := $<e1>.ast if $<e1>;
        %parts<condition> := $<e2>.ast if $<e2>;
        %parts<increment> := $<e3>.ast if $<e3>;
        %parts<body>      := $<block>.ast;
        self.attach: $/, Nodify('Statement', 'Loop').new(|%parts);
    }

#-------------------------------------------------------------------------------
# Pragma and module loading related statements

    # "no foo" can only mean a pragma at the moment
    method statement-control:sym<no>($/) {
        my str $name := ~$<module_name>;
        my $Pragma   := Nodify('Pragma');
        if $Pragma.IS-PRAGMA($name) {
            my $ast := $<arglist><EXPR>
              ?? $Pragma.new(:$name, :argument($<arglist><EXPR>.ast), :off)
              !! $Pragma.new(:$name, :off);
            $ast.ensure-begin-performed($*R, $*CU.context);
            self.attach: $/, $ast;
        }
        else {
            nqp::die("Don't know how to 'no " ~ $name ~ "'just yet")
        }
    }

    method statement-control:sym<use>($/) {
        my str $name := ~$<module_name>;
        my $Pragma   := Nodify('Pragma');
        my $ast;

        if $Pragma.IS-PRAGMA($name) {
            $ast := $<arglist><EXPR>
              ?? $Pragma.new(:$name, :argument($<arglist><EXPR>.ast))
              !! $Pragma.new(:$name);
            $ast.ensure-begin-performed($*R, $*CU.context);
        }

        # proper module loading
        else {
            $ast := $<arglist><EXPR>
              ?? Nodify('Statement', 'Use').new(
                   :module-name($<module_name>.ast),
                   :argument($<arglist><EXPR>.ast)
                 )
              !! Nodify('Statement', 'Use').new(
                   :module-name($<module_name>.ast)
                 );
            $ast.ensure-begin-performed($*R, $*CU.context);
            for $ast.IMPL-UNWRAP-LIST($ast.categoricals) {
                $/.add-categorical(
                  $_.category, $_.opname, $_.canname, $_.subname, $_.declarand);
            }
        }

        self.attach: $/, $ast;
    }

    method statement-control:sym<need>($/) {
        my @module-names;
        for $<module_name> {
            @module-names.push: $_.ast;
        }

        my $ast := Nodify('Statement', 'Need').new(:@module-names);
        $ast.ensure-begin-performed($*R, $*CU.context);

        self.attach: $/, $ast;
    }

    method statement-control:sym<import>($/) {
        my $ast := $<arglist><EXPR>
          ?? Nodify('Statement', 'Import').new(
               :module-name($<module_name>.ast),
               :argument($<arglist><EXPR>.ast)
             )
          !! Nodify('Statement', 'Import').new(
               :module-name($<module_name>.ast)
             );
        $ast.IMPL-CHECK($*R, $*CU.context, 1);
        for $ast.IMPL-UNWRAP-LIST($ast.categoricals) {
            $/.add-categorical(
              $_.category, $_.opname, $_.canname, $_.subname, $_.declarand);
        }

        self.attach: $/, $ast;
    }

    method statement-control:sym<require>($/) {
        #TODO non-trivial cases, args
        self.attach: $/, Nodify('Statement', 'Require').new(
            module-name => $<module_name>.ast,
        );
    }

#-------------------------------------------------------------------------------
# Statement modifiers

    # Helper method to attach an expression
    method modifier-expr($/) {
        self.attach: $/, $<EXPR>.ast;
    }

    # Helper method for setting up statement modifiers
    method SM-cond($/, $name) {
        self.attach: $/,
          Nodify('StatementModifier', $name).new($<modifier-expr>.ast)
    }

    # Simple statement modifiers
    method statement-mod-cond:sym<if>($/)      { self.SM-cond($/, 'If')      }
    method statement-mod-cond:sym<unless>($/)  { self.SM-cond($/, 'Unless')  }
    method statement-mod-cond:sym<when>($/)    { self.SM-cond($/, 'When')    }
    method statement-mod-cond:sym<with>($/)    { self.SM-cond($/, 'With')    }
    method statement-mod-cond:sym<without>($/) { self.SM-cond($/, 'Without') }

    # Statement modifiers that set $_
    method statement-mod-loop:sym<for>($/)     { self.SM-cond($/, 'For')     }
    method statement-mod-loop:sym<given>($/)   { self.SM-cond($/, 'Given')   }
    method statement-mod-loop:sym<until>($/)   { self.SM-cond($/, 'Until')   }
    method statement-mod-loop:sym<while>($/)   { self.SM-cond($/, 'While')   }

#-------------------------------------------------------------------------------
# Phasers

    # Helper method for setting up simple phasers that just take a blorst
    method SP-phaser($/, $name) {
        self.attach: $/,
          Nodify('StatementPrefix', 'Phaser', $name).new($<blorst>.ast)
    }

    # Simple phasers that just take a blorst
    method statement-prefix:sym<CHECK>($/) { self.SP-phaser($/, 'Check') }
    method statement-prefix:sym<CLOSE>($/) { self.SP-phaser($/, 'Close') }
    method statement-prefix:sym<END>($/)   { self.SP-phaser($/, 'End')   }
    method statement-prefix:sym<ENTER>($/) { self.SP-phaser($/, 'Enter') }
    method statement-prefix:sym<FIRST>($/) { self.SP-phaser($/, 'First') }
    method statement-prefix:sym<INIT>($/)  { self.SP-phaser($/, 'Init')  }
    method statement-prefix:sym<KEEP>($/)  { self.SP-phaser($/, 'Keep')  }
    method statement-prefix:sym<LAST>($/)  { self.SP-phaser($/, 'Last')  }
    method statement-prefix:sym<LEAVE>($/) { self.SP-phaser($/, 'Leave') }
    method statement-prefix:sym<NEXT>($/)  { self.SP-phaser($/, 'Next')  }
    method statement-prefix:sym<QUIT>($/)  { self.SP-phaser($/, 'Quit')  }
    method statement-prefix:sym<UNDO>($/)  { self.SP-phaser($/, 'Undo')  }

    # BEGIN phaser needs to be executed *now* and produce a value
    method statement-prefix:sym<BEGIN>($/) {
        my $ast :=
          Nodify('StatementPrefix','Phaser','Begin').new($<blorst>.ast);
        $ast.ensure-begin-performed($*R, $*CU.context);
        self.attach: $/, $ast;
    }

    # PRE/POST phasers need a stringification of the blorst as well
    method statement-prefix:sym<PRE>($/) {
        self.attach: $/, Nodify(
          'StatementPrefix', 'Phaser', 'Pre'
        ).new($<blorst>.ast, ~$<blorst>);
    }
    method statement-prefix:sym<POST>($/) {
        self.attach: $/, Nodify(
          'StatementPrefix', 'Phaser', 'Post'
        ).new($<blorst>.ast, ~$<blorst>);
    }

    # DOC phaser only works if so activated on command line
    method statement-prefix:sym<DOC>($/) {
        if %*OPTIONS<doc> || %*OPTIONS<rakudoc> {
            my $phase := ~$<phase>;
            $phase eq 'BEGIN'
              ?? self.statement-prefix:sym<BEGIN>($/)
              !! self.SP-phaser($/, nqp::tclc($phase));
        }

        # not activated
        else {
            self.attach: $/, self.Nil
        }
    }

#-------------------------------------------------------------------------------
# Statement prefixes

    # Helper method to normalize a blorst
    method blorst($/) { self.attach: $/, $<block>.ast }

    # Helper method for setting up simple prefix that just take a blorst
    method SP-prefix($/, $name) {
        self.attach: $/,
          Nodify('StatementPrefix', $name).new($<blorst>.ast)
    }

    # Simple prefixes that just take a blorst
    method statement-prefix:sym<do>($/)      { self.SP-prefix($/, 'Do')      }
    method statement-prefix:sym<eager>($/)   { self.SP-prefix($/, 'Eager')   }
    method statement-prefix:sym<gather>($/)  { self.SP-prefix($/, 'Gather')  }
    method statement-prefix:sym<once>($/)    { self.SP-prefix($/, 'Once')    }
    method statement-prefix:sym<quietly>($/) { self.SP-prefix($/, 'Quietly') }
    method statement-prefix:sym<react>($/)   { self.SP-prefix($/, 'React')   }
    method statement-prefix:sym<start>($/)   { self.SP-prefix($/, 'Start')   }
    method statement-prefix:sym<supply>($/)  { self.SP-prefix($/, 'Supply')  }
    method statement-prefix:sym<try>($/)     { self.SP-prefix($/, 'Try')     }

    # Helper method for statement prefixes that modify for loops
    method SP-looper($/, $mode) {
        my $ast := $<blorst>.ast;
        if nqp::istype($ast, Nodify('Statement', 'For')) {
            $ast.replace-mode(nqp::lc($mode));
            self.attach: $/, $ast;
        }
        else {
            self.SP-prefix($/, $mode);
        }
    }

    # Prefixes that work differently on for loops
    method statement-prefix:sym<hyper>($/) { self.SP-looper($/, 'Hyper') }
    method statement-prefix:sym<lazy>($/)  { self.SP-looper($/, 'Lazy')  }
    method statement-prefix:sym<race>($/)  { self.SP-looper($/, 'Race')  }

#-------------------------------------------------------------------------------
# Expression generation

    # Just a term
    method EXPR($/) {
        self.attach: $/, $/.ast // $<OPER>.ast;
        $/  # simplies end of EXPR "token"
    }

    # A ternary expression
    method TERNARY-EXPR($/) {
        self.attach: $/, Nodify('Ternary').new:
          condition => $/[0].ast, then => $/[1].ast, else => $/[2].ast;
    }

    # An assignment, or infix expression
    method INFIX-EXPR($/) {
        if $<infix><sym> eq '=' {
            my $lhs := $/[0];
            if nqp::istype($lhs,Nodify('ApplyPostfix')) {
                my $postfix := $lhs.postfix;
                if (nqp::istype(        # [foo]
                    $postfix,
                    Nodify('Postcircumfix','ArrayIndex')
                  ) && $postfix.index.statements.elems == 1
                ) || nqp::istype(       # <bar>
                       $postfix,
                       Nodify('Postcircumfix','LiteralHashIndex')
                     ) {
                    $postfix.set-assignee($/[1]);
                    self.attach: $/, $lhs;
                    return;
                }
            }
        }

        my $infix := $/.ast;
        if nqp::istype($infix, Nodify('DottyInfixish')) {
            self.attach: $/, Nodify('ApplyDottyInfix').new:
              infix => $infix, left => $/[0].ast, right => $/[1].ast;
        }
        else {
            self.attach: $/, Nodify('ApplyInfix').new:
              infix => $infix, left => $/[0].ast, right => $/[1].ast;
        }
    }

    # A listy expression
    method LIST-EXPR($/) {
        my @operands;
        for $/.list {
            my $ast := $_.ast;
            @operands.push($ast) if nqp::isconcrete($ast);
        }
        self.attach: $/, Nodify('ApplyListInfix').new:
          infix => $/.ast, operands => @operands;
    }

    # A prefix expression
    method PREFIX-EXPR($/) {
        self.attach: $/, Nodify('ApplyPrefix').new:
          prefix  => $/.ast // Nodify('Prefix').new($<prefix><sym>),
          operand => $/[0].ast;
    }

    # A postfix expression
    method POSTFIX-EXPR($/) {
        my $ast     := $/.ast;
        my $operand := $/[0].ast;
        my $cp      := $<colonpair>;

        if $cp {
            if $*ADVERB-AS-INFIX
              && (nqp::istype($operand, Nodify('ColonPair'))
                    || nqp::istype($operand, Nodify('ColonPairs'))
                 ) {
                self.attach: $/, Nodify('ColonPairs').new($operand,$cp.ast);
            }
            else {
                $operand.add-colonpair($<colonpair>.ast);
                make $operand;
            }
        }

        elsif nqp::istype($ast, Nodify('Call', 'Name')) {
            $ast.args.push: $operand;
            self.attach: $/, $ast;
        }

        elsif $ast {
            if nqp::istype($ast, Nodify('Postfixish')) {
                self.attach: $/, Nodify('ApplyPostfix').new:
                  postfix => $ast, operand => $operand;
            }
            # Report the sorry if there is no more specific sorry already
            elsif !$*R.has-sorries {
                $/.typed-sorry: 'X::Syntax::Confused';
            }
        }
        else {
            self.attach: $/, Nodify('ApplyPostfix').new:
              postfix => Nodify('Postfix').new($<postfix><sym>),
              operand => $operand;
        }
    }

#-------------------------------------------------------------------------------
# Operators

    method prefixish($/) {
        my $ast := $<OPER>.ast // Nodify('Prefix').new(~$<prefix><sym>);
        if $<prefix-postfix-meta-operator> {
            $ast := $<prefix-postfix-meta-operator>.ast.new($ast);
        }
        self.attach: $/, $ast;
    }

    method prefix-postfix-meta-operator:sym<«>($/) {
        make Nodify('MetaPrefix', 'Hyper');
    }

    method postfixish($/) {
        my $ast := $<OPER>.ast // Nodify('Postfix').new(~$<postfix><sym>);
        if $<postfix-prefix-meta-operator> {
            $ast := $<postfix-prefix-meta-operator>.ast.new($ast);
        }
        self.attach: $/, $ast;
    }

    method postfix-prefix-meta-operator:sym<»>($/) {
        # Check if we are inside «...» quoters and complain if the hyper creates
        # ambiguity with the quoters, since user may not wanted to have a hyper
        my str $sym := ~$<sym>;
        if ($/.pragma("STOPPER") // '') eq $sym {
            $/.worry:
                "Ambiguous use of $sym; use "
                ~ ($sym eq '>>' ?? '»' !! '>>')
                ~ " instead to mean hyper, or insert whitespace before"
                ~ " $sym to mean a quote terminator (or use different delimiters?)";
        }

        make Nodify('MetaPostfix', 'Hyper');
    }

    method postop($/) {
        self.attach: $/, $<postfix> ?? $<postfix>.ast !! $<postcircumfix>.ast;
    }

    method postcircumfix:sym<[ ]>($/) {
        self.attach: $/, Nodify('Postcircumfix', 'ArrayIndex').new(:index($<semilist>.ast));
    }

    method postcircumfix:sym<{ }>($/) {
        self.attach: $/, Nodify('Postcircumfix', 'HashIndex').new($<semilist>.ast);
    }

    method postcircumfix:sym<ang>($/) {
        self.attach: $/, Nodify('Postcircumfix', 'LiteralHashIndex').new(:index($<nibble>.ast));
    }

    method postcircumfix:sym«<< >>»($/) {
        self.attach: $/, Nodify('Postcircumfix', 'LiteralHashIndex').new(:index($<nibble>.ast));
    }

    method postcircumfix:sym<« »>($/) {
        self.attach: $/, Nodify('Postcircumfix', 'LiteralHashIndex').new(:index($<nibble>.ast));
    }

    method postcircumfix:sym<( )>($/) {
        self.attach: $/, Nodify('Call', 'Term').new(args => $<arglist>.ast);
    }

    method dotty:sym<.>($/) {
        self.attach: $/, $<dottyop>.ast;
    }

    method dotty:sym<.^>($/) {
        self.attach: $/, $<dottyop>.ast;
    }

    method dotty:sym<.?>($/) {
        self.attach: $/, $<dottyop>.ast;
    }

    method dotty:sym<.&>($/) {
        self.attach: $/, $<dottyop>.ast;
    }

    method dottyop($/) {
        if $<methodop> {
            self.attach: $/, $<methodop>.ast;
        }
        elsif $<postop> {
            self.attach: $/, $<postop>.ast;
        }
        elsif $<colonpair> {
            if $<colonpair><identifier> eq "" && $<colonpair><coloncircumfix> -> $cf {
                if $cf<circumfix> -> $op_name {
                    self.attach: $/, Nodify('Call', 'Name').new(
                        :name(
                            Nodify('Name').from-identifier(
                                'prefix:' ~ Nodify('ColonPairish').IMPL-QUOTE-VALUE(
                                    Nodify('BeginTime').IMPL-BEGIN-TIME-EVALUATE(
                                      ($op_name<nibble> // $op_name<semilist> // $op_name<pointy-block>).ast, $*R, $*CU.context
                                    )
                                )
                            )
                        ),
                        :p5isms($*LANG.pragma('p5isms')),
                        :has-terminator-or-infix($*MISSING)
                    );
                }
                else {
                    nqp::die('NYI kind of dottyop with coloncircumfix');
                }
            } else {
                self.attach: $/, $<colonpair>.ast;
            }
        }
        else {
            nqp::die('NYI kind of dottyop');
        }
    }

    method privop($/) {
        self.attach: $/, $<methodop>.ast;
    }

    method methodop($/) {
        my $args := $<args> ?? $<args>.ast !! Nodify('ArgList').new();
        if $<longname> {
            my $longname := $<longname>.ast;
            my $name     := $longname.canonicalize;

            if $*DOTTY {
                my $DOTTY    := $*DOTTY;
                unless $longname.is-identifier {
                    $/.dotty-non-ident($DOTTY);
                }
                if $DOTTY eq '!' {
                    self.attach: $/,Nodify('Call','PrivateMethod').new(:name($longname),:$args);
                }
                elsif $DOTTY eq '.^' {
                    self.attach: $/, Nodify('Call', 'MetaMethod').new(:$name, :$args);
                }
                elsif $DOTTY eq '.?' {
                    self.attach: $/, Nodify('Call', 'MaybeMethod').new(:$name, :$args);
                }
                elsif $DOTTY eq '.&' {
                    self.attach: $/, Nodify('Call', 'VarMethod').new(:name($longname), :$args);
                }
                else {
                    nqp::die("Missing compilation of $DOTTY");
                }
            }
            else {
                self.attach: $/, Nodify('Call', 'Method').new(:name($longname), :$args);
            }
        }
        elsif $<quote> {
            self.attach: $/, Nodify('Call', 'QuotedMethod').new(:name($<quote>.ast), :$args);
        }
        else {
            nqp::die('NYI kind of methodop');
        }
    }

    sub super-int-to-Int($digits, $sign = "") {
        intify($sign, $digits, "⁰¹²³⁴⁵⁶⁷⁸⁹")
    }
    sub sub-int-to-Int($digits, $sign = "") {
        intify($sign, $digits, "₀₁₂₃₄₅₆₇₈₉")
    }

    sub intify($sign, $digits, $from) {
        my $Int       := $*LITERALS.int-type;
        my $value     := nqp::box_i(0, $Int);
        my int $i     := -1;
        my int $chars := nqp::chars($digits);

        while ++$i < $chars {
            $value := nqp::add_I(
              nqp::mul_I($value, nqp::box_i(10, $Int), $Int),
              nqp::box_i(nqp::index($from, nqp::substr($digits,$i,1)), $Int),
              $Int
            );
        }
        $sign eq '⁻' || $sign eq '¯' ?? nqp::neg_I($value,$Int) !! $value
    }

    method postfix:sym<ⁿ>($/) {
        self.attach: $/, Nodify('Postfix', 'Power').new(
          super-int-to-Int(~$<power><super-integer>, ~$<power><super-sign>)
        );
    }

    method postfix:sym<+>($/) {
        my $v := $<vulgar>;
        my int $nu;
        my int $de;

        # 4²/₃₃
        if $v<super-integer> -> $super {
            $nu := super-int-to-Int(~$super);
            $de := sub-int-to-Int(~$v<sub-integer>);
            if $nu >= $de {
                $/.panic("Numerator must be less than denominator: $nu >= $de")
            }
        }

        # 22⅔
        else {
            my $ord := nqp::ord(~$v);
            $nu := ord-to-numerator($ord);
            $de := ord-to-denominator($ord);
        }

        self.attach: $/, Nodify('Postfix', 'Vulgar').new(
          $*LITERALS.intern-rat($nu, $de)
        );
    }

    # Assignment in Raku can take two forms: item and list assignment:
    # my @b = my $a = 1,2,3;  # $a = 1, @b = 1,2,3 .  In the item
    # assignment case, the '=' gets a higher precedence than the ','.
    # In the list assignment case, a lowed precedence.  So the expression
    # is really: my @b = (my $a = 1),2,3 .  The grammar is supposed to
    # set the $*ITEM dynamic variable to a truthy value if item assignment
    # is to be assumed.
    method infix:sym<=>($/) {
        make Nodify('Assignment').new(:item($*ITEM));
    }

    method infix:sym«==>»($/)  { self.attach: $/, Nodify('Feed').new($<sym>) }
    method infix:sym«<==»($/)  { self.attach: $/, Nodify('Feed').new($<sym>) }
    method infix:sym«==>>»($/) { self.attach: $/, Nodify('Feed').new($<sym>) }
    method infix:sym«<<==»($/) { self.attach: $/, Nodify('Feed').new($<sym>) }

    method infixish($/) {
        return 0 if $<adverb-as-infix>;

        my $ast;
        if $<infix> {
            my str $op := ~$<infix>;
            $ast := $<infix>.ast;
            unless $ast {
                $ast := Nodify('Infix').new(
                  # A ternary op arrives here as '?? expression !!', so
                  # check for that and create a dummy infix AST for further
                  # parsing / EXPR handling if so.
                  nqp::eqat($op,'??',0)
                    && nqp::eqat($op,'!!',nqp::chars($op) - 2)
                    ?? '?? !!'
                    !! $op
                );
            }
        }

        elsif $<infix-prefix-meta-operator> {
            $ast := $<infix-prefix-meta-operator>.ast;
        }

        elsif $<infix-circumfix-meta-operator> {
            $ast := $<infix-circumfix-meta-operator>.ast;
        }
        elsif $<infixish> {
            $ast := Nodify('BracketedInfix').new($<infixish>.ast);
        }
        elsif $<variable> {
            $ast := Nodify('FunctionInfix').new($<variable>.ast);
        }
        else {
            nqp::die('unknown kind of infix');
        }

        if $<infix-postfix-meta-operator> {
            $ast := $<infix-postfix-meta-operator>.ast.new($ast);
        }
        self.attach: $/, $ast;
    }

    method infix-prefix-meta-operator:sym<!>($/) {
        self.attach: $/, Nodify('MetaInfix', 'Negate').new($<infixish>.ast);
    }

    method infix-prefix-meta-operator:sym<R>($/) {
        self.attach: $/, Nodify('MetaInfix', 'Reverse').new($<infixish>.ast);
    }

    method revO($/) {
        my $O := nqp::clone($*FROM);
        if    $O<assoc> eq 'right' { $O<assoc> := 'left' }
        elsif $O<assoc> eq 'left'  { $O<assoc> := 'right' }
        make $O;
    }

    method infix-prefix-meta-operator:sym<X>($/) {
        self.attach: $/, Nodify('MetaInfix', 'Cross').new($<infixish>.ast);
    }

    method infix-prefix-meta-operator:sym<Z>($/) {
        self.attach: $/, Nodify('MetaInfix', 'Zip').new($<infixish>.ast);
    }

    method infix-postfix-meta-operator:sym<=>($/) {
        self.attach: $/, Nodify('MetaInfix', 'Assign');
    }

    method infix-circumfix-meta-operator:sym<« »>($/) {
        self.attach: $/, Nodify('MetaInfix', 'Hyper').new:
            infix => $<infixish>.ast,
            dwim-left => $<opening> eq '«',
            dwim-right => $<closing> eq '»'
    }

    method infix-circumfix-meta-operator:sym«<< >>»($/) {
        self.attach: $/, Nodify('MetaInfix', 'Hyper').new:
            infix => $<infixish>.ast,
            dwim-left => $<opening> eq '<<',
            dwim-right => $<closing> eq '>>'
    }

    method circumfix:sym<( )>($/) {
        self.attach: $/, Nodify('Circumfix', 'Parentheses').new($<semilist>.ast);
    }

    method circumfix:sym<[ ]>($/) {
        self.attach: $/, Nodify('Circumfix', 'ArrayComposer').new($<semilist>.ast);
    }

    method circumfix:sym<{ }>($/) {
        self.attach: $/, $<pointy-block>.ast.block-or-hash;
    }

    method circumfix:sym<ang>($/) { self.attach: $/, $<nibble>.ast; }

    method circumfix:sym«<< >>»($/) { self.attach: $/, $<nibble>.ast; }

    method circumfix:sym<« »>($/) { self.attach: $/, $<nibble>.ast; }

    method infix:sym<.>($/) {
        self.attach: $/, Nodify('DottyInfix', 'Call').new;
    }

    method infix:sym<.=>($/) {
        self.attach: $/, Nodify('DottyInfix', 'CallAssign').new;
    }

    ##
    ## Stubs
    ##

    method term:sym<...>($/) {
        self.attach: $/, Nodify('Stub', 'Fail').new(
          args => $<args>.ast
        );
    }

    method term:sym<???>($/) {
        self.attach: $/, Nodify('Stub', 'Warn').new(
          args => $<args>.ast
        );
    }

    method term:sym<!!!>($/) {
        self.attach: $/, Nodify('Stub', 'Die').new(
          args => $<args>.ast
        );
    }

    ##
    ## Terms
    ##

    method term:sym<::?IDENT>($/) {
        self.attach: $/, Nodify('Var', 'Lexical', 'Constant').new(~$/);
    }

    method term:sym<self>($/) {
        self.attach: $/, Nodify('Term', 'Self').new();
    }

    method term:sym<now>($/) {
        self.attach: $/, Nodify('Term', 'Named').new('now');
    }

    method term:sym<time>($/) {
        self.attach: $/, Nodify('Term', 'Named').new('time');
    }

    method term:sym<empty_set>($/) {
        self.attach: $/, Nodify('Term', 'EmptySet').new();
    }

    method term:sym<rand>($/) {
        self.attach: $/, Nodify('Term', 'Rand').new();
    }

    method term:sym<fatarrow>($/) {
        self.attach: $/, Nodify('FatArrow').new:
            key => $*LITERALS.intern-str(~$<key>),
            value => $<val>.ast;
    }

    method term:sym<colonpair>($/) {
        self.attach: $/, $<colonpair>.ast;
    }

    method term:sym<variable>($/) {
        self.attach: $/, $<variable>.ast;
    }

    method term:sym<package-declarator>($/) {
        self.attach: $/, $<package-declarator>.ast;
    }

    method term:sym<scope-declarator>($/) {
        self.attach: $/, $<scope-declarator>.ast;
    }

    method term:sym<routine-declarator>($/) {
        self.attach: $/, $<routine-declarator>.ast
    }

    method term:sym<multi-declarator>($/) {
        self.attach: $/, $<multi-declarator>.ast;
    }

    method term:sym<regex-declarator>($/) {
        self.attach: $/, $<regex-declarator>.ast;
    }

    method term:sym<type-declarator>($/) {
        self.attach: $/, $<type-declarator>.ast;
    }

    method term:sym<statement-prefix>($/) {
        self.attach: $/, $<statement-prefix>.ast;
    }

    method term:sym<*>($/) {
        self.attach: $/, Nodify('Term', 'Whatever').new;
    }

    method term:sym<**>($/) {
        self.attach: $/, Nodify('Term', 'HyperWhatever').new;
    }

    method term:sym<lambda>($/) {
        self.attach: $/, $<pointy-block>.ast;
    }

    method term:sym<value>($/) {
        self.attach: $/, $<value>.ast;
    }

    method term:sym<identifier>($/) {
        self.attach: $/, Nodify('Call', 'Name').new:
            name => Nodify('Name').from-identifier(~$<identifier>),
            args => $<args>.ast,
            p5isms => $*LANG.pragma('p5isms'),
            has-terminator-or-infix => $*MISSING;
    }

    method term:sym<nqp::op>($/) {
        self.attach: $/, Nodify('Nqp').new: ~$<op>, $<args>.ast;
    }

    method term:sym<nqp::const>($/) {
        self.attach: $/, Nodify('Nqp', 'Const').new(~$<const>);
    }

    method term:sym<name>($/) {
        my $name := $<longname>.ast;
        if $<args> {
            my $args := $<args>.ast;
            if $args.invocant {
                # Indirect method call syntax, e.g. new Int: 1
                self.attach: $/, Nodify('ApplyPostfix').new:
                    :operand($args.invocant),
                    :postfix(Nodify('Call', 'Method').new(:$name, :$args));
            }
            else {
                self.attach: $/, Nodify('Call', 'Name').new: name => $name, args => $args, p5isms => $*LANG.pragma('p5isms'), has-terminator-or-infix => $*MISSING
            }
        }
        else {
            if $*is-type {
                self.attach: $/, self.type-for-name($/, $name);
            }
            else {
                self.attach: $/, Nodify('Term', 'Name').new($name);
            }
        }
    }

    method term:sym<dotty>($/) {
        self.attach: $/, Nodify('Term', 'TopicCall').new($<dotty>.ast);
    }

    method term:sym<capture>($/) {
        self.attach: $/, Nodify('Term', 'Capture').new($<args>.ast);
    }

    method term:sym<onlystar>($/) {
        self.attach: $/, Nodify('OnlyStar').new;
    }

    method colonpair($/) {
        my $key-str := $*key;
        if $key-str {
            my $key := $*LITERALS.intern-str($key-str);
            if $<num> {
                my $value := Nodify('IntLiteral').new($*LITERALS.intern-int(~$<num>, 10));
                self.attach: $/, Nodify('ColonPair', 'Number').new(:$key, :$value);
            }
            elsif $<coloncircumfix> {
                my $value := $<coloncircumfix>.ast;
                self.attach: $/, Nodify('ColonPair', 'Value').new(:$key, :$value);
            }
            elsif $<var> {
                my $value := $<var>.ast;
                self.attach: $/, Nodify('ColonPair', 'Variable').new(:$key, :$value);
            }
            else {
                self.attach: $/,
                  Nodify('ColonPair', $<neg> ?? 'False' !! 'True').new($key);
            }
        }
        elsif $<fakesignature> {
            make $<fakesignature>.ast;
        }
        else {
            make $<coloncircumfix>.ast;
        }
    }

    method Nil() {
        Nodify('Term', 'Name').new(Nodify('Name').from-identifier('Nil'))
    }

    method coloncircumfix($/) {
        self.attach: $/, $<circumfix> ?? $<circumfix>.ast !! self.Nil
    }

    method colonpair-variable($/) {
        if $<capvar> {
            self.attach: $/, Nodify('Var', 'NamedCapture').new:
                Nodify('QuotedString').new:
                    :segments(Nodify('QuotedString').IMPL-WRAP-LIST([
                        Nodify('StrLiteral').new(~$<desigilname>)
                    ]));
        }
        else {
            my str $sigil := ~$<sigil>;
            my str $twigil := $<twigil> ?? ~$<twigil> !! '';
            my $desigilname := $<desigilname><longname>
                ?? $<desigilname><longname>.ast
                !! Nodify('Name').from-identifier(~$<desigilname>);
            self.compile_variable_access($/, $sigil, $twigil, $desigilname);
        }
    }

    method variable($/) {
        if $<index> {
            self.attach: $/, Nodify('Var', 'PositionalCapture').new($*LITERALS.intern-int(~$<index>, 10));
        }
        elsif $<postcircumfix> {
            self.attach: $/, Nodify('Var', 'NamedCapture').new($<postcircumfix>.ast.index);
        }
        elsif $<contextualizer> {
            self.attach: $/, $<contextualizer>.ast;
        }
        elsif $<infixish> {
            my $name := Nodify('Name').from-identifier('infix');
            $name.add-colonpair(
                Nodify('QuotedString').new(
                    :segments($name.IMPL-WRAP-LIST([
                        Nodify('StrLiteral').new($<infixish>.Str)
                    ]))
                )
            );
            self.compile_variable_access($/, '&', '', $name);
        }
        elsif $<desigilname><variable> {
            self.contextualizer-for-sigil($/, ~$<sigil>, $<desigilname><variable>.ast);
        }
        else {
            my str $sigil := ~$<sigil>;
            my str $twigil := $<twigil> ?? ~$<twigil> !! '';
            my $desigilname := $<desigilname><longname>
                ?? $<desigilname><longname>.ast
                !! Nodify('Name').from-identifier(~$<desigilname>);
            self.compile_variable_access($/, $sigil, $twigil, $desigilname);
        }
    }

    method compile_variable_access($/, $sigil, $twigil, $desigilname) {
        $desigilname.IMPL-CHECK($*R, $*CU.context, 1);
        my str $name := $sigil ~ $twigil ~ $desigilname.canonicalize;
        if $twigil eq '' && $desigilname.is-empty {
            # Generate an anonymous state variable.
            self.attach: $/, Nodify('VarDeclaration', 'Anonymous').new(:$sigil, :scope('state'));
        }
        elsif $name eq '@_' {
            my $decl := Nodify('VarDeclaration', 'Placeholder', 'SlurpyArray').new();
            $*R.declare-lexical($decl);
            self.attach: $/, $decl;
        }
        elsif $name eq '%_' {
            my $decl := Nodify('VarDeclaration', 'Placeholder', 'SlurpyHash').new();
            $*R.declare-lexical($decl);
            self.attach: $/, $decl;
        }
        elsif $twigil eq '' {
            if $desigilname.is-identifier {
                if $*LANG.pragma("strict") || $*R.resolve-lexical($name) {
                    self.attach: $/, Nodify('Var', 'Lexical').new(:$sigil, :$desigilname);
                }
                else {
                    my $decl := Nodify('VarDeclaration', 'Auto').new:
                        :scope<our>, :$desigilname, :$sigil, :$twigil;
                    $*R.declare-lexical($decl);
                    self.attach: $/, $decl;
                }
            }
            else { # package variable
                self.attach: $/, Nodify('Var', 'Package').new(
                    :name($desigilname),
                    :$sigil
                );
            }
        }
        elsif $twigil eq '*' {
            self.attach: $/, Nodify('Var', 'Dynamic').new($name);
        }
        elsif $twigil eq '!' {
            self.attach: $/, Nodify('Var', 'Attribute').new($name);
        }
        elsif $twigil eq '?' {
            my $origin-source := $*ORIGIN-SOURCE;
            if $name eq '$?FILE' {
                my str $file := $origin-source.original-file();
                self.attach: $/, Nodify('Var', 'Compiler', 'File').new($*LITERALS.intern-str($file));
            }
            elsif $name eq '$?LINE' {
                my int $line := $origin-source.original-line($/.from());
                self.attach: $/, Nodify('Var', 'Compiler', 'Line').new($*LITERALS.intern-int($line, 10));
            }
            elsif $name eq '&?BLOCK' {
                self.attach: $/, Nodify('Var', 'Compiler', 'Block').new;
            }
            elsif $name eq '&?ROUTINE' {
                self.attach: $/, Nodify('Var', 'Compiler', 'Routine').new;
            }
            else {
                self.attach: $/, Nodify('Var', 'Compiler', 'Lookup').new($name);
            }
        }
        elsif $twigil eq '^' {
            my $decl := Nodify('VarDeclaration', 'Placeholder', 'Positional').new:
                    $sigil ~ $desigilname.canonicalize;
            $*R.declare-lexical($decl);
            self.attach: $/, $decl;
        }
        elsif $twigil eq ':' {
            my $decl := Nodify('VarDeclaration', 'Placeholder', 'Named').new:
                    $sigil ~ $desigilname.canonicalize;
            $*R.declare-lexical($decl);
            self.attach: $/, $decl;
        }
        elsif $twigil eq '=' {
            if $name eq '$=pod'
              || $name eq '$=data'
              || $name eq '$=finish'
              || $name eq '$=rakudoc' {
                self.attach: $/, Nodify('Var', 'Doc').new(nqp::substr($name,2));
            }
            else {
                nqp::die("Pod variable $name NYI");
            }
        }
        elsif $twigil eq '.' {
            self.attach: $/, Nodify('ApplyPostfix').new:
                :postfix(
                    Nodify('Call', 'Method').new:
                        # contextualize based on sigil
                        :name(Nodify('Name').from-identifier(
                              $sigil eq '@' ?? 'list' !!
                              $sigil eq '%' ?? 'hash' !!
                              'item')),
                        :args(Nodify('ArgList').new)),
                :operand(
                    Nodify('ApplyPostfix').new:
                        :postfix(
                            Nodify('Call', 'Method').new(
                                :name($desigilname),
                                :args($<arglist> ?? $<arglist>.ast !! Nodify('ArgList').new),
                            )),
                        :operand(
                            Nodify('Term', 'Self').new
                        ));
        }
        elsif $twigil eq '~' {
            my $grammar := $/.slang_grammar($desigilname.canonicalize);
            my $actions := $/.slang_actions($desigilname.canonicalize);
            self.attach: $/, Nodify('Var', 'Slang').new(:$grammar, :$actions);
        }
        else {
            nqp::die("Lookup with twigil '$twigil' NYI");
        }
    }

    method contextualizer($/) {
        self.contextualizer-for-sigil($/, ~$<sigil>, $<coercee>.ast);
    }

    method contextualizer-for-sigil($/, $sigil, $target) {
        my str $node-type := $sigil eq '@' ?? 'List' !!
                             $sigil eq '%' ?? 'Hash' !!
                                              'Item';
        self.attach: $/, Nodify('Contextualizer', $node-type).new($target);
    }

    method term:sym<reduce>($/) {
        my $infix := $<op>.ast // Nodify('Infix').new($<op><OPER><sym>);
        self.attach: $/, Nodify('Term', 'Reduce').new(:$infix, :args($<args>.ast),
            :triangle(?$<triangle>));
    }

    ##
    ## Declarations
    ##

    method package-declarator:sym<package>($/) { self.attach: $/, $<package-def>.ast; }
    method package-declarator:sym<module>($/)  { self.attach: $/, $<package-def>.ast; }
    method package-declarator:sym<class>($/)   { self.attach: $/, $<package-def>.ast; }
    method package-declarator:sym<grammar>($/) { self.attach: $/, $<package-def>.ast; }
    method package-declarator:sym<role>($/)    { self.attach: $/, $<package-def>.ast; }
    method package-declarator:sym<knowhow>($/) { self.attach: $/, $<package-def>.ast; }
    method package-declarator:sym<native>($/)  { self.attach: $/, $<package-def>.ast; }

    sub is-yada($/) {
        $<blockoid><statementlist>
          && nqp::elems($<blockoid><statementlist><statement>) == 1
          && ~$<blockoid><statementlist><statement>[0]
               ~~ /^ \s* ['...'|'???'|'!!!'|'…'] \s* $/;
    }

    method package-def($/) {
        my $package := $*PACKAGE;
        my $body := $<block> ?? $<block>.ast !! $<unit-block>.ast;

        if is-yada($<block> || $<unit-block>) {
            $package.set-is-stub(1);
            return self.attach: $/, $package;
        }

        $package.replace-body($body, $<signature> ?? $<signature>.ast !! Mu);
        $package.IMPL-CHECK($*R, $*CU.context, 1);
        $package.IMPL-COMPOSE();
        self.attach: $/, $package;
    }

    method stub-package($/) {
        # Resolve the meta-object.
        my $declarator := $*PKGDECL;
        my $how;
        if $/.know_how($declarator) {
            $how := $/.how($declarator);
        }
        else {
            $/.panic("Cannot resolve meta-object for $declarator")
        }

        # Stub the package AST node.
        my str $scope := $*SCOPE // 'our';
        my $name-match := $*PACKAGE-NAME;
        my $name := $name-match ?? $name-match.ast !! Nodify('Name');
        if $scope eq 'augment' {
            $*PACKAGE := Nodify('Package', 'Augmented').new: :$declarator, :$how, :$name, :$scope;
            $*PACKAGE.IMPL-CHECK($*R, $*CU.context, 1);
        }
        else {
            $*PACKAGE := my $package := Nodify('Package').new: :$declarator, :$how, :$name, :$scope;
            $package.resolve-with($*R);
        }
        self.set-declarand($/, $*PACKAGE);
    }

    method enter-package-scope($/) {
        # Perform BEGIN-time effects (declaring the package, applying traits,
        # etc.)
        $*PACKAGE.ensure-begin-performed($*R, $*CU.context);

        # Let the resolver know which package we're in.
        $*R.push-package($*PACKAGE);

        if $*SIGNATURE {
            my $parameterization := $*SIGNATURE.ast;
            for $parameterization.IMPL-UNWRAP-LIST($parameterization.parameters) {
                if $_.target {
                    $*R.declare-lexical($_.target);
                }
            }
        }
    }

    method leave-package-scope($/) {
        $*R.pop-package();
    }

    method scope-declarator:sym<my>($/)    { self.attach: $/, $<scoped>.ast; }
    method scope-declarator:sym<our>($/)   { self.attach: $/, $<scoped>.ast; }
    method scope-declarator:sym<has>($/)   { self.attach: $/, $<scoped>.ast; }
    method scope-declarator:sym<HAS>($/)   { self.attach: $/, $<scoped>.ast; }
    method scope-declarator:sym<anon>($/)  { self.attach: $/, $<scoped>.ast; }
    method scope-declarator:sym<state>($/) { self.attach: $/, $<scoped>.ast; }
    method scope-declarator:sym<unit>($/)  { self.attach: $/, $<scoped>.ast; }

    method scope-declarator:sym<augment>($/) { self.attach: $/, $<scoped>.ast; }

    method scoped($/) {
        self.attach: $/, $<DECL>.ast;
    }

    method multi-declarator:sym<multi>($/) {
        self.attach: $/, $<declarator> ?? $<declarator>.ast !! $<routine-def>.ast;
    }

    method multi-declarator:sym<proto>($/) {
        self.attach: $/, $<declarator> ?? $<declarator>.ast !! $<routine-def>.ast;
    }

    method multi-declarator:sym<only>($/) {
        self.attach: $/, $<declarator> ?? $<declarator>.ast !! $<routine-def>.ast;
    }

    method multi-declarator:sym<null>($/) {
        self.attach: $/, $<declarator>.ast;
    }

    method declarator($/) {
        if $<variable-declarator> {
            self.attach: $/, $<variable-declarator>.ast;
        }
        elsif $<type-declarator> {
            self.attach: $/, $<type-declarator>.ast;
        }
        elsif $<signature> {
            my str $scope := $*SCOPE;
            my $type := $*OFTYPE ?? $*OFTYPE.ast !! Nodify('Type');
            my $initializer := $<initializer>
                ?? $<initializer>.ast
                !! Nodify('Initializer');

            my $decl := Nodify('VarDeclaration', 'Signature').new:
                :signature($<signature>.ast), :$scope, :$type, :$initializer;
            for $<trait> {
                $decl.add-trait($_.ast);
            }
            self.attach: $/, $decl;
        }
        elsif $<routine-declarator> {
            self.attach: $/, $<routine-declarator>.ast;
        }
        elsif $<defterm> {
            my str $scope := $*SCOPE;
            my $type := $*OFTYPE ?? $*OFTYPE.ast !! Nodify('Type');
            my $name := $<defterm>.ast;
            my $initializer := $<term_init>.ast;
            my $decl := Nodify('VarDeclaration', 'Term').new:
                :$scope, :$type, :$name, :$initializer;
            $/.typed-sorry('X::Redeclaration', :symbol($name))
                if $*R.declare-lexical($decl);
            self.attach: $/, $decl;
        }
        else {
            nqp::die('Unimplemented declarator');
        }
    }

    method initializer:sym<=>($/) {
        self.attach: $/, Nodify('Initializer', 'Assign').new($<EXPR>.ast);
    }

    method initializer:sym<:=>($/) {
        self.attach: $/, Nodify('Initializer', 'Bind').new($<EXPR>.ast);
    }

    method initializer:sym<.=>($/) {
        self.attach: $/, Nodify('Initializer', 'CallAssign').new($<dottyop>.ast);
    }

    method stub-variable($stub) {
        my $/ := $*VARIABLE-MATCH;

        my str $scope := $*SCOPE;
        my $type := $*OFTYPE ?? $*OFTYPE.ast !! Nodify('Type');

        my $decl;
        if $<desigilname> {
            my $desigilname := $<desigilname><longname>
                ?? $<desigilname><longname>.ast
                !! Nodify('Name').from-identifier(~$<desigilname>);
            my str $sigil := $<sigil>;
            my str $twigil := $<twigil> || '';
            my $shape := $<semilist> ?? $<semilist>[0].ast !! Nodify('SemiList');
            $desigilname.IMPL-CHECK($*R, $*CU.context, 1);
            my $dynprag := $*LANG.pragma('dynamic-scope');
            my $forced-dynamic := $dynprag
                ?? $dynprag($sigil ~ $twigil ~ $desigilname.canonicalize)
                !! 0;
            $decl := Nodify('VarDeclaration', 'Simple').new:
                :$scope, :$type, :$desigilname, :$sigil, :$twigil,
                :$shape, :$forced-dynamic;
            if $scope eq 'my' || $scope eq 'state' || $scope eq 'our' {
                my str $name := $<sigil> ~ ($<twigil> || '') ~ $desigilname;
                $/.typed-worry('X::Redeclaration', :symbol($name))
                  if $*R.declare-lexical($decl);
            }
            self.set-declarand($/, $decl);
        }
        else {
            if $scope ne 'my' && $scope ne 'state' {
                $/.panic("Cannot declare an anonymous {$scope}-scoped variable");
            }
            if $<twigil> {
                $/.panic("Cannot declare an anonymous variable with a twigil");
            }
            $decl := Nodify('VarDeclaration', 'Anonymous').new:
                :$scope, :$type, :sigil(~$<sigil>);
        }

        $*VARIABLE := $decl;
    }

    method variable-declarator($/) {
        my $decl := $*VARIABLE;
        my str $scope := $*SCOPE;
        my $type := $*OFTYPE ?? $*OFTYPE.ast !! Nodify('Type');

        $/.panic("Cannot use := to initialize an attribute")
          if $scope eq 'has' && $<initializer><sym> eq ':=';

        $decl.set-initializer($<initializer>.ast) if $<initializer>;

        for $<trait> {
            $decl.add-trait($_.ast);
        }

        self.attach: $/, $decl;
    }

    method routine-declarator:sym<sub>($/) {
        self.attach: $/, $<routine-def>.ast;
    }
    method routine-declarator:sym<method>($/) {
        self.attach: $/, $<method-def>.ast;
    }
    method routine-declarator:sym<submethod>($/) {
        self.attach: $/, $<method-def>.ast;
    }

    method routine-def($/) {
        my $routine := $*BLOCK;
        if $<signature> {
            $routine.replace-signature($<signature>.ast);
        }
        $routine.replace-body($<onlystar>
          ?? Nodify('OnlyStar').new
          !! $<blockoid>.ast
        );
        $routine.IMPL-CHECK($*R, $*CU.context, 1);
        self.attach: $/, $routine;
    }

    method method-def($/) {
        my $routine := $*BLOCK;
        if $<signature> {
            $routine.replace-signature($<signature>.ast);
        }
        if $<specials> {
            if ~$<specials> eq '^' {
                $routine.set-meta(1);
            }
            elsif ~$<specials> eq '!' {
                $routine.set-private(1);
            }
        }
        $routine.replace-body($<onlystar>
          ?? Nodify('OnlyStar').new
          !! $<blockoid>.ast
        );
        $routine.IMPL-CHECK($*R, $*CU.context, 1);
        $routine.ensure-begin-performed($*R, $*CU.context);
        self.attach: $/, $routine;
    }

    method regex-declarator:sym<regex>($/) {
        self.attach: $/, $<regex-def>.ast;
    }

    method regex-declarator:sym<token>($/) {
        self.attach: $/, $<regex-def>.ast;
    }

    method regex-declarator:sym<rule>($/) {
        self.attach: $/, $<regex-def>.ast;
    }

    method regex-def($/) {
        my $regex := $*BLOCK;
        $regex.set-source(~$/);
        if $<signature> {
            $regex.replace-signature($<signature>.ast);
        }
        $regex.replace-body($<nibble>.ast);
        $regex.IMPL-CHECK($*R, $*CU.context, 1);
        $regex.ensure-begin-performed($*R, $*CU.context);
        self.attach: $/, $regex;
    }

    method type-declarator:sym<constant>($/) {
        # Provided it's named, install it.
        my %args;
        if $<defterm> {
            %args<name> := $<defterm>.ast.canonicalize;
        }
        elsif $<variable> {
            if $<variable><twigil> {
                my $twigil := ~$<variable><twigil>;

                if $twigil eq '?' {
                    unless $*COMPILING_CORE_SETTING {
                        $/.typed-panic('X::Comp::NYI',
                          feature => "Constants with a '$twigil' twigil"
                        );
                    }
                }
                elsif $twigil eq '*' {
                    $/.typed-panic('X::Syntax::Variable::Twigil',
                      name       => ~$<variable>,
                      what       => 'constant',
                      twigil     => $twigil,
                      scope      => $*SCOPE,
                      additional => ' because values cannot be constant and dynamic at the same time',
                    );
                }
                # Don't handle other twigil'd case yet.
                else {
                    $/.typed-panic('X::Comp::NYI',
                      feature => "Constants with a '$twigil' twigil");
                }
            }

            %args<name> := ~$<variable>;
        }
        %args<scope> := $*SCOPE;
        %args<type>  := $*OFTYPE.ast if nqp::defined($*OFTYPE);
        %args<initializer> := $<initializer>.ast;
        if $<trait> {
            %args<traits> := my @traits;
            @traits.push($_.ast) for $<trait>;
        }

        my $decl := Nodify('VarDeclaration', 'Constant').new(|%args);
        $/.typed-panic('X::Redeclaration', :symbol(%args<name>))
          if $*R.declare-lexical($decl);
        $decl.IMPL-CHECK($*R, $*CU.context, 1);
        self.attach: $/, $decl;
    }

    method type-declarator:sym<enum>($/) {
        # TODO: <variable> being defined means we should throw an NYI
        # Need to support anonymous enums
        my $name := $<longname>
                        ?? $<longname>.ast
                        !! Nodify('Name').from-identifier('');
        my $base-type := $*OFTYPE ?? $*OFTYPE.ast !! Nodify("Type");
        my $decl := Nodify('Type', 'Enum').new(
            :name($name),
            :term($<term>.ast),
            :scope($*SCOPE),
            :of($base-type)
        );
        self.set-declarand($/, $decl);
        for $<trait> {
            $decl.add-trait($_.ast)
        }
        $decl.IMPL-CHECK($*R, $*CU.context, 1);
        self.attach: $/, $decl;
    }

    method type-declarator:sym<subset>($/) {
        my $where := $<EXPR> ?? $<EXPR>.ast !! Mu;
        my $decl  := Nodify('Type', 'Subset').new(
            :name($<longname>.ast),
            :where($where),
            :scope($*SCOPE)
        );

        $where && $*DECLARAND
          ?? self.steal-declarand($/, $decl)
          !! self.set-declarand($/, $decl);

        for $<trait> {
            $decl.add-trait($_.ast);
        }
        $decl.IMPL-CHECK($*R, $*CU.context, 1);
        self.attach: $/, $decl;
    }

    method trait($/) {
        my $trait := $<trait_mod>.ast;
        if $trait { # is repr(...) won't be handled as a trait
            if $*TARGET {
                # Already have the target to apply it to.
                $*TARGET.add-trait($trait);
            }
            else {
                # Will be added to a target later, so attach to the match.
                self.attach: $/, $trait;
            }
        }
    }

    method trait_mod:sym<is>($/) {
        if ~$<longname> eq 'repr' {
            if $<circumfix> {
                my $circumfix := $<circumfix>.ast;
                my $repr := nqp::istype($circumfix, Nodify('Circumfix'))
                    ?? $<circumfix>.ast.IMPL-UNWRAP-LIST($<circumfix>.ast.semilist.statements)[0]
                    !! nqp::istype($circumfix, Nodify('QuotedString'))
                        ?? $circumfix
                        !! nqp::die("NYI trait_mod circumfix " ~ $circumfix.HOW.name($circumfix));
                unless $repr.IMPL-CAN-INTERPRET {
                    $/.typed-panic('X::Value::Dynamic', :what('is repr(...) trait'));
                }
                $repr := $repr.IMPL-INTERPRET(Nodify('IMPL', 'InterpContext').new);
                $*PACKAGE.set-repr($repr);
                return;
            }
            else {
                $/.panic("is repr(...) trait needs a parameter");
            }
        }
        else
        {
            my $ast-type := Nodify('Trait', 'Is');
            my $trait := $<circumfix>
                ?? $ast-type.new(:name($<longname>.ast), :argument($<circumfix>.ast))
                !! $ast-type.new(:name($<longname>.ast));
            $trait.ensure-begin-performed($*R, $*CU.context);
            self.attach: $/, $trait;
        }
    }

    method trait_mod:sym<hides>($/) {
        self.attach: $/, Nodify('Trait', 'Hides').new($<typename>.ast);
    }

    method trait_mod:sym<does>($/) {
        self.attach: $/, Nodify('Trait', 'Does').new($<typename>.ast);
    }

    method trait_mod:sym<of>($/) {
        self.attach: $/, Nodify('Trait', 'Of').new($<typename>.ast);
    }

    method trait_mod:sym<returns>($/) {
        self.attach: $/, Nodify('Trait', 'Returns').new($<typename>.ast);
    }

    method trait_mod:sym<handles>($/) {
        self.attach: $/, Nodify('Trait', 'Handles').new($<term>.ast);
    }

    ##
    ## Values
    ##

    method value:sym<quote>($/) {
        self.attach: $/, $<quote>.ast;
    }

    method value:sym<number>($/) {
        self.attach: $/, $<number>.ast;
    }

    method value:sym<version>($/) {
        self.attach: $/, $<version>.ast;
    }

    method number:sym<numish>($/) {
        self.attach: $/, $<numish>.ast;
    }

    sub ord-to-numerator($ord) {
        nqp::coerce_si(
          nqp::getuniprop_str(
            $ord, nqp::unipropcode("Numeric_Value_Numerator")
          )
        )
    }
    sub ord-to-denominator($ord) {
        nqp::coerce_si(
          nqp::getuniprop_str(
            $ord, nqp::unipropcode("Numeric_Value_Denominator")
          )
        )
    }

    method numish($/) {
        my $attachee;

        # 42
        if $<integer> -> $integer {
            $attachee := Nodify('IntLiteral').new($integer.ast);
        }

        # 42.137
        elsif $<decimal-number> -> $decimal {
            $attachee := $decimal.ast;
        }

        # :16(42)
        elsif $<radix-number> -> $radix {
            $attachee := $radix.ast;
        }

        # -22/33
        elsif $<rational-number> -> $rational {
            $attachee := $rational.ast;
        }

        # 42+i1
        elsif $<complex-number> -> $complex {
            $attachee := $complex.ast;
        }

        # Ⅼ or ⅔
        elsif $<unum> {
            my $ord := nqp::ord($/.Str);
            my int $nu := ord-to-numerator($ord);
            my int $de := ord-to-denominator($ord);

            # Ⅼ
            if !$de || $de == 1 {
                $attachee := Nodify('IntLiteral').new(
                  $*LITERALS.intern-int($nu, 10)
                );
            }

            # ⅔
            else {
                my $LITERALS := $*LITERALS;
                $attachee := Nodify('RatLiteral').new(
                  $LITERALS.intern-rat(
                    $LITERALS.intern-int($nu, 10),
                    $LITERALS.intern-int($de, 10)
                  )
                );
            }
        }

        # ∞ or other floating point value
        else {
            $attachee := Nodify('NumLiteral').new(
              $*LITERALS.intern-num($<uinf>
                ?? "Inf"  # ∞
                !! ~$/    # other floating point value
              )
            );
        }

        self.attach: $/, $attachee;
    }

    method decint($/) {
        self.attach: $/, $*LITERALS.intern-int: ~$/, 10, -> {
            $/.panic("'$/' is not a valid number")
        }
    }

    method hexint($/) {
        self.attach: $/, $*LITERALS.intern-int: ~$/, 16, -> {
            $/.panic("'$/' is not a valid number")
        }
    }

    method octint($/) {
        self.attach: $/, $*LITERALS.intern-int: ~$/, 8, -> {
            $/.panic("'$/' is not a valid number")
        }
    }

    method binint($/) {
        self.attach: $/, $*LITERALS.intern-int: ~$/, 2, -> {
            $/.panic("'$/' is not a valid number")
        }
    }

    method integer($/) { make $<VALUE>.made; }

    method signed-integer($/) {
        my $integer := $<integer>.ast;
        self.attach: $/, $<sign> eq '-' || $<sign> eq '−'
            ?? nqp::neg_I($integer, $integer.WHAT)
            !! $integer;
    }

    method decimal-number($/) {
        if $<escale> { # wants a Num
            self.attach: $/, Nodify('NumLiteral').new($*LITERALS.intern-num(~$/));
        }
        else { # wants a Rat
            self.attach: $/, Nodify('RatLiteral').new($*LITERALS.intern-decimal(
                $<int> ?? $<int>.ast !! NQPMu,
                ~$<frac>));
        }
    }

    method radix-number($/) {
        my $literals := $*LITERALS;
        if $<bracket> {
            self.attach: $/, Nodify('Term', 'RadixNumber').new:
                :radix($literals.intern-int(~$<radix>, 10)),
                :value($<bracket>.ast),
                :multi-part;
        }
        elsif $<circumfix> {
            self.attach: $/, Nodify('Term', 'RadixNumber').new:
                :radix($literals.intern-int(~$<radix>, 10)),
                :value($<circumfix>.ast);
        }
        else {
            # Check and override $radix if necessary.
            my int $radix := nqp::radix(10, $<radix>, 0, 0)[0];
            $/.typed-panic('X::Syntax::Number::RadixOutOfRange', :$radix)
                unless (2 <= $radix) && ($radix <= 36);
            if nqp::chars($<ohradix>) {
                my $ohradstr := $<ohradix>.Str;
                if $ohradstr eq "0x" {
                    $radix := 16;
                } elsif $ohradstr eq "0o" {
                    $radix := 8;
                } elsif $ohradstr eq "0d" {
                    $radix := 10;
                } elsif $ohradstr eq "0b" {
                    $radix := 2;
                } else {
                    $/.panic("Unknown radix prefix '$ohradstr'.");
                }
            }

            # Parse and assemble number.
            my $Int := $literals.int-type;
            my $Num := $literals.num-type;
            my $ipart := nqp::radix_I($radix, $<intpart>.Str, 0, 0, $Int);
            my $fpart := nqp::radix_I($radix, nqp::chars($<fracpart>) ?? $<fracpart>.Str !! ".0", 1, 4, $Int);
            my $bpart := $<base> ?? nqp::tonum_I($<base>[0].ast) !! $radix;
            my $epart := $<exp> ?? nqp::tonum_I($<exp>[0].ast) !! 0;

            if $ipart[2] < nqp::chars($<intpart>.Str) {
                $/.typed-panic: 'X::Str::Numeric',
                    :source($<intpart> ~ ($<fracpart> // '')),
                    :pos($ipart[2] < 0 ?? 0 !! $ipart[2]),
                    :reason("malformed base-$radix number");
            }
            if $fpart[2] < nqp::chars($<fracpart>.Str) {
                $/.typed-panic: 'X::Str::Numeric',
                    :source($<intpart> ~ ($<fracpart> // '')),
                    :reason("malformed base-$radix number"),
                    :pos( # the -1 dance is due to nqp::radix returning -1 for
                        # failure to parse the first char, instead of 0;
                        # we return `1` to cover the decimal dot in that case
                        $ipart[2] + ($fpart[2] == -1 ?? 1 !! $fpart[2])
                    );
            }

            my $base := nqp::pow_I(nqp::box_i($radix, $Int), $fpart[1], $Num, $Int);
            $ipart := nqp::mul_I($ipart[0], $base, $Int);
            $ipart := nqp::add_I($ipart, $fpart[0], $Int);
            $fpart := $base;

            my $scientific := nqp::pow_n($bpart, $epart);
            $ipart := nqp::mul_I($ipart, nqp::fromnum_I($scientific, $Int), $Int);

            if $fpart != 1 { # non-unit fractional part, wants Rat
                self.attach: $/, Nodify('RatLiteral').new($literals.intern-decimal($ipart, $fpart));
            }
            else { # wants Int
                self.attach: $/, Nodify('IntLiteral').new($ipart);
            }
        }
    }

    method rational-number($/) {
        self.attach: $/, $<bare-rational-number>.ast;
    }

    method bare-rational-number($/) {
        self.attach: $/, Nodify('RatLiteral').new(
          $*LITERALS.intern-decimal($<nu>.ast, $<de>.ast)
        );
    }

    method complex-number($/) {
        self.attach: $/, $<bare-complex-number>.ast;
    }

    method bare-complex-number($/) {
        self.attach: $/, Nodify('ComplexLiteral').new(
          $*LITERALS.intern-complex(~$<re>, ~$<im>)
        );
    }

    method version($/) {
        # We don't self.attach: $/, an object for the initial language version line,
        # which occurs before a setting is loaded.
        if $*R {
            my $Version := $*R.resolve-lexical-constant('Version').compile-time-value;
            self.attach: $/, Nodify('VersionLiteral').new($Version.new(~$<vstr>));
        }
    }

    method quote:sym<apos>($/)  { self.attach: $/, $<nibble>.ast; }
    method quote:sym<sapos>($/) { self.attach: $/, $<nibble>.ast; }
    method quote:sym<lapos>($/) { self.attach: $/, $<nibble>.ast; }
    method quote:sym<hapos>($/) { self.attach: $/, $<nibble>.ast; }
    method quote:sym<dblq>($/)  { self.attach: $/, $<nibble>.ast; }
    method quote:sym<sdblq>($/) { self.attach: $/, $<nibble>.ast; }
    method quote:sym<ldblq>($/) { self.attach: $/, $<nibble>.ast; }
    method quote:sym<hdblq>($/) { self.attach: $/, $<nibble>.ast; }
    method quote:sym<crnr>($/)  { self.attach: $/, $<nibble>.ast; }
    method quote:sym<qq>($/)    { self.attach: $/, $<quibble>.ast; }
    method quote:sym<q>($/)     { self.attach: $/, $<quibble>.ast; }
    method quote:sym<Q>($/)     { self.attach: $/, $<quibble>.ast; }

    method quote:sym</ />($/) {
        self.attach: $/, Nodify('QuotedRegex').new(body => $<nibble>.ast);
    }

    method quote:sym<rx>($/) {
        self.attach: $/, Nodify('QuotedRegex').new:
            body => $<quibble>.ast,
            adverbs => $<rx-adverbs>.ast;
    }

    method quote:sym<m>($/) {
        my @adverbs := $<rx-adverbs>.ast // nqp::list;
        @adverbs.push(Nodify('ColonPair','True').new("s"))
          if nqp::elems($/[0]);  # handling ms//

        self.attach: $/, Nodify('QuotedRegex').new:
          :match-immediately, body => $<quibble>.ast, :@adverbs;
    }

    method quote:sym<s>($/) {
        self.attach: $/, Nodify('Substitution').new:
            immutable => $<sym> eq 'S',
            samespace => ?$/[0],
            adverbs => $<rx-adverbs>.ast,
            pattern => $<sibble><left>.ast,
            infix => $<sibble><infixish> ?? $<sibble><infixish>.ast !! Nodify('Infixish'),
            replacement => $<sibble><right>.ast;
    }

    # We make a list of the quotepairs to attach them to the regex
    # construct; validation of what is valid takes place in the AST.
    # However, a limited number of them are required for parsing the
    # regex and constructing its AST correctly. Of note, these are
    # s (sigspace, as it controls how whitespce is parsed), m (so we
    # can construct character class ranges correctly), and P5 (Perl5,
    # so we know which regex language to parse). These get special
    # handling.
    my %RX_ADVERB_COMPILE := nqp::hash('s', 1, 'm', 1, 'P5', 1);
    my %RX_ADVERB_COMPILE_CANON := nqp::hash(
        'sigspace', 's',
        'ignoremark', 'm',
        'Perl5', 'P5',
        'ss', 's',
        'samespace', 's',
        'mm', 'm',
        'samemark', 'm');
    method rx-adverbs($/) {
        my @pairs;
        for $<quotepair> {
            my $ast := $_.ast;
            @pairs.push($ast);
            my str $key := $ast.key;
            my str $canon := %RX_ADVERB_COMPILE_CANON{$key} // $key;
            if %RX_ADVERB_COMPILE{$canon} {
                my $value := $ast.simple-compile-time-quote-value();
                if nqp::isconcrete($value) {
                    %*RX{$canon} := $value ?? 1 !! 0;
                }
                else {
                    $_.typed-panic('X::Value::Dynamic', what => "Adverb $key");
                }
            }
        }
        make @pairs;
    }

    method quotepair($/) {
        my $key := $*LITERALS.intern-str($*key);
        if $<num> {
            my $value := Nodify('IntLiteral').new($*LITERALS.intern-int(~$<num>, 10));
            self.attach: $/, Nodify('ColonPair', 'Number').new(:$key, :$value);
        }
        elsif $<circumfix> {
            my $value := $<circumfix>.ast;
            self.attach: $/, Nodify('ColonPair', 'Value').new(:$key, :$value);
        }
        elsif $<neg> {
            self.attach: $/, Nodify('ColonPair', 'False').new($key);
        }
        else {
            self.attach: $/, Nodify('ColonPair', 'True').new($key);
        }
    }

    ##
    ## Types
    ##

    method type-for-name($/, $base-name) {
        my $type := Nodify('Type', 'Simple').new($base-name.without-colonpair('_'));
        if $base-name.has-colonpair('D') {
            $type := Nodify('Type', 'Simple').new($base-name.without-colonpair('D'));
            $type := Nodify('Type', 'Definedness').new(:base-type($type), :definite);
        }
        elsif $base-name.has-colonpair('U') {
            $type := Nodify('Type', 'Simple').new($base-name.without-colonpair('U'));
            $type := Nodify('Type', 'Definedness').new(:base-type($type), :!definite);
        }
        if $<arglist> {
            $type := Nodify('Type', 'Parameterized').new(:base-type($type), :args($<arglist>.ast));
        }
        if $<accept> {
            $type := Nodify('Type', 'Coercion').new(:base-type($type), :constraint($<accept>.ast));
        }
        elsif $<accept_any> {
            $type := Nodify('Type', 'Coercion').new(:base-type($type));
        }
        $type
    }

    method typename($/) {
        my $base-name := $<longname>
            ?? $<longname>.ast
            !! Nodify('Name').from-identifier('::?' ~ $<identifier>);
        for $<colonpair> {
            $base-name.add-colonpair($_.ast);
        }
        my str $str_longname := ~$<longname>;
        if nqp::eqat($str_longname, '::', 0) {
            if $<arglist> || $<typename> {
                $/.panic("Cannot put type parameters on a type capture");
            }
            if $<accepts> || $<accepts_any> {
                $/.panic("Cannot base a coercion type on a type capture");
            }
            if $str_longname eq '::' {
                $/.panic("Cannot use :: as a type name");
            }
            my $type-capture := Nodify('Type', 'Capture').new($base-name.without-colonpairs);
            self.attach: $/, $type-capture;

            # Declare the lexical so it is available right away (e.g. for traits)
            $*R.declare-lexical($type-capture);
        }
        else {
            self.attach: $/, self.type-for-name($/, $base-name);
        }
    }

    ##
    ## Signatures
    ##

    method fakesignature($/) {
        self.attach: $/, Nodify('FakeSignature').new: $<signature>.ast
    }

    method signature($/) {
        my @parameters;
        my int $param_idx := 0;
        for $<parameter> {
            my $param := $_.ast;
            my $sep := @*seps[$param_idx];
            if ~$sep eq ':' {
                if $param_idx != 0 {
                    $/.typed-sorry('X::Syntax::Signature::InvocantMarker');
                }
                unless $*ALLOW_INVOCANT {
                    $/.typed-sorry('X::Syntax::Signature::InvocantNotAllowed');
                }
                $param.set-invocant(1);
            }
            @parameters.push($param);
            $param_idx := $param_idx + 1;
        }
        my $returns;
        if $<typename> {
            $returns := $<typename>.ast;
        }
        elsif $<value> {
            $returns := $<value>.ast;
            unless nqp::istype($returns, Nodify('CompileTimeValue')) {
                $<value>.panic('Return value after --> may only be a type or a constant');
            }
        }
        else {
            $returns := Nodify('Node');
        }
        self.attach: $/, Nodify('Signature').new(:@parameters, :$returns);
    }

    method parameter($/) {
        my $parameter := $<param-var>   ?? $<param-var>.ast   !!
                         $<named-param> ?? $<named-param>.ast !!
                         $<param-term>  ?? $<param-term>.ast  !!
                         Nodify('Parameter').new;
        my $capture := Nodify('Type', 'Capture');
        my $raku-type := Nodify('Type');
        my $raku-quotedstring := Nodify('QuotedString');
        my $raku-compiletimevalue := Nodify('CompileTimeValue');
        for $<type-constraint> {
            my $type-constraint := $_.ast;
            if nqp::istype($type-constraint, $capture) {
                $parameter.add-type-capture($type-constraint);
            }
            elsif nqp::istype($type-constraint, $raku-type) {
                $parameter.set-type($type-constraint);
            }
            elsif nqp::istype($type-constraint,$raku-quotedstring) {
                my $value := $type-constraint.literal-value;
                if nqp::defined($value) {  # not Nil
                    $parameter.set-type($type-constraint.ast-type);
                    $parameter.set-value($value);
                }
                else {
                    nqp::die("Could not get a literal value of a quoted string as a parameter");
                }
            }
            else {
                $parameter.set-type($type-constraint.ast-type);
                my $value := $type-constraint.compile-time-value;
                $parameter.set-value($value);
            }
        }
        if $<quant> {
            my str $q := ~$<quant>;
            $parameter.set-optional() if $q eq '?';
            $parameter.set-required() if $q eq '!';
            $parameter.set-slurpy(Nodify('Parameter', 'Slurpy', 'Flattened'))
                if $q eq '*';
            $parameter.set-slurpy(Nodify('Parameter', 'Slurpy', 'Unflattened'))
                if $q eq '**';
            $parameter.set-slurpy(Nodify('Parameter', 'Slurpy', 'SingleArgument'))
                if $q eq '+';
            $parameter.set-slurpy(Nodify('Parameter', 'Slurpy', 'Capture'))
                if $q eq '|';
        }
        for $<trait> {
            $parameter.add-trait($_.ast);
        }
        if $<default-value> {
            $parameter.set-default($<default-value>.ast);
        }
        if $<post-constraint> {
            if $<post-constraint>[0]<EXPR> {
                $parameter.set-where($<post-constraint>[0].ast);
            }
            elsif $<post-constraint>[0]<signature> {
                $parameter.set-sub-signature($<post-constraint>[0].ast);
            }
        }
        self.attach: $/, $parameter;
    }

    method param-var($/) {
        # Work out what kind of thing we're binding into, if any.
        my %args;
        if $<name> {
            my $dynprag := $*LANG.pragma('dynamic-scope');
            my $forced-dynamic := $dynprag
                ?? $dynprag(~$<declname>)
                !! 0;
            my $decl := Nodify('ParameterTarget', 'Var').new(~$<declname>, :$forced-dynamic);
            $/.typed-panic('X::Redeclaration', :symbol(~$<declname>))
              if $*DECLARE-TARGETS && $*R.declare-lexical($decl);
            %args<target> := $decl;
        }
        elsif $<signature> {
            %args<sub-signature> := $<signature>.ast;
        }

        # Build the parameter.
        self.attach: $/, self.set-declarand($/, Nodify('Parameter').new(|%args));
    }

    method param-term($/) {
        if $<defterm> {
            # Create sigilless target to bind into
            my $name := $<defterm>.ast;
            my $decl := Nodify('ParameterTarget', 'Term').new($name);
            $/.typed-panic('X::Redeclaration', :symbol($name.canonicalize))
              if $*DECLARE-TARGETS && $*R.declare-lexical($decl);
            self.attach: $/, Nodify('Parameter').new(target => $decl);
        }
        else {
            # Anonymous
            self.attach: $/, Nodify('Parameter').new();
        }
    }

    method named-param($/) {
        my $parameter;
        if $<name> {
            # Explicitly specified name to attach.
            if $<named-param> {
                $parameter := $<named-param>.ast;
            }
            else {
                $parameter := $<param-var>.ast;
            }
            $parameter.add-name(~$<name>);
        }
        else {
            # Name comes from the parameter variable.
            $parameter := $<param-var>.ast;
            my $name-match := $<param-var><name>;
            $parameter.add-name($name-match ?? ~$name-match !! '');
        }
        self.attach: $/, $parameter;
    }

    method default-value($/) {
        make $<EXPR>.ast;
    }

    method type-constraint($/) {
        self.attach: $/, $<typename> ?? $<typename>.ast !! $<value>.ast;
    }

    method post-constraint($/) {
        if $<EXPR> {
            make $<EXPR>.ast;
        }
        elsif $<signature> {
            make $<signature>.ast;
        }
    }

    ##
    ## Argument lists and captures
    ##

    method args($/) {
        if    $<semiarglist> { self.attach: $/, $<semiarglist>.ast; }
        elsif $<arglist>     { self.attach: $/, $<arglist>.ast; }
        else                 { self.attach: $/, Nodify('ArgList').new(); }
    }

    method semiarglist($/) {
        if nqp::elems($<arglist>) == 1 {
            self.attach: $/, $<arglist>[0].ast;
        }
        else {
            nqp::die('Multiple arg lists NYI')
        }
    }

    method arglist($/) {
        if $<EXPR> {
            my $expr := $<EXPR>.ast;
            if nqp::istype($expr, Nodify('ApplyListInfix')) &&
                    nqp::istype($expr.infix, Nodify('Infix')) &&
                    $expr.infix.operator eq ',' {
                self.attach: $/, Nodify('ArgList').from-comma-list($expr);
            }
            elsif nqp::istype($expr, Nodify('ApplyListInfix')) &&
                    nqp::istype($expr.infix, Nodify('Infix')) &&
                    $expr.infix.operator eq ':' {
                self.attach: $/, Nodify('ArgList').from-invocant-list($expr);
            }
            elsif nqp::istype($expr, Nodify('ColonPairs')) {
                self.attach: $/, Nodify('ArgList').new(|$expr.colonpairs);
            }
            else {
                self.attach: $/, Nodify('ArgList').new($expr);
            }
        }
        else {
            self.attach: $/, Nodify('ArgList').new();
        }
    }

    ##
    ## Lexer stuff
    ##

    method name($/) {
        if $<morename> {
            my @parts;
            if $<identifier> {
                @parts.push(Nodify('Name', 'Part', 'Simple').new(~$<identifier>));
            }
            for $<morename> {
                @parts.push($_.ast);
            }
            self.attach: $/, Nodify('Name').new(|@parts);
        }
        else {
            self.attach: $/, Nodify('Name').from-identifier(~$<identifier>);
        }
    }

    method morename($/) {
        if $<identifier> {
            self.attach: $/, Nodify('Name', 'Part', 'Simple').new(~$<identifier>);
        }
        elsif $<EXPR> {
            my $expr := $<EXPR>.ast;
            $expr.IMPL-CHECK($*R, $*CU.context, True);
            self.attach: $/, Nodify('Name', 'Part', 'Expression').new($expr);
        }
        else {
            self.attach: $/, Nodify('Name', 'Part', 'Empty');
        }
    }

    method longname($/) {
        my $name := $<name>.ast;
        for $<colonpair> {
            $name.add-colonpair($_.ast);
        }
        self.attach: $/, $name;
    }

    method deflongname($/) {
        # Set the name on the definition immediately, since it's known at this
        # point onwards.
        my $name := $<name>.ast;
        for $<colonpair> {
            $name.add-colonpair($_.ast);
        }
        $*BLOCK.replace-name($name);

        # Register it with the resolver.
        my $scope := $*SCOPE || $*DEFAULT-SCOPE;
        $*BLOCK.replace-scope($scope);
        if $*MULTINESS ne 'multi' {
            if $scope eq 'my' || $scope eq 'our' || $scope eq 'unit' {
                $/.typed-sorry('X::Redeclaration', :symbol($name.canonicalize))
                    if $*R.declare-lexical-in-outer($*BLOCK);
            }
            elsif $*DEFAULT-SCOPE ne 'has' {
                $/.typed-sorry('X::Redeclaration', :symbol($name.canonicalize))
                    if $*R.declare-lexical($*BLOCK);
            }
        }
    }

    method defterm($/) {
        self.attach: $/, Nodify('Name').from-identifier(~$/);
    }

    method comment:sym<line_directive>($/) {
        my $origin-source := $*ORIGIN-SOURCE;
        $origin-source.register-line-directive(
            $origin-source.original-line($/.from()),
            nqp::radix(10, $<line>, 0, 0)[0],
            $<filename> );
    }

#-------------------------------------------------------------------------------
# Declator doc handling

    method add-leading-declarator-doc($/) {
        nqp::push(@*LEADING-DOC,~$/) unless $*FROM-SEEN{$/.from}++;
    }

    method comment:sym<#|(...)>($/) {
        self.add-leading-declarator-doc($<attachment><nibble>);
    }

    method comment:sym<#|>($/) {
        self.add-leading-declarator-doc($<attachment>);
    }

    method add-trailing-declarator-doc($/) {
        my $from := $/.from;

        sub accept($/) {
            $*DECLARAND.add-trailing(~$/);
            ++$*FROM-SEEN{$from};
            nqp::deletekey($*DECLARAND-WORRIES,$from);
        }

        if $*FROM-SEEN{$from} {
            # nothing to do, all has been done already
        }
        elsif $*DECLARAND {
            my $line := +$*ORIGIN-SOURCE.original-line($from);
            # accept the trailing declarator doc if it is on the same line
            # as the declarand, or it is on the *start* of the next line
            # (-4 to get to the newline in "\n#= ")
            if $line == $*LAST-TRAILING-LINE {
                accept($/);
            }
            elsif $line == $*LAST-TRAILING-LINE + 1 {
                my $orig := $/.orig;

                # verify \n \s* #=
                my $i    := $from - 3;
                while nqp::eqat($orig,' ',--$i) { }

                if nqp::eqat($orig,"\n",$i) {
                    accept($/);
                    $*LAST-TRAILING-LINE := $line;
                }
                else {
                    $*DECLARAND-WORRIES{$from} := $/;
                }
            }
            else {
                $*DECLARAND-WORRIES{$from} := $/;
            }
        }
        else {
            $*DECLARAND-WORRIES{$from} := $/;
        }
    }

    method comment:sym<#=(...)>($/) {
        self.add-trailing-declarator-doc($<attachment><nibble>);
    }

    method comment:sym<#=>($/) {
        self.add-trailing-declarator-doc($<attachment>);
    }

#-------------------------------------------------------------------------------
# Doc blocks handling

    method doc-TOP($/) {
        my $docs := nqp::elems($*SEEN);
        if $docs > 1 {
            nqp::die("Found $docs doc blocks instead of just one");
        }
        else {
            for $*SEEN {
                $*DOC-BLOCKS-COLLECTED.push($_.value);
            }
        }
    }

    method extract-config($/) {
        my $config := nqp::hash;
        $config<numbered> := 1 if $<doc-numbered>;

        if $<colonpair> -> @colonpairs {
            for @colonpairs -> $/ {
                my $key := ~$<identifier>;
                if $<num> -> $int {
                    $config{$key} := Nodify('IntLiteral').new(+$int);
                }
                elsif $<coloncircumfix> -> $ccf {  # :bar("foo",42)
                    $config{$key} := $ccf.ast;
                }
                elsif $<var> -> $var {             # :$bar
                    $config{$key} := $var.ast;
                }
                else {                             # :!bar | :bar
                    $config{$key} :=
                      Nodify('Term', $<neg> ?? 'False' !! 'True').new;
                }
            }
        }
        $config
    }

    method extract-type($/) {
        if $<type> -> $type {
            my $level := 0;
            if $type<level> -> $levelish {
                $type := ~$type;
                nqp::substr(
                  $type,0,nqp::chars($type) - nqp::chars(~$levelish)
                )
            }
            else {
                ~$type;
            }
        }
        else {
            ""
        }
    }

    method extract-level($/) {
        if $<type> && $<type><level> -> $levelish {
            +$levelish
        }
        else {
            0
        }
    }

    method doc-block:sym<finish>($/) {
        $*CU.replace-finish-content(~$<finish>);
    }

    method doc-block:sym<alias>($/) {
        if $*FROM-SEEN{$/.from}++ {
            return;
        }

        my @paragraphs := nqp::list(~$<first>);
        if $<line> -> @lines {
            for @lines {
                nqp::push(@paragraphs,~$_);
            }
        }

        $*SEEN{$/.from} := Nodify('Doc','Block').from-alias:
          :directive, :margin(~$<margin>), :type<alias>,
          :lemma(~$<lemma>), :@paragraphs
    }

    method doc-block:sym<column-row>($/) {
        unless $*FROM-SEEN{$/.from}++ {
            $*SEEN{$/.from} := Nodify('Doc','Block').new:
              :directive, :margin(~$<margin>), :type(~$<type>),
              :config(self.extract-config($/))
        }
    }

    method doc-block:sym<config>($/) {
        unless $*FROM-SEEN{$/.from}++ {
            $*SEEN{$/.from} := Nodify('Doc','Block').from-config:
              :directive, :margin(~$<margin>), :type<config>,
              :config(self.extract-config($/)), :key(~$<doc-identifier>)
        }
    }

    method doc-block:sym<verbatim>($/) {
        if $*FROM-SEEN{$/.from}++ {
            return;
        }

        my $config := self.extract-config($/);

        my @paragraphs;
        if $<lines> -> $lines {
            if ~$lines -> $text {
                nqp::push(@paragraphs,$text);
            }
        }

        $*SEEN{$/.from} := Nodify('Doc','Block').from-paragraphs:
          :margin(~$<margin>), :type(~$<type>), :$config, :@paragraphs;
    }

    method doc-block:sym<begin>($/) {
        if $*FROM-SEEN{$/.from}++ {
            return;
        }
        my $SEEN := $*SEEN;

        my $config := self.extract-config($/);
        my $type   := self.extract-type($/);
        my $level  := self.extract-level($/);

        my @paragraphs;
        if $<doc-content> -> $doc-content {
            for $doc-content {
                my $from := ~$_.from;
                if nqp::existskey($SEEN,$from) {
                    nqp::push(@paragraphs,nqp::atkey($SEEN,$from));
                    nqp::deletekey($SEEN,$from);
                }
                elsif ~$_ -> $text {
                    nqp::push(@paragraphs,$text);
                }
            }
        }

        $SEEN{$/.from} := Nodify('Doc','Block').from-paragraphs:
          :margin(~$<margin>), :$type, :$level, :$config, :@paragraphs;
    }

    method doc-block:sym<for>($/) {
        if $*FROM-SEEN{$/.from}++ {
            return;
        }

        my $config := self.extract-config($/);
        my $type   := self.extract-type($/);
        my $level  := self.extract-level($/);

        my @paragraphs;
        if $<lines> -> $lines {
            if ~$lines -> $text {
                nqp::push(@paragraphs,$text);
            }
        }
        $*SEEN{$/.from} := Nodify('Doc','Block').from-paragraphs:
          :margin(~$<margin>), :for, :$type, :$level, :$config, :@paragraphs;
    }

    method doc-block:sym<abbreviated>($/) {
        if $*FROM-SEEN{$/.from}++ {
            return;
        }

        my $config := self.extract-config($/);
        my $type   := self.extract-type($/);
        my $level  := self.extract-level($/);

        my @paragraphs;
        if ($<header> ?? $<margin> ~ $<header> !! "")
            ~ ($<lines> ?? ~$<lines> !! "") -> $text {
            @paragraphs := nqp::list($text);
        }

        $*SEEN{$/.from} := Nodify('Doc','Block').from-paragraphs:
          :margin(~$<margin>), :abbreviated, :$type, :$level, :$config,
          :@paragraphs;
    }
}

class Raku::QActions is HLL::Actions does Raku::CommonActions {
    # This overrides NQP during the deprecation period for Unicode 1 names not covered by Alias Names
    method charname-panic($/) { $/.panic("Unrecognized character name [$/]") }

    method charname($/) {
        my $codepoint := $<integer>
                         ?? nqp::chr($<integer>.made)
                         !! nqp::strfromname(~$/);
        $codepoint := self.charname-notfound($/) if $codepoint eq '';
        self.attach: $/, $codepoint;
    }

    method charname-notfound($/) {
        my @worry-text := ( "LINE FEED, NEW LINE, END OF LINE, LF, NL or EOL",
                            "FORM FEED or FF",
                            "CARRIAGE RETURN or CR",
                            "NEXT LINE or NEL" );
        my $text := "Deprecated character name [%s] in lookup of Unicode character by name.\n" ~
                    "Unicode 1 names are deprecated.\nPlease use %s";
        if ~$/ eq "LINE FEED (LF)" {
            $/.worry(nqp::sprintf($text, (~$/, @worry-text[0]) ) );
            return nqp::strfromname("LINE FEED");
        }
        if ~$/ eq "FORM FEED (FF)" {
            $/.worry(nqp::sprintf($text, (~$/, @worry-text[1]) ) );
            return nqp::strfromname("FORM FEED");
        }
        if ~$/ eq "CARRIAGE RETURN (CR)" {
            $/.worry(nqp::sprintf($text, (~$/, @worry-text[2]) ) );
            return nqp::strfromname("CARRIAGE RETURN");
        }
        if ~$/ eq "NEXT LINE (NEL)" {
            $/.worry(nqp::sprintf($text, (~$/, @worry-text[3]) ) );
            return nqp::strfromname("NEXT LINE");
        }
        self.charname-panic($/);
    }

    method nibbler($/) {
        my @segments;
        my $lastlit := '';
        my $StrLiteral := Nodify('StrLiteral');

        for @*nibbles {
            if nqp::istype($_, NQPMatch) {
                my $ast := $_.ast;
                if nqp::isstr($ast) {
                    $lastlit := $lastlit ~ $ast;
                }
                else {
                    if $lastlit ne '' {
                        @segments.push: $StrLiteral.new($*LITERALS.intern-str($lastlit));
                        $lastlit := '';
                    }
                    @segments.push($ast);
                }
            }
            else {
                $lastlit := $lastlit ~ $_;
            }
        }

        if $lastlit ne '' || !@segments {
            @segments.push: $StrLiteral.new($*LITERALS.intern-str($lastlit));
        }

        my @processors := nqp::can($/, 'postprocessors') ?? $/.postprocessors !! [];
        my $node-class := nqp::can($/, 'herelang') ?? 'Heredoc' !! 'QuotedString';
        self.attach: $/, Nodify($node-class).new(:@segments, :@processors);
    }

    method escape:sym<\\>($/) { self.attach: $/, $<item>.ast; }
    method backslash:sym<qq>($/) { self.attach: $/, $<quote>.ast; }
    method backslash:sym<\\>($/) { self.attach: $/, $<text>.Str; }
    method backslash:delim ($/) { self.attach: $/, $<text>.Str; }
    method backslash:sym<miscq>($/) { make '\\' ~ ~$/; }
    method backslash:sym<misc>($/) { make ~$/; }

    method backslash:sym<a>($/) { make nqp::chr(7) }
    method backslash:sym<b>($/) { make "\b" }
    method backslash:sym<c>($/) { make $<charspec>.ast }
    method backslash:sym<e>($/) { make "\c[27]" }
    method backslash:sym<f>($/) { make "\c[12]" }
    method backslash:sym<n>($/) {
        my str $nl := $*R.resolve-lexical-constant('$?NL').compile-time-value;
        if nqp::can($/, 'parsing-heredoc') {
            # In heredocs, we spit out a QAST::SVal here to prevent newlines
            # being taken literally and affecting the dedent.
            make Nodify('Heredoc', 'InterpolatedWhiteSpace').new($*LITERALS.intern-str($nl));
        }
        else {
            make $nl;
        }
    }
    method backslash:sym<o>($/) { make self.ints_to_string( $<octint> ?? $<octint> !! $<octints><octint> ) }
    method backslash:sym<r>($/) {
        if nqp::can($/, 'parsing-heredoc') {
            make Nodify('Heredoc', 'InterpolatedWhiteSpace').new($*LITERALS.intern-str("\r"));
        }
        else {
            make "\r";
        }
    }
    method backslash:sym<rn>($/) {
        if nqp::can($/, 'parsing-heredoc') {
            make Nodify('Heredoc', 'InterpolatedWhiteSpace').new($*LITERALS.intern-str("\r\n"));
        }
        else {
            make "\r\n";
        }
    }
    method backslash:sym<t>($/) {
        if nqp::can($/, 'parsing-heredoc') {
            make Nodify('Heredoc', 'InterpolatedWhiteSpace').new($*LITERALS.intern-str("\t"));
        }
        else {
            make "\t";
        }
    }
    method backslash:sym<x>($/) { make self.ints_to_string( $<hexint> ?? $<hexint> !! $<hexints><hexint> ) }
    method backslash:sym<0>($/) { make "\c[0]" }

    method escape:sym<$>($/) { self.attach: $/, $<EXPR>.ast; }
    method escape:sym<@>($/) { self.attach: $/, $<EXPR>.ast; }
    method escape:sym<%>($/) { self.attach: $/, $<EXPR>.ast; }
    method escape:sym<&>($/) { self.attach: $/, $<EXPR>.ast; }

    method escape:sym<{ }>($/) {
        self.attach: $/, $<block>.ast;
    }

    method escape:sym<'>($/) { self.attach: $/, self.qwatom($<quote>.ast); }
    method escape:sym<colonpair>($/) { self.attach: $/, self.qwatom($<colonpair>.ast); }
    method escape:sym<#>($/) { make ''; }
    method qwatom($ast) { Nodify('QuoteWordsAtom').new($ast) }
}

class Raku::RegexActions is HLL::Actions does Raku::CommonActions {

    method nibbler($/) {
        self.attach: $/, $<termseq>.ast;
    }

    method termseq($/) {
        self.attach: $/, $<termaltseq>.ast;
    }

    method termaltseq($/) {
        if nqp::elems($<termconjseq>) == 1 {
            self.attach: $/, $<termconjseq>[0].ast;
        }
        else {
            my @branches;
            for $<termconjseq> {
                @branches.push($_.ast);
            }
            self.attach: $/, Nodify('Regex', 'SequentialAlternation').new(|@branches);
        }
    }

    method termconjseq($/) {
        if nqp::elems($<termalt>) == 1 {
            self.attach: $/, $<termalt>[0].ast;
        }
        else {
            my @branches;
            for $<termalt> {
                @branches.push($_.ast);
            }
            self.attach: $/, Nodify('Regex', 'SequentialConjunction').new(|@branches);
        }
    }

    method termalt($/) {
        if nqp::elems($<termconj>) == 1 {
            self.attach: $/, $<termconj>[0].ast;
        }
        else {
            my @branches;
            for $<termconj> {
                @branches.push($_.ast);
            }
            self.attach: $/, Nodify('Regex', 'Alternation').new(|@branches);
        }
    }

    method termconj($/) {
        if nqp::elems($<termish>) == 1 {
            self.attach: $/, $<termish>[0].ast;
        }
        else {
            my @branches;
            for $<termish> {
                @branches.push($_.ast);
            }
            self.attach: $/, Nodify('Regex', 'Conjunction').new(|@branches);
        }
    }

    method termish($/) {
        if nqp::elems($<noun>) == 1 {
            self.attach: $/, $<noun>[0].ast;
        }
        else {
            my @terms;
            for $<noun> {
                @terms.push($_.ast);
            }
            self.attach: $/, Nodify('Regex', 'Sequence').new(|@terms);
        }
    }

    method quantified_atom($/) {
        my $atom := self.wrap-whitespace($<sigmaybe>, $<atom>.ast);

        if $<quantifier> {
            my $quantifier := $<quantifier>.ast;
            if $<separator> {
                my $separator := $<separator>.ast;
                my $trailing-separator := $<separator><septype> eq '%%';
                self.attach: $/, self.wrap-whitespace: $<sigfinal>,
                    Nodify('Regex', 'QuantifiedAtom').new(:$atom, :$quantifier,
                        :$separator, :$trailing-separator);
            }
            else {
                self.attach: $/, self.wrap-whitespace: $<sigfinal>,
                    Nodify('Regex', 'QuantifiedAtom').new(:$atom, :$quantifier);
            }
        }
        # no quantifier
        elsif $<separator> {
            $/.panic("'" ~ $<separator><septype> ~
                "' may only be used immediately following a quantifier");
        }
        elsif $<backmod> {
            self.attach: $/, self.wrap-whitespace: $<sigfinal>,
                Nodify('Regex', 'BacktrackModifiedAtom').new:
                    :$atom, :backtrack($<backmod>.ast);
        }
        else {
            self.attach: $/, self.wrap-whitespace($<sigfinal>, $atom);
        }
    }

    method wrap-whitespace($cond, $ast) {
        nqp::chars(~$cond)
          && nqp::istype($ast,Nodify('Regex','Term'))
          && $ast.whitespace-wrappable
          ?? Nodify('Regex', 'WithWhitespace').new($ast)
          !! $ast
    }

    method atom($/) {
        if $<metachar> {
            self.attach: $/, $<metachar>.ast;
        }
        else {
            self.attach: $/, Nodify('Regex', 'Literal').new(~$/);
        }
    }

    method quantifier:sym<*>($/) {
        self.attach: $/, Nodify('Regex', 'Quantifier', 'ZeroOrMore').new(backtrack => $<backmod>.ast);
    }

    method quantifier:sym<+>($/) {
        self.attach: $/, Nodify('Regex', 'Quantifier', 'OneOrMore').new(backtrack => $<backmod>.ast);
    }

    method quantifier:sym<?>($/) {
        self.attach: $/, Nodify('Regex', 'Quantifier', 'ZeroOrOne').new(backtrack => $<backmod>.ast);
    }

    method quantifier:sym<**>($/) {
        if $<codeblock> {
            self.attach: $/, Nodify('Regex', 'Quantifier', 'BlockRange').new:
                :block($<codeblock>.ast), :backtrack($<backmod>.ast);
        }
        else {
            my $LITERALS := $*LITERALS;
            my $min := $<min>
                ?? $LITERALS.build-int(~$<min>, 10)
                !! $LITERALS.int-type;
            my $max;
            if !$<max> {
                $max := $min;
            }
            elsif $<max> eq '*' {
                $max := $LITERALS.int-type;
            }
            else {
                $max := $LITERALS.build-int(~$<max>, 10);
            }
            my int $excludes-min := $<from> eq '^';
            my int $excludes-max := $<upto> eq '^';
            self.attach: $/, Nodify('Regex', 'Quantifier', 'Range').new:
                :$min, :$max, :$excludes-min, :$excludes-max,
                :backtrack($<backmod>.ast);
        }
    }

    method backmod($/) {
        my str $backmod := ~$/;
        if $backmod eq ':' {
            self.attach: $/, Nodify('Regex', 'Backtrack', 'Ratchet')
        }
        elsif $backmod eq ':?' || $backmod eq '?' {
            self.attach: $/, Nodify('Regex', 'Backtrack', 'Frugal')
        }
        elsif $backmod eq ':!' || $backmod eq '!' {
            self.attach: $/, Nodify('Regex', 'Backtrack', 'Greedy')
        }
        else {
            self.attach: $/, Nodify('Regex', 'Backtrack')
        }
    }

    method separator($/) {
        self.attach: $/, $<quantified_atom>.ast;
    }

    method metachar:sym<[ ]>($/) {
        self.attach: $/, Nodify('Regex', 'Group').new($<nibbler>.ast);
    }

    method metachar:sym<( )>($/) {
        self.attach: $/, Nodify('Regex', 'CapturingGroup').new($<nibbler>.ast);
    }

    method metachar:sym<.>($/) {
        self.attach: $/, Nodify('Regex', 'CharClass', 'Any').new;
    }

    method metachar:sym<^>($/) {
        self.attach: $/, Nodify('Regex', 'Anchor', 'BeginningOfString').new;
    }

    method metachar:sym<^^>($/) {
        self.attach: $/, Nodify('Regex', 'Anchor', 'BeginningOfLine').new;
    }

    method metachar:sym<$>($/) {
        self.attach: $/, Nodify('Regex', 'Anchor', 'EndOfString').new;
    }

    method metachar:sym<$$>($/) {
        self.attach: $/, Nodify('Regex', 'Anchor', 'EndOfLine').new;
    }

    method metachar:sym<lwb>($/) {
        self.attach: $/, Nodify('Regex', 'Anchor', 'LeftWordBoundary').new;
    }

    method metachar:sym<rwb>($/) {
        self.attach: $/, Nodify('Regex', 'Anchor', 'RightWordBoundary').new;
    }

    method metachar:sym<from>($/) {
        self.attach: $/, Nodify('Regex', 'MatchFrom').new;
    }

    method metachar:sym<to>($/) {
        self.attach: $/, Nodify('Regex', 'MatchTo').new;
    }

    method metachar:sym<bs>($/) {
        self.attach: $/, $<backslash>.ast;
    }

    method metachar:sym<mod>($/) {
        self.attach: $/, $<mod_internal>.ast;
    }

    method metachar:sym<assert>($/) {
        self.attach: $/, $<assertion>.ast;
    }

    method metachar:sym<:my>($/) {
        self.attach: $/, Nodify('Regex', 'Statement').new($<statement>.ast);
    }

    method metachar:sym<{ }>($/) {
        self.attach: $/, Nodify('Regex', 'Block').new($<codeblock>.ast);
    }

    method metachar:sym<var>($/) {
        if $<quantified_atom> {
            self.attach: $/, Nodify('Regex', 'NamedCapture').new:
                name => ~($<name> || $<pos>),
                array => $<wantarray> ?? 1 !! 0,
                regex => $<quantified_atom>[0].ast;
        }
        elsif $<pos> {
            self.attach: $/, Nodify('Regex', 'BackReference', 'Positional').new: +$<pos>;
        }
        else {
            self.attach: $/, Nodify('Regex', 'BackReference', 'Named').new: ~$<name>;
        }
    }

    method metachar:sym<rakvar>($/) {
        if $<var><sigil> eq '%' {
            $<var>.typed-panic('X::Syntax::Reserved', :reserved('use of hash variables in regexes'))
        }
        my $sequential := $*SEQ ?? 1 !! 0;
        self.attach: $/, Nodify('Regex', 'Interpolation').new(:var($<var>.ast), :$sequential);
    }

    method metachar:sym<qw>($/) {
        self.attach: $/, Nodify('Regex', 'Quote').new($<nibble>.ast);
    }

    method metachar:sym<'>($/) {
        self.attach: $/, Nodify('Regex', 'Quote').new($<quote>.ast);
    }

    method backslash:sym<e>($/) {
        self.attach: $/, Nodify('Regex', 'CharClass', 'Escape').new(negated => $<sym> le 'Z');
    }

    method backslash:sym<f>($/) {
        self.attach: $/, Nodify('Regex', 'CharClass', 'FormFeed').new(negated => $<sym> le 'Z');
    }

    method backslash:sym<h>($/) {
        self.attach: $/, Nodify('Regex', 'CharClass', 'HorizontalSpace').new(negated => $<sym> le 'Z');
    }

    method backslash:sym<r>($/) {
        self.attach: $/, Nodify('Regex', 'CharClass', 'CarriageReturn').new(negated => $<sym> le 'Z');
    }

    method backslash:sym<s>($/) {
        my constant NAME := nqp::hash('d', 'Digit', 'n', 'Newline', 's', 'Space', 'w', 'Word');
        self.attach: $/, Nodify('Regex', 'CharClass', NAME{nqp::lc(~$<sym>)}).new(negated => $<sym> le 'Z');
    }

    method backslash:sym<t>($/) {
        self.attach: $/, Nodify('Regex', 'CharClass', 'Tab').new(negated => $<sym> le 'Z');
    }

    method backslash:sym<v>($/) {
        self.attach: $/, Nodify('Regex', 'CharClass', 'VerticalSpace').new(negated => $<sym> le 'Z');
    }

    method backslash:sym<0>($/) {
        self.attach: $/, Nodify('Regex', 'CharClass', 'Nul').new();
    }

    method backslash:sym<o>($/) {
        my str $characters := HLL::Actions.ints_to_string($<octint> || $<octints><octint>);
        self.attach: $/, Nodify('Regex', 'CharClass', 'Specified').new:
            :negated($<sym> le 'Z'), :$characters
    }

    method backslash:sym<x>($/) {
        my str $characters := HLL::Actions.ints_to_string($<hexint> || $<hexints><hexint>);
        self.attach: $/, Nodify('Regex', 'CharClass', 'Specified').new:
            :negated($<sym> le 'Z'), :$characters
    }

    method backslash:sym<c>($/) {
        self.attach: $/, Nodify('Regex', 'CharClass', 'Specified').new:
            :negated($<sym> le 'Z'), :characters($<charspec>.ast)
    }

    method backslash:sym<misc>($/) {
        self.attach: $/, Nodify('Regex', 'Literal').new(~$/);
    }

    method assertion:sym<?>($/) {
        if $<assertion> {
            my $assertion := $<assertion>.ast;
            self.attach: $/, Nodify('Regex', 'Assertion', 'Lookahead').new(:$assertion);
        }
        else {
            self.attach: $/, Nodify('Regex', 'Assertion', 'Pass').new();
        }
    }

    method assertion:sym<!>($/) {
        if $<assertion> {
            my $assertion := $<assertion>.ast;
            self.attach: $/, Nodify('Regex', 'Assertion', 'Lookahead').new(:$assertion, :negated);
        }
        else {
            self.attach: $/, Nodify('Regex', 'Assertion', 'Fail').new();
        }
    }

    method assertion:sym<method>($/) {
        my $ast := $<assertion>.ast;
        if nqp::can($ast, 'set-capturing') {
            $ast.set-capturing(0);
        }
        self.attach: $/, $ast;
    }

    method assertion:sym<name>($/) {
        my $name := ~$<longname>;
        my $qast;
        if $<assertion> {
            my str $name := ~$<longname>;
            my $assertion := $<assertion>.ast;
            self.attach: $/, Nodify('Regex', 'Assertion', 'Alias').new(:$name, :$assertion);
        }
        else {
            my $name := $<longname>.ast;
            if $<arglist> {
                my $args := $<arglist>.ast;
                self.attach: $/, Nodify('Regex', 'Assertion', 'Named', 'Args').new(
                    :$name, :capturing, :$args);
            }
            elsif $<nibbler> {
                my $regex-arg := $<nibbler>.ast;
                self.attach: $/, Nodify('Regex', 'Assertion', 'Named', 'RegexArg').new(
                    :$name, :capturing, :$regex-arg);
            }
            else {
                self.attach: $/, Nodify('Regex', 'Assertion', 'Named').new(:$name, :capturing);
            }
        }
    }

    method assertion:sym<{ }>($/) {
        my $sequential := $*SEQ ?? 1 !! 0;
        self.attach: $/, Nodify('Regex', 'Assertion', 'InterpolatedBlock').new:
            :block($<codeblock>.ast), :$sequential;
    }

    method assertion:sym<?{ }>($/) {
        self.attach: $/, Nodify('Regex', 'Assertion', 'PredicateBlock').new:
            :block($<codeblock>.ast);
    }

    method assertion:sym<!{ }>($/) {
        self.attach: $/, Nodify('Regex', 'Assertion', 'PredicateBlock').new:
            :negated, :block($<codeblock>.ast);
    }

    method assertion:sym<var>($/) {
        if $<call> {
            my $node := Nodify('Regex', 'Assertion', 'Callable');
            self.attach: $/, $<arglist>
                ?? $node.new(:callee($<call>.ast), :args($<arglist>.ast))
                !! $node.new(:callee($<call>.ast));
        }
        else {
            my $sequential := $*SEQ ?? 1 !! 0;
            self.attach: $/, Nodify('Regex', 'Assertion', 'InterpolatedVar').new:
                :var($<var>.ast), :$sequential;
        }
    }

    method assertion:sym<[>($/) {
        my @elems := $<cclass_elem>;
        my @asts;
        my int $i := 0;
        my int $n := nqp::elems(@elems);
        while $i < $n {
            my $sign := @elems[$i]<sign>;
            if $i > 0 && $sign eq '' {
                $sign."!clear_highwater"();
                $sign.panic('Missing + or - between character class elements')
            }
            @asts.push(@elems[$i].ast);
            $i++;
        }
        self.attach: $/, Nodify('Regex', 'Assertion', 'CharClass').new(|@asts);
    }

    method assertion:sym<~~>($/) {
        if $<num> {
            $/.panic('Sorry, ~~ regex assertion with a capture is not yet implemented');
        }
        elsif $<desigilname> {
            $/.panic('Sorry, ~~ regex assertion with a capture is not yet implemented');
        }
        else {
            self.attach: $/, Nodify('Regex', 'Assertion', 'Recurse').new($/);
        }
    }

    method cclass_elem($/) {
        my int $negated := $<sign> eq '-';
        if $<name> {
            self.attach: $/, Nodify('Regex', 'CharClassElement', 'Rule').new:
                :name(~$<name>), :$negated;
        }
        elsif $<identifier> {
            my str $property := ~$<identifier>;
            my int $inverted := $<invert> eq '!';
            my $predicate := $<coloncircumfix>
                ?? $<coloncircumfix>.ast
                !! Nodify('Expression');
            self.attach: $/, Nodify('Regex', 'CharClassElement', 'Property').new:
                :$property, :$inverted, :$predicate, :$negated;
        }
        else {
            my @elements;
            for $<charspec> {
                if $_[1] {
                    my int $from := extract_endpoint($_[0]);
                    my int $to := extract_endpoint($_[1][0]);
                    my $node := Nodify('Regex', 'CharClassEnumerationElement', 'Range');
                    @elements.push($node.new(:$from, :$to));
                }
                elsif $_[0]<cclass_backslash> {
                    @elements.push($_[0]<cclass_backslash>.ast);
                }
                else {
                    my $node := Nodify('Regex', 'CharClassEnumerationElement', 'Character');
                    @elements.push($node.new(~$_[0]));
                }
            }
            self.attach: $/, Nodify('Regex', 'CharClassElement', 'Enumeration').new:
                :@elements, :$negated;
        }
    }

    sub extract_endpoint($/) {
        my str $chr;
        if $<cclass_backslash> {
            my $ast := $<cclass_backslash>.ast;
            if $ast.range-endpoint -> $ok {
                $chr := $ok;
            }
            else {
                $/.panic("Illegal range endpoint: " ~ ~$/)
            }
        }
        else {
            $chr := ~$/;
        }
        %*RX<m>
            ?? nqp::ordbaseat($chr, 0)
            !! non_synthetic_ord($/, $chr)
    }

    sub non_synthetic_ord($/, $chr) {
        my int $ord := nqp::ord($chr);
        if nqp::chr($ord) ne $chr {
            $/.panic("Cannot use $chr as a range endpoint, as it is not a single codepoint");
        }
        $ord
    }

    method cclass_backslash:sym<s>($/) {
        self.backslash:sym<s>($/)
    }

    method cclass_backslash:sym<b>($/) {
        self.attach: $/, Nodify('Regex', 'CharClass', 'BackSpace').new(negated => $<sym> le 'Z');
    }

    method cclass_backslash:sym<e>($/) {
        self.backslash:sym<e>($/)
    }

    method cclass_backslash:sym<f>($/) {
        self.backslash:sym<f>($/)
    }

    method cclass_backslash:sym<h>($/) {
        self.backslash:sym<h>($/)
    }

    method cclass_backslash:sym<r>($/) {
        self.backslash:sym<r>($/)
    }

    method cclass_backslash:sym<t>($/) {
        self.backslash:sym<t>($/)
    }

    method cclass_backslash:sym<v>($/) {
        self.backslash:sym<v>($/)
    }

    method cclass_backslash:sym<o>($/) {
        self.backslash:sym<o>($/)
    }

    method cclass_backslash:sym<x>($/) {
        self.backslash:sym<x>($/)
    }

    method cclass_backslash:sym<c>($/) {
        self.backslash:sym<c>($/)
    }

    method cclass_backslash:sym<0>($/) {
        self.backslash:sym<0>($/)
    }

    method cclass_backslash:sym<any>($/) {
        self.attach: $/,
            Nodify('Regex', 'CharClassEnumerationElement', 'Character').new(~$/);
    }

    method mod_internal($/) {
        my constant NODE := nqp::hash('i', 'IgnoreCase', 'm', 'IgnoreMark',
            'r', 'Ratchet', 's', 'Sigspace');
        if NODE{$<mod_ident><sym>} -> $node-name {
            my str $n := $<n> ?? ~$<n>[0] !! '';
            my $negated := $n eq ''  ?? 0 !!
                           $n eq '!' ?? 1 !!
                           +$n == 0  ?? 1 !! 0;
            self.attach: $/, Nodify('Regex', 'InternalModifier', $node-name).new(:$negated);
        }
        else {
            nqp::die('Unimplemented internal modifier ' ~ $<sym>);
        }
    }

    method codeblock($/) {
        make $<block>.ast;
    }

    method arglist($/) {
        make $<arglist>.ast;
    }
}

class Raku::P5RegexActions is HLL::Actions does Raku::CommonActions {
    method p5metachar:sym<(?{ })>($/) {
        self.attach: $/, Nodify('Regex', 'Block').new($<codeblock>.ast);
    }

    method p5metachar:sym<(??{ })>($/) {
        my $sequential := $*SEQ ?? 1 !! 0;
        self.attach: $/, Nodify('Regex', 'Assertion', 'InterpolatedBlock').new:
            :block($<codeblock>.ast), :$sequential;
    }

    method p5metachar:sym<var>($/) {
        my $sequential := $*SEQ ?? 1 !! 0;
        self.attach: $/, Nodify('Regex', 'Interpolation').new(:var($<var>.ast), :$sequential);
    }

    method store_regex_nfa($code_obj, $block, $nfa) {
        $code_obj.SET_NFA($nfa.save);
    }

    method codeblock($/) {
        make $<block>.ast;
    }

    method arglist($/) {
        make $<arglist>.ast;
    }
}
