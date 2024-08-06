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
    my $idx := 0;
    my $todo-elems := nqp::elems(@todo);
    my $res := nqp::atkey($RakuAST-WHO,nqp::atpos(@todo, $idx++));
    while $idx < $todo-elems && !nqp::isnull($res) {
        $res := nqp::atkey($res.WHO,nqp::atpos(@todo, $idx++));
    }
    nqp::ifnull(
      $res,
      nqp::stmts(
        nqp::die('No such node RakuAST::' ~ nqp::join('::',@todo))
      )
    )
}

# Used by '-p' to print $_ on each iteration
sub print-topic() {
    Nodify('Statement', 'Expression').new(expression =>
        Nodify('Call', 'Name').new(
            name => Nodify('Name').from-identifier('say'),
            args => Nodify('ArgList').new(Nodify('Var', 'Lexical').new('$_'))))
}

# Provide the functionality of '-n' and '-p'
sub wrap-in-for-loop($ast) {
    Nodify('StatementList').new(
        Nodify('Statement', 'For').new(
            source => Nodify('Call', 'Name').new(name => Nodify('Name').from-identifier('lines')),
            body   => Nodify('PointyBlock').new(
                signature => Nodify('Signature').new(
                    parameters => (Nodify('Parameter').new(
                        target => Nodify('ParameterTarget', 'Var').new(name => '$_'),
                        traits => Nodify('Trait', 'Is').new(
                            name => Nodify('Name').from-identifier('copy'))))),
                body => Nodify('Blockoid').new($ast))));
}

#-------------------------------------------------------------------------------
# Role for all Action classes associated with Raku grammar slangs

role Raku::CommonActions {
    # Some AST nodes need symbol resolution or attachment of position
    # information as we go. This factors out that process and attaches
    # the AST to the match object.
    method attach($/, $node, :$as-key-origin) {
        my $cu := $*CU; # Might be too early to even have a CompUnit
        self.SET-NODE-ORIGIN($/, $node, :$as-key-origin);
        $node.to-begin-time($*R, $cu ?? $cu.context !! NQPMu);
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
        self.attach: $/, $<nibble>.ast // Nodify('Node');
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

        # Set up the literals builder, so we can produce and intern literal
        # values.
        $*LITERALS := Nodify('LiteralBuilder').new(:resolver($RESOLVER));

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
                my $modifier := @vparts > 1
                  && nqp::objprimspec(@vparts[-1]) == nqp::const::BIND_VAL_STR
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
            $HLL-COMPILER.set_language_revision: $language-revision;
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
              :$language-revision,
              :setting($*R.setting),
              :resolver($RESOLVER),
            );
        }

        # Top-level compilation.
        else {
            $*CU := Nodify('CompUnit').new(
              :$comp-unit-name,
              :$setting-name,
              :setting($*R.setting),
              :global-package-how($package-how),
              :precompilation-mode(%OPTIONS<precomp>),
              :$export-package,
              :$language-revision,
              :resolver($RESOLVER),
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

        # Do any requested wrapping (-n or -p)
        my $statement-list := $<statementlist>.ast;
        if (my $add-print-topic := nqp::existskey(%OPTIONS,'p')) || nqp::existskey(%OPTIONS,'n') {
            $statement-list.add-statement(print-topic()) if $add-print-topic;
            $statement-list := wrap-in-for-loop($statement-list);
            # Give the wrapper nodes a chance to do BEGIN time effects
            $statement-list.IMPL-BEGIN($RESOLVER, $COMPUNIT.context);
        }
        $RESOLVER.enter-scope($COMPUNIT);

        # Put the body in place.
        $COMPUNIT.replace-statement-list($statement-list);

        $COMPUNIT.to-begin-time($RESOLVER, $COMPUNIT.context);

        # Sort out sinking; the compilation unit is sunk as a whole if we are
        # not in a REPL or EVAL context.
        $COMPUNIT.mark-sunk() unless nqp::existskey(%OPTIONS,'outer_ctx');
        $COMPUNIT.calculate-sink();

        # if --(raku)doc specified, add INIT phaser that handles that
        if nqp::existskey(%OPTIONS,'doc') {
            $COMPUNIT.add-INIT-phaser-for-doc-handling(
              'Pod', %OPTIONS<doc> || 'Text'
            ).IMPL-BEGIN($RESOLVER, $*CU.context);
        }
        elsif nqp::existskey(%OPTIONS,'rakudoc') {
            $COMPUNIT.add-INIT-phaser-for-doc-handling(
              'RakuDoc', %OPTIONS<rakudoc> || 'Text'
            ).IMPL-BEGIN($RESOLVER, $*CU.context);
        }

        self.attach: $/, $COMPUNIT, :as-key-origin;

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

        $RESOLVER.leave-scope();
    }

    # Action method to load any modules specified with -M
    method load-M-modules($/) {
        my $M := %*OPTIONS<M>;
        return Nil unless nqp::defined($M); # nothing to do here

        # shortcuts
        my $R       := $*R;
        my $context := $*CU.context;

        # Create a RakuAST statement list with -use- statements
        # of the specified module names and attach that
        my $ast := Nodify('StatementList').new;
        for nqp::islist($M) ?? $M !! [$M] -> $longname {
            my $use := Nodify('Statement', 'Use').new(
              module-name => Nodify('Name').from-identifier-parts(
                |nqp::split('::', $longname)
              )
            );
            $use.ensure-begin-performed($R, $context);
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
            my $ast := $<statement>.ast;
            $ast.add-label($<label>.ast);
            make $ast;
            return;       # nothing left to do here
        }

        # Statement ID must be captured before creation of statement object
        my $statement-id := $*STATEMENT-ID;
        my $statement;

        # Handle expression with optional condition/loop modifiers
        if $<EXPR> {
            my $ast := $<EXPR>.ast;
            my $context := $*CU ?? $*CU.context !! NQPMu; # Might be too early to even have a CU
            if nqp::istype($ast, Nodify('ColonPairs')) {
                $ast := Nodify('ApplyListInfix').new:
                  :infix(Nodify('Infix').new(',').to-begin-time($*R, $context)),
                  :operands($ast.colonpairs);
            }
            $statement := Nodify('Statement','Expression').new(:expression($ast.to-begin-time($*R, $context)));
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
        $statement.set-trace(1) if $/.pragma('trace');
        $statement.set-statement-id($statement-id);
        $statement.attach-doc-blocks unless $*PARSING-DOC-BLOCK;

        self.attach: $/, $statement;
    }

    # Action method for handling labels attached to a statement
    method label($/) {
        my $name := ~$<identifier>;
        my $decl := Nodify('Label').new($name);
        $*R.declare-lexical($decl)
          ?? $/.typed-panic('X::Redeclaration', :symbol($name))
          !! self.attach: $/, $decl;
    }

    # Helper method for attaching (pointy) blocks
    method attach-block($/) {
        my $block := $*BLOCK;
        $block.replace-body($<blockoid>.ast);
        if $*IN-LOOP {
            $block.set-is-loop-body;
        }
        self.attach: $/, $block;
    }

    # Action methods for handling (pointy) blocks
    method pointy-block($/) { self.attach-block($/) }
    method        block($/) { self.attach-block($/) }

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
        my $signature := $*PARAMETERIZATION;
        my $block := $*MULTINESS
          ?? Nodify($*SCOPE-KIND).new(:$signature, :multiness($*MULTINESS))
          !! Nodify($*SCOPE-KIND).new(:$signature);
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
        self.takes-loop($/, $*WHILE ?? 'RepeatWhile' !! 'RepeatUntil')
    }
    method statement-control:sym<while>($/) {
        self.takes-loop($/, $*WHILE ?? 'While' !! 'Until')
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

        # collect the if and all of the elsifs / orwiths
        my @elsifs;
        my int $index;
        for @*IF-PARTS {
            @elsifs.push:
              Nodify('Statement',$_).new:
                condition => $<EXPR>[$index].ast,
                then      => $<pointy-block>[$index].ast;
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
        self.attach: $/, Nodify('Statement','Loop').new(|%parts);
    }

#-------------------------------------------------------------------------------
# Pragma and module loading related statements

    # "no foo" can only mean a pragma at the moment
    method statement-control:sym<no>($/) {
        my str $name := $/.pragma2str(~$<module-name>);
        my $Pragma   := Nodify('Pragma');
        if $Pragma.IS-PRAGMA($name) {
            my $argument := $<arglist><EXPR>;
            $argument := $argument.ast if $argument;

            my $ast := $Pragma.new(:$name, :$argument, :off);
            self.attach: $/, $ast;
        }
        else {
            nqp::die("Don't know how to 'no " ~ $name ~ "'just yet")
        }
    }

    method statement-control:sym<use>($/) {
        my str $name := $/.pragma2str(~$<module-name>);
        my $Pragma   := Nodify('Pragma');
        my $argument := $<arglist><EXPR>;
        $argument    := $argument.ast if $argument;
        my $ast;

        if $Pragma.IS-PRAGMA($name) {
            $ast := $Pragma.new(:$name, :$argument);
            $ast.ensure-begin-performed($*R, $*CU.context);
        }

        # proper module loading
        else {
            $ast := Nodify('Statement','Use').new(
              :module-name($<module-name>.ast), :$argument
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
        for $<module-name> {
            @module-names.push: $_.ast;
        }

        my $ast := Nodify('Statement', 'Need').new(:@module-names);

        self.attach: $/, $ast;
    }

    method statement-control:sym<import>($/) {
        my $argument := $<arglist><EXPR>;
        $argument    := $argument.ast if $argument;

        my $ast := Nodify('Statement', 'Import').new(
          :module-name($<module-name>.ast), :$argument
        );
        $ast.to-begin-time($*R, $*CU.context);
        for $ast.IMPL-UNWRAP-LIST($ast.categoricals) {
            $/.add-categorical(
              $_.category, $_.opname, $_.canname, $_.subname, $_.declarand);
        }

        self.attach: $/, $ast;
    }

    method statement-control:sym<require>($/) {
        my $ast := Nodify('Statement', 'Require').new(
            module-name => $<module-name>.ast,
            argument => $<EXPR> ?? $<EXPR>.ast !! Nodify('Expression'),
        );
        self.attach: $/, $ast;
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
            my $phase := $*DOC-PHASER;
            $phase eq 'Begin'
              ?? self.statement-prefix:sym<BEGIN>($/)
              !! self.SP-phaser($/, $phase);
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
    method statement-prefix:sym<sink>($/)    { self.SP-prefix($/, 'Sink')   }
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
          condition => $/[0].ast,
          then      => $<infix><EXPR>.ast,  # the way the grammar parses
          else      => $/[1].ast;
    }

    # An assignment, or infix expression
    method INFIX-EXPR($/) {
        if $<infix><sym> eq '=' {
            my $lhs := $/[0].ast;
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
                    $postfix.set-assignee($/[1].ast);
                    self.attach: $/, $lhs;
                    return;
                }
            }
        }

        my $infix := $/.ast;
        my $node;
        if nqp::istype($infix, Nodify('DottyInfixish')) {
            $node := Nodify('ApplyDottyInfix').new:
              infix => $infix, left => $/[0].ast, right => $/[1].ast;
        }
        else {
            $node := Nodify('ApplyInfix').new:
              infix => $infix, left => $/[0].ast, right => $/[1].ast;
        }
        my $cu := $*CU; # Might be too early to even have a CompUnit
        $node.set-origin(
            Nodify('Origin').new(
                :from($/[0].from),
                :to($/[1].to),
                :source($*ORIGIN-SOURCE)));
        $node.to-begin-time($*R, $cu ?? $cu.context !! NQPMu);
        make $node;
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
              postfix => Nodify('Postfix').new(:operator(~$<postfix><sym>)),
              operand => $operand;
        }
    }

#-------------------------------------------------------------------------------
# Prefix operators

    # Alpha prefix actions are statically coupled to their appropriate
    # prefix object to allow a slang to change the wording of these prefix
    # operators *without* needing to supply additional actions mapping
    # them to the correct english prefix name.
    method prefix:sym<let>($/) {
        self.attach: $/, Nodify('Prefix').new('let')
    }
    method prefix:sym<not>($/) {
        self.attach: $/, Nodify('Prefix').new('not')
    }
    method prefix:sym<so>($/) {
        self.attach: $/, Nodify('Prefix').new('so')
    }
    method prefix:sym<temp>($/) {
        self.attach: $/, Nodify('Prefix').new('temp')
    }

    method prefixish($/) {
        my $ast := $<OPER>.ast // Nodify('Prefix').new(~$<prefix><sym>);
        $ast := $<prefix-postfix-meta-operator>.ast.new($ast.to-begin-time($*R, $*CU.context))
          if $<prefix-postfix-meta-operator>;
        self.attach: $/, $ast;
    }

    method prefix-postfix-meta-operator:sym<«>($/) {
        make Nodify('MetaPrefix', 'Hyper');
    }

#-------------------------------------------------------------------------------
# Postfix operators

    method postfixish($/) {
        my $ast := $<OPER>.ast
          // Nodify('Postfix').new(:operator(~$<postfix><sym>));

        self.attach: $/, $<postfix-prefix-meta-operator>
          ?? Nodify('MetaPostfix','Hyper').new($ast.to-begin-time($*R, $*CU.context))
          !! $ast
    }

    method postfix-prefix-meta-operator:sym<»>($/) {
        # Check if we are inside «...» quoters and complain if the hyper creates
        # ambiguity with the quoters, since user may not wanted to have a hyper
        my str $sym := ~$<sym>;
        $/.worry("Ambiguous use of $sym; use "
          ~ ($sym eq '>>' ?? '»' !! '>>')
          ~ " instead to mean hyper, or insert whitespace before"
          ~ " $sym to mean a quote terminator (or use different delimiters?)"
        ) if ($/.pragma("STOPPER") // '') eq $sym;
    }

    method postop($/) {
        self.attach: $/, $<postfix> ?? $<postfix>.ast !! $<postcircumfix>.ast;
    }

    method postcircumfix:sym<[ ]>($/) {
        self.attach: $/, Nodify('Postcircumfix', 'ArrayIndex').new(:index($<semilist>.ast));
    }

    method postcircumfix:sym<{ }>($/) {
        self.attach: $/, Nodify('Postcircumfix', 'HashIndex').new(:index($<semilist>.ast));
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
        my $ast;

        if $<methodop> {
            $ast := $<methodop>.ast;
        }
        elsif $<postop> {
            $ast := $<postop>.ast;
        }
        elsif $<colonpair> {
            my $cp := $<colonpair>;
            if $cp<identifier> eq "" && $cp<coloncircumfix> -> $cf {
                if $cf<circumfix> -> $op {
                    $ast := Nodify('Call','Name').new(
                      name => Nodify('Name').from-identifier(
                        'prefix:' ~ Nodify('ColonPairish').IMPL-QUOTE-VALUE(
                          Nodify('BeginTime').IMPL-BEGIN-TIME-EVALUATE(
                            (
                              $op<nibble> // $op<semilist> // $op<pointy-block>
                            ).ast, $*R, $*CU.context
                          )
                        )
                      )
                    );
                }
                else {
                    nqp::die('NYI kind of dottyop with coloncircumfix');
                }
            }
            else {
                $ast := $cp.ast;
            }
        }
        else {
            nqp::die('NYI kind of dottyop');
        }

        self.attach: $/, $ast;
    }

    method privop($/) {
        self.attach: $/, $<methodop>.ast;
    }

    method methodop($/) {
        my $args := $<args> ?? $<args>.ast !! Nodify('ArgList').new();
        my $ast;

        if $<longname> -> $longname {
            $ast     := $longname.ast.without-colonpairs;
            my $name := $ast.canonicalize;

            if $*DOTTY {
                my $DOTTY := $*DOTTY;
                $/.dotty-non-ident($DOTTY) unless $ast.is-identifier;

                $ast := $DOTTY eq '!'
                  ?? Nodify('Call','PrivateMethod').new(:name($ast),:$args)
                  !! $DOTTY eq '.^'
                    ?? Nodify('Call','MetaMethod').new(:$name, :$args)
                    !! $DOTTY eq '.?'
                      ?? Nodify('Call', 'MaybeMethod').new(:$name, :$args)
                      !! $DOTTY eq '.&'
                        ?? Nodify('Call','VarMethod').new(:name($ast), :$args)
                        !! nqp::die("Missing compilation of $DOTTY");
            }
            else {
                $ast := Nodify('Call','Method').new(
                  :name($longname.core2ast.without-colonpairs), :$args
                );
            }
        }
        elsif $<quote> {
            $ast := Nodify('Call','QuotedMethod').new(
              :name($<quote>.ast), :$args
            );
        }
        elsif $<variable> {
            $ast := Nodify('Call','BlockMethod').new(:block($<variable>.ast), :$args);
        }
        else {
            nqp::die('NYI kind of methodop');
        }

        self.attach: $/, $ast
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
        my int $chars := nqp::chars($digits);
        my int $i;

        while $i < $chars {
            $value := nqp::add_I(
              nqp::mul_I($value, nqp::box_i(10, $Int), $Int),
              nqp::box_i(nqp::index($from, nqp::substr($digits,$i,1)), $Int),
              $Int
            );
            ++$i;
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

#-------------------------------------------------------------------------------
# Infix operators

    # Assignment in Raku can take two forms: item and list assignment:
    # my @b = my $a = 1,2,3;  # $a = 1, @b = 1,2,3 .  In the item
    # assignment case, the '=' gets a higher precedence than the ','.
    # In the list assignment case, a lowed precedence.  So the expression
    # is really: my @b = (my $a = 1),2,3 .  The grammar is supposed to
    # set the $*ITEM dynamic variable to a truthy value if item assignment
    # is to be assumed.
    method infix:sym<=>($/) {
        self.attach: $/, Nodify('Assignment').new(:item($*ITEM));
    }

    # These infix operators are purely a grammar construct at the moment
    method infix:sym«==>»($/)   { self.attach: $/, Nodify('Feed').new($<sym>) }
    method infix:sym«<==»($/)   { self.attach: $/, Nodify('Feed').new($<sym>) }
    method infix:sym«==>>»($/)  { self.attach: $/, Nodify('Feed').new($<sym>) }
    method infix:sym«<<==»($/)  { self.attach: $/, Nodify('Feed').new($<sym>) }

    method infix:sym<ff>($/) {
        self.attach: $/, Nodify('FlipFlop').new('ff')
    }
    method infix:sym<^ff>($/) {
        self.attach: $/, Nodify('FlipFlop').new('^ff')
    }
    method infix:sym<ff^>($/) {
        self.attach: $/, Nodify('FlipFlop').new('ff^')
    }
    method infix:sym<^ff^>($/) {
        self.attach: $/, Nodify('FlipFlop').new('^ff^')
    }
    method infix:sym<fff>($/) {
        self.attach: $/, Nodify('FlipFlop').new('fff')
    }
    method infix:sym<^fff>($/) {
        self.attach: $/, Nodify('FlipFlop').new('^fff')
    }
    method infix:sym<fff^>($/) {
        self.attach: $/, Nodify('FlipFlop').new('fff^')
    }
    method infix:sym<^fff^>($/) {
        self.attach: $/, Nodify('FlipFlop').new('^fff^')
    }

    method infix:sym<.>($/) {
        self.attach: $/, Nodify('DottyInfix','Call').new;
    }
    method infix:sym<.=>($/) {
        self.attach: $/, Nodify('DottyInfix','CallAssign').new;
    }
    # A ternary op arrives here as '?? expression !!', so check for that and
    # create a dummy infix AST for further parsing / EXPR handling if so
    method infix:sym<?? !!>($/) {
        self.attach: $/, Nodify('Infix').new('?? !!')
    }

    # Alpha infix actions are statically coupled to their appropriate
    # infix object to allow a slang to change the wording of these infix
    # operators *without* needing to supply additional actions mapping
    # them to the correct english infix name.
    method infix:sym<after>($/) {
        self.attach: $/, Nodify('Infix').new('after')
    }
    method infix:sym<and>($/) {
        self.attach: $/, Nodify('Infix').new('and')
    }
    method infix:sym<andthen>($/) {
        self.attach: $/, Nodify('Infix').new('andthen')
    }
    method infix:sym<before>($/) {
        self.attach: $/, Nodify('Infix').new('before')
    }
    method infix:sym<but>($/) {
        self.attach: $/, Nodify('Mixin').new('but')
    }
    method infix:sym<cmp>($/) {
        self.attach: $/, Nodify('Infix').new('cmp')
    }
    method infix:sym<(cont)>($/) {
        self.attach: $/, Nodify('Infix').new('(cont)')
    }
    method infix:sym<coll>($/) {
        self.attach: $/, Nodify('Infix').new('coll')
    }
    method infix:sym<div>($/) {
        self.attach: $/, Nodify('Infix').new('div')
    }
    method infix:sym<does>($/) {
        self.attach: $/, Nodify('Mixin').new('does')
    }
    method infix:sym<(elem)>($/) {
        self.attach: $/, Nodify('Infix').new('(elem)')
    }
    method infix:sym<eq>($/) {
        self.attach: $/, Nodify('Infix').new('eq')
    }
    method infix:sym<eqv>($/) {
        self.attach: $/, Nodify('Infix').new('eqv')
    }
    method infix:sym<gcd>($/) {
        self.attach: $/, Nodify('Infix').new('gcd')
    }
    method infix:sym<ge>($/) {
        self.attach: $/, Nodify('Infix').new('ge')
    }
    method infix:sym<gt>($/) {
        self.attach: $/, Nodify('Infix').new('gt')
    }
    method infix:sym<lcm>($/) {
        self.attach: $/, Nodify('Infix').new('lcm')
    }
    method infix:sym<le>($/) {
        self.attach: $/, Nodify('Infix').new('le')
    }
    method infix:sym<leg>($/) {
        self.attach: $/, Nodify('Infix').new('leg')
    }
    method infix:sym<lt>($/) {
        self.attach: $/, Nodify('Infix').new('lt')
    }
    method infix:sym<max>($/) {
        self.attach: $/, Nodify('Infix').new('max')
    }
    method infix:sym<min>($/) {
        self.attach: $/, Nodify('Infix').new('min')
    }
    method infix:sym<minmax>($/) {
        self.attach: $/, Nodify('Infix').new('minmax')
    }
    method infix:sym<mod>($/) {
        self.attach: $/, Nodify('Infix').new('mod')
    }
    method infix:sym<ne>($/) {
        self.attach: $/, Nodify('Infix').new('ne')
    }
    method infix:sym<notandthen>($/) {
        self.attach: $/, Nodify('Infix').new('notandthen')
    }
    method infix:sym<or>($/) {
        self.attach: $/, Nodify('Infix').new('or')
    }
    method infix:sym<orelse>($/) {
        self.attach: $/, Nodify('Infix').new('orelse')
    }
    method infix:sym<unicmp>($/) {
        self.attach: $/, Nodify('Infix').new('unicmp')
    }
    method infix:sym<x>($/) {
        self.attach: $/, Nodify('Infix').new('x')
    }
    method infix:sym<xor>($/) {
        self.attach: $/, Nodify('Infix').new('xor')
    }
    method infix:sym<xx>($/) {
        self.attach: $/, Nodify('Infix').new('xx')
    }

    # Handle all of the infix:<sym> that don't have their own action method
    method infixish($/) {
        return 0 if $<adverb-as-infix>;

        my $ast := $<infix>
          ?? ($<infix>.ast || Nodify('Infix').new(~$<infix>))
          !! $<infix-prefix-meta-operator>
            ?? $<infix-prefix-meta-operator>.ast
            !! $<infix-circumfix-meta-operator>
              ?? $<infix-circumfix-meta-operator>.ast
              !! $<infixish>
                ?? Nodify('BracketedInfix').new($<infixish>.ast)
                !! $<variable>
                  ?? Nodify('FunctionInfix').new($<variable>.ast)
                  !! nqp::die('Unknown kind of infix: ' ~ $/);

        self.attach: $/, $<infix-postfix-meta-operator>
          ?? $<infix-postfix-meta-operator>.ast.new($ast.to-begin-time($*R, $*CU.context))
          !! $ast;
    }

    method infix-prefix-meta-operator:sym<!>($/) {
        self.attach: $/, Nodify('MetaInfix', 'Negate').new($<infixish>.ast);
    }

    method infix-prefix-meta-operator:sym<R>($/) {
        self.attach: $/, Nodify('MetaInfix', 'Reverse').new($<infixish>.ast);
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
          infix      => $<infixish>.ast,
          dwim-left  => $<opening> eq '«',
          dwim-right => $<closing> eq '»'
    }
    method infix-circumfix-meta-operator:sym«<< >>»($/) {
        self.attach: $/, Nodify('MetaInfix', 'Hyper').new:
          infix      => $<infixish>.ast,
          dwim-left  => $<opening> eq '<<',
          dwim-right => $<closing> eq '>>'
    }

#-------------------------------------------------------------------------------
# Circumfix operators

    method circumfix:sym<( )>($/) {
        self.attach: $/,
          Nodify('Circumfix','Parentheses').new($<semilist>.ast)
    }

    method circumfix:sym<[ ]>($/) {
        self.attach: $/,
          Nodify('Circumfix','ArrayComposer').new($<semilist>.ast)
    }

    method circumfix:sym<{ }>($/) {
        self.attach($/, $<pointy-block>.ast.block-or-hash(:object-hash($*OBJECT-HASH || 0)))
    }

    method circumfix:sym<ang>($/) { self.attach: $/, $<nibble>.ast }

    method circumfix:sym«<< >>»($/) { self.attach: $/, $<nibble>.ast }
    method circumfix:sym<« »>($/)   { self.attach: $/, $<nibble>.ast }

#-------------------------------------------------------------------------------
# Stubs

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

#-------------------------------------------------------------------------------
# Terms

    method term:sym<::?IDENT>($/) {
        self.attach: $/, Nodify('Var', 'Lexical', 'Constant').new(~$/)
    }

    method term:sym<self>($/) {
        self.attach: $/, Nodify('Term', 'Self').new
    }

    method term:sym<nano>($/) {
        self.attach: $/, Nodify('Term', 'Named').new('nano')
    }

    method term:sym<now>($/) {
        self.attach: $/, Nodify('Term', 'Named').new('now')
    }

    method term:sym<time>($/) {
        self.attach: $/, Nodify('Term', 'Named').new('time')
    }

    method term:sym<empty_set>($/) {
        self.attach: $/, Nodify('Term', 'EmptySet').new
    }

    method term:sym<rand>($/) {
        self.attach: $/, Nodify('Term', 'Rand').new
    }

    method term:sym<fatarrow>($/) {
        self.attach: $/, Nodify('FatArrow').new:
          key   => $*LITERALS.intern-str(~$<key>),
          value => $<val>.ast
    }

    method term:sym<colonpair>($/) {
        self.attach: $/, $<colonpair>.ast
    }

    method term:sym<variable>($/) {
        self.attach: $/, $<variable>.ast
    }

    method term:sym<package-declarator>($/) {
        self.attach: $/, $<package-declarator>.ast
    }

    method term:sym<scope-declarator>($/) {
        self.attach: $/, $<scope-declarator>.ast
    }

    method term:sym<routine-declarator>($/) {
        self.attach: $/, $<routine-declarator>.ast
    }

    method term:sym<multi-declarator>($/) {
        self.attach: $/, $<multi-declarator>.ast
    }

    method term:sym<regex-declarator>($/) {
        self.attach: $/, $<regex-declarator>.ast
    }

    method term:sym<type-declarator>($/) {
        self.attach: $/, $<type-declarator>.ast
    }

    method term:sym<statement-prefix>($/) {
        self.attach: $/, $<statement-prefix>.ast
    }

    method term:sym<*>($/) {
        self.attach: $/, Nodify('Term', 'Whatever').new
    }

    method term:sym<**>($/) {
        self.attach: $/, Nodify('Term', 'HyperWhatever').new
    }

    method term:sym<lambda>($/) {
        self.attach: $/, $<pointy-block>.ast
    }

    method term:sym<value>($/) {
        self.attach: $/, $<value>.ast
    }

    method term:sym<identifier>($/) {
        my $args := $<args>.ast;
        my $name := $<identifier>.core2ast;
        self.attach: $/, (my $invocant := $args.invocant)
            # Indirect method call syntax, e.g. key($pair:)
            ?? Nodify('ApplyPostfix').new(
                operand => $invocant,
                postfix => Nodify('Call', 'Method').new(:$name, :$args)
            )
            !! Nodify('Call', 'Name').new:
              name => $name,
              args => $args,
    }

    method term:sym<nqp::op>($/) {
        my $op := ~$<op>;
        $*LANG.pragma('MONKEY-GUTS')
          ?? self.attach: $/, Nodify('Nqp').new: $op, $<args>.ast
          !! $/.typed-panic('X::NQP::NotFound', :$op);
    }

    method term:sym<nqp::const>($/) {
        self.attach: $/, Nodify('Nqp', 'Const').new(~$<const>);
    }

    method term:sym<name>($/) {
        my $name := $<longname>.core2ast;
        if $*META-OP {
            my $META := $*META-OP.ast;
            $META.to-begin-time($*R, $*CU.context);

            my $meta-op := $META.IMPL-HOP-INFIX;
            my $op := Nodify('Constant').new($meta-op);
            self.attach: $/, Nodify('ApplyPostfix').new(
                operand => $op,
                postfix => Nodify('Call', 'Term').new(args => $<args>.ast),
            )
        }
        else {
            if $<args> {
                my $args := $<args>.ast;
                self.attach: $/, (my $invocant := $args.invocant)
                  # Indirect method call syntax, e.g. new Int: 1
                  ?? Nodify('ApplyPostfix').new(
                       operand => $invocant,
                       postfix => Nodify('Call','Method').new(:$name, :$args)
                     )
                  # Normal named call
                  !! $name.is-identifier && !$name.has-colonpairs
                    ?? Nodify('Call','Name','WithoutParentheses').new(:$name, :$args)
                    !! Nodify('Call','Name').new(:$name, :$args)
            }
            else {
                self.attach: $/, $*IS-TYPE
                  ?? self.type-for-name($/, $name)
                  !! Nodify('Term', 'Name').new($name)
            }
        }
    }

    method term:sym<dotty>($/) {
        self.attach: $/, Nodify('Term', 'TopicCall').new($<dotty>.ast)
    }

    method term:sym<capture>($/) {
        self.attach: $/, Nodify('Term', 'Capture').new($<args>.ast)
    }

    method term:sym<onlystar>($/) {
        self.attach: $/, Nodify('OnlyStar').new
    }

    method colonpair($/) {
        my $key-str := ~$*KEY;
        if $key-str {
            my $key := $*LITERALS.intern-str($key-str);
            self.attach: $/, $<num>
              ?? Nodify('ColonPair', 'Number').new(
                   key   => $key,
                   value => Nodify('IntLiteral').new(
                     $*LITERALS.intern-int(~$<num>)
                   )
                 )
              !! $<coloncircumfix>
                ?? Nodify('ColonPair', 'Value').new(
                     key => $key, value => $<coloncircumfix>.ast
                   )
                !! $<var>
                  ?? Nodify('ColonPair', 'Variable').new(
                       key => $key, value => $<var>.ast
                     )
                  !! Nodify('ColonPair',$<neg> ?? 'False' !! 'True').new($key);
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
        $<capvar>
          ?? self.attach($/, Nodify('Var', 'NamedCapture').new(
               Nodify('QuotedString').new(
                 :segments(Nodify('QuotedString').IMPL-WRAP-LIST([
                   Nodify('StrLiteral').new(~$<desigilname>)
                 ]))
               )
             ))
          !! self.simple-variable($/);
    }

    method variable($/) {
        if $<index> {
            self.attach: $/,
              Nodify('Var','PositionalCapture').new(
                $*LITERALS.intern-int(~$<index>)
              );
        }
        elsif $<postcircumfix> {
            self.attach: $/,
              Nodify('Var','NamedCapture').new($<postcircumfix>.ast.index);
        }
        elsif $<contextualizer> {
            self.attach: $/, $<contextualizer>.ast;
        }
        elsif $<infixish> {
            my $name := Nodify('Name').from-identifier('infix');
            $name.add-colonpair(
                Nodify('QuotedString').new(
                    :segments($name.IMPL-WRAP-LIST([
                        Nodify('StrLiteral').new(~$<infixish>)
                    ]))
                )
            );
            self.compile-variable-access($/, '&', '', $name);
        }
        elsif $<desigilname><variable> {
            self.contextualizer-for-sigil(
              $/, ~$<sigil>, $<desigilname><variable>.ast);
        }
        else {
            self.simple-variable($/);
        }
    }

    # Compile variable access for a simple variable
    method simple-variable($/) {
        my str $twigil  := $<twigil> ?? ~$<twigil> !! '';
        my $desigilname := $<desigilname><longname>
          ?? $<desigilname><longname>.ast
          !! Nodify('Name').from-identifier(~$<desigilname>);
        self.compile-variable-access($/, ~$<sigil>, $twigil, $desigilname);
    }

    # Declare @_ / %_
    sub slurpy-placeholder($class) {
        my $decl := Nodify('VarDeclaration','Placeholder',$class).new;
        $*R.declare-lexical($decl);
        $decl
    }

    sub sigil-to-context(str $sigil) {
        $sigil eq '@' ?? 'List' !! $sigil eq '%' ?? 'Hash' !! 'Item'
    }

    method compile-variable-access($/, $sigil, $twigil, $desigilname) {
        $desigilname.to-begin-time($*R, $*CU.context);
        my str $name := $sigil ~ $twigil ~ $desigilname.canonicalize;
        my $ast;

        if $twigil eq '' {
            if $name eq '@_' {
                $ast := slurpy-placeholder('SlurpyArray');
            }
            elsif $name eq '%_' {
                $ast := slurpy-placeholder('SlurpyHash');
            }

            # an anonymous state variable.
            elsif $desigilname.is-empty {
                $ast := Nodify('VarDeclaration','Anonymous').new(
                  :$sigil, :scope<state>
                );
            }

            # simple variable
            elsif $desigilname.is-identifier {

                # strict is active or identifier already known
                if $*LANG.pragma("strict") || $*R.resolve-lexical($name) {
                    $ast := Nodify('Var', 'Lexical').new(
                      :$sigil, :$desigilname
                    );
                }

                # strict is *NOT* active and identifier not known
                else {
                    $ast := Nodify('VarDeclaration', 'Auto').new:
                      :scope<our>, :$desigilname, :$sigil, :$twigil;
                    $*R.declare-lexical($ast);
                }
            }

            # package variable
            else {
                $ast := Nodify('Var','Package').new(
                  :$sigil, :name($desigilname)
                );
            }
        }
        elsif $twigil eq '*' {
            $ast := Nodify('Var','Dynamic').new($name);
        }
        elsif $twigil eq '!' {
            $ast := Nodify('Var','Attribute').new($name);
        }
        elsif $twigil eq '.' {
            $ast := Nodify('Var','Attribute','Public').new($name);
        }
        elsif $twigil eq '?' {
            my $origin-source := $*ORIGIN-SOURCE;
            $ast := $name eq '$?FILE'
              ?? Nodify('Var','Compiler','File').new(
                   $*LITERALS.intern-str($origin-source.original-file)
                 )
              !! $name eq '$?LINE'
                ?? Nodify('Var','Compiler','Line').new(
                     $*LITERALS.intern-int($origin-source.original-line($/.from))
                   )
                !! $name eq '&?BLOCK'
                  ?? Nodify('Var','Compiler','Block').new
                  !! $name eq '&?ROUTINE'
                    ?? Nodify('Var','Compiler','Routine').new
                    !! Nodify('Var','Compiler','Lookup').new($name);
        }
        elsif $twigil eq '^' {
            $ast := Nodify('VarDeclaration','Placeholder','Positional').new(
              $sigil ~ $desigilname.canonicalize
            );
            $*R.declare-lexical($ast);
        }
        elsif $twigil eq ':' {
            $ast := Nodify('VarDeclaration', 'Placeholder', 'Named').new(
              $sigil ~ $desigilname.canonicalize
            );
            $*R.declare-lexical($ast);
        }
        elsif $twigil eq '=' {
            if $name eq '$=pod'
              || $name eq '$=data'
              || $name eq '$=finish'
              || $name eq '$=rakudoc' {
                $ast := Nodify('Var','Doc').new(nqp::substr($name,2));
            }
            else {
                nqp::die("Pod variable $name NYI");
            }
        }
        elsif $twigil eq '~' {
            my $name := $desigilname.canonicalize;
            $ast := Nodify('Var','Slang').new(
              grammar => $/.slang_grammar($name),
              actions => $/.slang_actions($name)
            );
        }
        else {
            nqp::die("Lookup with twigil '$twigil' NYI");
        }

        self.attach: $/, $ast;
    }

    method contextualizer($/) {
        self.contextualizer-for-sigil($/, ~$<sigil>, $<coercee>.ast);
    }

    method contextualizer-for-sigil($/, $sigil, $target) {
        self.attach: $/,
          Nodify('Contextualizer', sigil-to-context($sigil)).new($target);
    }

    method term:sym<reduce>($/) {
        my $infix := $<op>.ast // Nodify('Infix').new($<op><OPER><sym>);
        self.attach: $/, Nodify('Term', 'Reduce').new(:$infix, :args($<args>.ast),
            :triangle(?$<triangle>));
    }

#-------------------------------------------------------------------------------
# Declarations

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
        my $ast  := $*PACKAGE;
        my $body := $<block> || $<unit-block>;

        if is-yada($body) {
            $ast.set-is-stub(1);
        }
        else {
            $ast.replace-body($body.ast, $<signature> ?? $<signature>.ast !! Mu);
            $ast.body.IMPL-BEGIN($*R, $*CU.context); # Have body Sub declare its implicits before we cache them
            $ast.to-begin-time($*R, $*CU.context);
            $ast.IMPL-COMPOSE;
        }

        self.attach: $/, $ast;
    }

    method stub-package($/) {
        # Resolve the meta-object.
        my $declarator := $*PKGDECL;
        my $how := $/.know_how($declarator)
          ?? $/.how($declarator)
          !! $/.panic("Cannot resolve meta-object for $declarator");

        # Stub the package AST node.
        my str $scope := $*SCOPE // 'our';
        my $augmented := $scope eq 'augment';
        $/.typed-panic('X::Syntax::Augment::WithoutMonkeyTyping')
          if $augmented && !$*LANG.pragma('MONKEY-TYPING');

        my $name-match := $*PACKAGE-NAME;
        my $name       := $name-match ?? $name-match.ast !! Nodify('Name');
        my $package    := Nodify(nqp::tclc($declarator)).new(
          :$how, :$name, :$scope, :$augmented
        );

        if $augmented {
            $package.to-begin-time($*R, $*CU.context);
        }
        else {
            $package.to-parse-time($*R, $*CU.context);
        }

        self.set-declarand($/, $*PACKAGE := $package);
    }

    method enter-package-scope($/) {
        # Perform BEGIN-time effects (declaring the package, applying traits,
        # etc.)
        my $R := $*R;
        my $package := $*PACKAGE;
        if nqp::istype($package, Nodify('ParseTime')) {
            $package.ensure-parse-performed($R, $*CU.context);
        }
        $package.ensure-begin-performed($R, $*CU.context);

        # Let the resolver know which package we're in.
        $R.push-package($package);

        if $*SIGNATURE {
            my $params := $*SIGNATURE.ast;
            for $params.IMPL-UNWRAP-LIST($params.parameters) {
                $R.declare-lexical($_.target) if $_.target;
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
        self.attach: $/, ($<declarator> || $<routine-def>).ast;
    }

    method multi-declarator:sym<proto>($/) {
        self.attach: $/, ($<declarator> || $<routine-def>).ast;
    }

    method multi-declarator:sym<only>($/) {
        self.attach: $/, ($<declarator> || $<routine-def>).ast;
    }

    method multi-declarator:sym<null>($/) {
        self.attach: $/, $<declarator>.ast;
    }

    method declarator($/) {
        my $ast;

        if $<variable-declarator> {
            $ast := $<variable-declarator>.ast;
        }
        elsif $<type-declarator> {
            $ast := $<type-declarator>.ast;
        }
        elsif $<signature> {
            my str $scope   := $*SCOPE;
            my     $type    := $*OFTYPE ?? $*OFTYPE.ast !! Nodify('Type');
            my $initializer := $<initializer>
              ?? $<initializer>.ast
              !! Nodify('Initializer');

            $ast := Nodify('VarDeclaration','Signature').new:
              :signature($<signature>.ast), :$scope, :$type, :$initializer;
            for $<trait> {
                $ast.add-trait($_.ast);
            }
        }
        elsif $<routine-declarator> {
            $ast := $<routine-declarator>.ast;
        }
        elsif $<defterm> {
            my str $scope   := $*SCOPE;
            my     $type    := $*OFTYPE ?? $*OFTYPE.ast !! Nodify('Type');
            my     $name    := $<defterm>.ast;
            my $initializer := $<term-init>.ast;

            $ast := Nodify('VarDeclaration','Term').new:
              :$scope, :$type, :$name, :$initializer;
            $/.typed-sorry('X::Redeclaration', :symbol($name))
              if $*R.declare-lexical($ast);
        }
        else {
            nqp::die('Unimplemented declarator');
        }

        self.attach: $/, $ast;
    }

    method initializer:sym<=>($/) {
        self.attach: $/, Nodify('Initializer','Assign').new($<EXPR>.ast);
    }

    method initializer:sym<:=>($/) {
        self.attach: $/, Nodify('Initializer','Bind').new($<EXPR>.ast);
    }

    method initializer:sym<.=>($/) {
        self.attach: $/, Nodify('Initializer','CallAssign').new($<dottyop>.ast);
    }

    method stub-variable($stub) {
        my $/ := $*VARIABLE-MATCH;

        my str $scope := $*SCOPE;
        my     $type  := $*OFTYPE.ast if $*OFTYPE;
        my str $sigil := $<sigil>;
        my     $where := $*WHERE;

        # No type or type is not a native type
        if !$type || !$type.is-native {
            my str $pragma  := $scope eq 'has' || $scope eq 'HAS'
              ?? 'attributes'
              !! $scope eq 'my' || $scope eq 'our' || $scope eq 'state'
                ?? 'variables'
                !! '';

            # Need to check pragma setting *and* there is a setting
            if $pragma && $*LANG.pragma($pragma) -> $value {
                my $definedness := Nodify('Type','Definedness');
                if !nqp::eqaddr($type.WHAT,$definedness)  # not already :D or :U
                  && ($value eq 'D' || $value eq 'U') {   # want :D or :U
                    $type := $definedness.new(            # wrap existing or new
                      :base-type($type // Nodify('Type','Simple').new(
                        Nodify('Name').from-identifier('Any')
                      )), :definite($value eq 'D'), :through-pragma
                    );
                }
            }
        }

        # Fallback to no explicit type
        $type := Nodify('Type') unless $type;

        my $decl;
        if $<desigilname> {
            my $desigilname := $<desigilname>;
            my $ast := $desigilname<longname>
                ?? $desigilname<longname>.ast
                !! Nodify('Name').from-identifier(~$desigilname);
            $ast.to-begin-time($*R, $*CU.context);

            my str $twigil := $<twigil> || '';
            my str $name   := $sigil ~ $twigil ~ $ast.canonicalize;
            my $shape := $<semilist> ?? $<semilist>[0].ast !! Nodify('SemiList');
            my $dynprag := $*LANG.pragma('dynamic-scope');
            my $forced-dynamic := $dynprag ?? $dynprag($name) !! 0;
            $decl := Nodify('VarDeclaration','Simple').new:
              :$scope, :$type, :$sigil, :$twigil, :desigilname($ast),
              :$shape, :$forced-dynamic, :$where;

            $/.typed-worry('X::Redeclaration', :symbol($name))
              if ($scope eq 'my' || $scope eq 'state' || $scope eq 'our')
              && $*R.declare-lexical($decl);

            self.set-declarand($/, $decl);
        }
        else {
            $scope ne 'my' && $scope ne 'state'
              ?? $/.panic(
                   "Cannot declare an anonymous '$scope' scoped variable"
                 )
              !! ($decl := Nodify('VarDeclaration', 'Anonymous').new(
                     :$scope, :$type, :$sigil, :twigil(~$<twigil> || '')
                   ));
        }

        $decl.to-parse-time($*R, $*CU.context);

        $*VARIABLE := $decl;
    }

    method variable-declarator($/) {
        my str $scope := $*SCOPE;
        my     $var   := $*VARIABLE;

        my $initializer := $<initializer>;
        if $initializer {
            $/.panic("Cannot use := to initialize an attribute")
              if $scope eq 'has' && $initializer<sym> eq ':=';

            $var.set-initializer($initializer.ast);
        }

        for $<trait> {
            $var.add-trait($_.ast);
        }

        if $var.is-attribute && !$var.twigil {
            $*R.declare-lexical:
                Nodify('VarDeclaration', 'AttributeAlias').new(
                    :desigilname($var.desigilname),
                    :sigil($var.sigil),
                    :attribute($var),
                );
        }

        self.attach: $/, $var;
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
        my $return-type := $*OFTYPE.ast if $*OFTYPE;
        $routine.replace-body($<onlystar>
          ?? Nodify('OnlyStar').new
          !! $<blockoid>.ast
        );
        # Entering scope again would throw off proto installation
        self.attach: $/, $routine;
    }

    method method-def($/) {
        my $method := $*BLOCK;

        # Handle localizations for BUILD, TWEAK, ACCEPTS, etc
        if $method.name.simple-identifier -> $name {
            my $sys-name := $/.system2str($name);
            $method.replace-name(Nodify('Name').from-identifier($sys-name))
              unless $sys-name eq $name;
        }

        if $<specials> {
            my $specials := ~$<specials>;
            if $specials eq '^' {
                $method.set-meta(1);
            }
            elsif $specials eq '!' {
                $method.set-private(1);
            }
        }
        $method.replace-body($<onlystar>
          ?? Nodify('OnlyStar').new
          !! $<blockoid>.ast
        );
        # Entering scope again to ensure implicits are attached to the method
        $*R.enter-scope($method);
        self.attach: $/, $method;
        $*R.leave-scope;
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
        # Entering scope again to ensure implicits are attached to the method
        $*R.enter-scope($regex);
        self.attach: $/, $regex;
        $*R.leave-scope;
    }

    method type-declarator:sym<constant>($/) {
        # Provided it's named, install it.
        my %args;
        if $<defterm> {
            %args<name> := $<defterm>.ast.canonicalize;
        }
        elsif $<variable> {
            my $variable := $<variable>;
            if $variable<twigil> {
                my $twigil := ~$variable<twigil>;

                if $twigil eq '?' {
                    unless $*COMPILING_CORE_SETTING {
                        $/.typed-panic('X::Comp::NYI',
                          feature => "Constants with a '$twigil' twigil"
                        );
                    }
                }
                elsif $twigil eq '*' {
                    $/.typed-panic('X::Syntax::Variable::Twigil',
                      name       => ~$variable,
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

            %args<name> := ~$variable;
        }
        %args<scope> := $*SCOPE;
        %args<type>  := $*OFTYPE.ast if nqp::defined($*OFTYPE);
        %args<initializer> := $<initializer>.ast;
        if $<trait> {
            %args<traits> := my @traits;
            @traits.push($_.ast) for $<trait>;
        }

        my $decl := Nodify('VarDeclaration','Constant').new(|%args);
        $/.typed-panic('X::Redeclaration', :symbol(%args<name>))
          if $*R.declare-lexical($decl);
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

    # Special handling for "is repr"
    method handle-is-repr($/) {
        my $circumfix := $<circumfix>;
        if $circumfix {
            my $ast  := $circumfix.ast;
            my $repr := nqp::istype($ast,Nodify('Circumfix'))
                ?? $ast.IMPL-UNWRAP-LIST($ast.semilist.statements)[0]
                !! nqp::istype($ast,Nodify('QuotedString'))
                    ?? $ast
                    !! nqp::die(
                         "NYI trait_mod circumfix " ~ $ast.HOW.name($ast)
                       );

            unless $repr.IMPL-CAN-INTERPRET {
                $/.typed-panic: 'X::Value::Dynamic',
                  :what('is repr(...) trait');
            }
            $repr := $repr.IMPL-INTERPRET(Nodify('IMPL','InterpContext').new);
            $*PACKAGE.set-repr($repr);
        }
        else {
            $/.panic("is repr(...) trait needs a parameter");
        }
    }

    method trait_mod:sym<is>($/) {
        my $longname := $<longname>;
        return self.handle-is-repr($/) if ~$longname eq 'repr';

        my $circumfix := $<circumfix>;
        my $trait := $<typename>
            ?? Nodify('Trait', 'Is').new-from-type(:type($<typename>.ast))
            !! Nodify('Trait', 'Is').new(
              :name($longname.trait-is2ast),
              :argument($circumfix ?? $circumfix.ast !! Mu)
            );

        self.attach: $/, $trait;
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

#-------------------------------------------------------------------------------
# Values

    method value:sym<quote>($/)   { self.attach: $/, $<quote>.ast   }
    method value:sym<number>($/)  { self.attach: $/, $<number>.ast  }
    method value:sym<version>($/) { self.attach: $/, $<version>.ast }

    method number:sym<numish>($/) { self.attach: $/, $<numish>.ast }

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
        if $<integer> {
            $attachee := Nodify('IntLiteral').new($<integer>.ast);
        }

        # 42.137
        elsif $<decimal-number> {
            $attachee := $<decimal-number>.ast;
        }

        # :16(42)
        elsif $<radix-number> {
            $attachee := $<radix-number>.ast;
        }

        # -22/33
        elsif $<rational-number> {
            $attachee := $<rational-number>.ast;
        }

        # 42+i1
        elsif $<complex-number> {
            $attachee := $<complex-number>.ast;
        }

        # Ⅼ or ⅔
        elsif $<unum> {
            my int $ord := nqp::ord($/.Str);
            my int $nu  := ord-to-numerator($ord);
            my int $de  := ord-to-denominator($ord);

            # Ⅼ
            if !$de || $de == 1 {
                $attachee := Nodify('IntLiteral').new(
                  $*LITERALS.intern-int($nu)
                );
            }

            # ⅔
            else {
                my $LITERALS := $*LITERALS;
                $attachee := Nodify('RatLiteral').new(
                  $LITERALS.intern-rat(
                    $LITERALS.intern-int($nu),
                    $LITERALS.intern-int($de)
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
        make $*LITERALS.intern-int: ~$/, 10, -> {
            $/.panic("'$/' is not a valid number")
        }
    }

    method hexint($/) {
        make $*LITERALS.intern-int: ~$/, 16, -> {
            $/.panic("'$/' is not a valid number")
        }
    }

    method octint($/) {
        make $*LITERALS.intern-int: ~$/, 8, -> {
            $/.panic("'$/' is not a valid number")
        }
    }

    method binint($/) {
        make $*LITERALS.intern-int: ~$/, 2, -> {
            $/.panic("'$/' is not a valid number")
        }
    }

    method integer($/) { make $<VALUE>.ast }

    method signed-integer($/) {
        my $integer := $<integer>.ast;
        make $<sign> eq '-' || $<sign> eq '−'
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
        my $ast;

        if $<bracket> {
            $ast := Nodify('Term','RadixNumber').new:
              :radix($literals.intern-int(~$<radix>)),
              :value($<bracket>.ast),
              :multi-part;
        }
        elsif $<circumfix> {
            $ast := Nodify('Term','RadixNumber').new:
              :radix($literals.intern-int(~$<radix>)),
              :value($<circumfix>.ast);
        }

        else {
            # Check and override $radix if necessary.
            my int $radix := nqp::radix(10, $<radix>, 0, 0)[0];
            $/.typed-panic('X::Syntax::Number::RadixOutOfRange', :$radix)
                unless (2 <= $radix) && ($radix <= 36);

            my $ohradix := ~$<ohradix>;
            if $ohradix {
                $radix := $ohradix eq "0x"
                  ?? 16
                  !! $ohradix eq "0o"
                    ?? 8
                    !! $ohradix eq "0d"
                      ?? 10
                      !! $ohradix eq "0b"
                        ?? 2
                        !! $/.panic("Unknown radix prefix '$ohradix'.");
            }

            # Parse and assemble number.
            my $Int := $literals.int-type;
            my $Num := $literals.num-type;
            my str $intpart  := ~$<intpart>;
            my str $fracpart := ~$<fracpart>;

            my $ipart := nqp::radix_I($radix, $intpart, 0, 0, $Int);
            my $fpart := nqp::radix_I($radix, $fracpart || ".0", 1, 4, $Int);

            my $pos := $ipart[2] < nqp::chars($intpart)
              ?? ($ipart[2] < 0 ?? 0 !! $ipart[2])
              !! $fpart[2] < nqp::chars($fracpart)
                # the -1 dance is due to nqp::radix returning -1 for
                # failure to parse the first char, instead of 0;
                # we return `1` to cover the decimal dot in that case
                ?? ($ipart[2] + ($fpart[2] == -1 ?? 1 !! $fpart[2]))
                !! nqp::null;
            $/.typed-panic('X::Str::Numeric',
              source => $intpart ~ $fracpart,
              reason => "malformed base-$radix number",
              pos    => $pos
            ) unless nqp::isnull($pos);

            my $base := nqp::pow_I(nqp::box_i($radix, $Int), $fpart[1], $Num, $Int);
            $ipart := nqp::mul_I($ipart[0], $base, $Int);
            $ipart := nqp::add_I($ipart, $fpart[0], $Int);
            $fpart := $base;

            my $bpart := $<base> ?? nqp::tonum_I($<base>[0].ast) !! $radix;
            my $epart := $<exp>  ?? nqp::tonum_I($<exp>[0].ast)  !! 0;
            my $scientific := nqp::pow_n($bpart, $epart);
            $ipart := nqp::mul_I($ipart, nqp::fromnum_I($scientific, $Int), $Int);

            $ast := $fpart != 1
              # non-unit fractional part, wants Rat
              ?? Nodify('RatLiteral').new(
                   $literals.intern-decimal($ipart, $fpart)
                 )
              # wants Int
              !! Nodify('IntLiteral').new($ipart);
        }

        self.attach: $/, $ast
    }

    method rational-number($/) {
        my $ast;

        if $<bare-rational-number> -> $rat {
            $ast := $rat.ast;
        }
        else {
            my $nu := super-int-to-Int(~$<super-integer>, ~$<super-sign>);
            my $de := sub-int-to-Int(~$<sub-integer>);
            $ast := Nodify('RatLiteral').new(
              $*LITERALS.intern-rat($nu, $de)
            );
        }

        self.attach: $/, $ast;
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
        make Nodify('VersionLiteral').new(
          $*R.resolve-lexical-constant('Version').compile-time-value.new(
            ~$<vstr>
          )
        ) if $*R;
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

    # Helper sub to codegen matches
    sub setup-match($/, :$samespace) {
        my @adverbs := $<rx-adverbs>.ast // nqp::list;
        @adverbs.push(Nodify('ColonPair','True').new("s")) if $samespace;

        Nodify('QuotedRegex').new:
          :match-immediately,
          body => $<quibble>.ast,
          :@adverbs
    }

    # Handle all m/// cases
    method quote:sym<m>($/)  { self.attach: $/, setup-match($/)             }
    method quote:sym<ms>($/) { self.attach: $/, setup-match($/, :samespace) }

    # Helper sub to codegen substitutions
    sub setup-substitution($/, *%_) {
        my $sibble := $<sibble>;
        Nodify('Substitution').new:
          adverbs     => $<rx-adverbs>.ast,
          pattern     => $sibble<left>.ast,
          infix       => $sibble<infixish>
            ?? $sibble<infixish>.ast
            !! Nodify('Infixish'),
          replacement => $sibble<right>.ast,
          |%_
    }

    # Handle all s/// cases
    method quote:sym<s>($/) {
        self.attach: $/, setup-substitution($/)
    }
    method quote:sym<ss>($/) {
        self.attach: $/, setup-substitution($/, :samespace)
    }
    method quote:sym<S>($/) {
        self.attach: $/, setup-substitution($/, :immutable)
    }
    method quote:sym<Ss>($/) {
        self.attach: $/, setup-substitution($/, :immutable, :samespace)
    }

    # We make a list of the quotepairs to attach them to the regex
    # construct; validation of what is valid takes place in the AST.
    # However, a limited number of them are required for parsing the
    # regex and constructing its AST correctly. Of note, these are
    # s (sigspace, as it controls how whitespce is parsed), m (so we
    # can construct character class ranges correctly), and P5 (Perl5,
    # so we know which regex language to parse). These get special
    # handling.
    my constant SPECIAL-RX-ADVERBS := nqp::hash(
        'ignoremark', 'm',
        'm',          'm',
        'mm',         'm',
        'samemark',   'm',
        's',          's',
        'samespace',  's',
        'sigspace',   's',
        'ss',         's',
        'P5',         'P5',
        'Perl5',      'P5'
    );
    method rx-adverbs($/) {
        my @pairs;
        for $<quotepair> {
            my $ast := $_.ast;
            @pairs.push($ast);

            my $key := SPECIAL-RX-ADVERBS{$ast.key};
            if $key {
                my $value := $ast.simple-compile-time-quote-value;
                nqp::isconcrete($value)
                  ?? (%*RX{$key} := ?$value)
                  !! $_.typed-panic: 'X::Value::Dynamic',
                       what => 'Adverb ' ~ $ast.key;
            }
        }
        make @pairs;
    }

    method quotepair($/) {
        my $key := $*LITERALS.intern-str(~$*KEY);
        self.attach: $/, $<num>
          ?? Nodify('ColonPair','Number').new(
              key   => $key,
              value => Nodify('IntLiteral').new(
                         $*LITERALS.intern-int(~$<num>)
                       )
             )
          !! $<circumfix>
            ?? Nodify('ColonPair','Value').new(
                 key   => $key,
                 value => $<circumfix>.ast
               )
            !! Nodify('ColonPair', $<neg> ?? 'False' !! 'True').new($key);
    }

#-------------------------------------------------------------------------------
# Types

    method type-for-name($/, $base-name) {
        my $type := Nodify('Type','Simple').new(
          $base-name.without-colonpair('_').without-colonpair('D').without-colonpair('U')
        ).to-begin-time($*R, $*CU.context);

        $type := $base-name.has-colonpair('D')
          ?? Nodify('Type','Definedness').new(:base-type($type), :definite).to-begin-time($*R, $*CU.context)
          !! $base-name.has-colonpair('U')
            ?? Nodify('Type','Definedness').new(:base-type($type), :!definite).to-begin-time($*R, $*CU.context)
            !! $type;

        $type := Nodify('Type','Parameterized').new(
          :base-type($type), :args($<arglist>.ast)
        ).to-begin-time($*R, $*CU.context) if $<arglist>;

        $<accept>
          ?? Nodify('Type','Coercion').new(
               :base-type($type), :constraint($<accept>.ast)).to-begin-time($*R, $*CU.context)
          !! $<accept_any>
            ?? Nodify('Type','Coercion').new(:base-type($type)).to-begin-time($*R, $*CU.context)
            !! $type
    }

    method typename($/) {
        my $base-name := $<longname>
          ?? $<longname>.ast
          !! Nodify('Name').from-identifier('::?' ~ $<identifier>);
        for $<colonpair> {
            $base-name.add-colonpair($_.ast);
        }
        my str $longname := ~$<longname>;
        if nqp::eqat($longname, '::', 0) {
            if $<arglist> || $<typename> {
                $/.panic("Cannot put type parameters on a type capture");
            }
            if $<accept> || $<accept_any> {
                $/.panic("Cannot base a coercion type on a type capture");
            }
            if $longname eq '::' {
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

#-------------------------------------------------------------------------------
# Signatures

    method fakesignature($/) {
        self.attach: $/, Nodify('FakeSignature').new: $<signature>.ast
    }

    method signature($/) {
        my @parameters;
        my int $param_idx;
        for $<parameter> {
            my $param := $_.ast;
            my $sep := @*SEPS[$param_idx];
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
            ++$param_idx;
        }
        my $returns;
        if $<typename> {
            $returns := $<typename>.ast;
        }
        elsif $<value> {
            $returns := $<value>.ast;
            unless nqp::istype($returns,Nodify('CompileTimeValue')) {
                $<value>.panic:
                  'Return value after --> may only be a type or a constant';
            }
        }
        else {
            $returns := Nodify('Node');
        }
        if $*OFTYPE {
            $/.typed-sorry('X::Redeclaration', :what('return type for'))
                if $returns;
            $returns := $*OFTYPE.ast;
        }
        my $signature := Nodify('Signature').new(:@parameters, :$returns);
        if $*ON-VARDECLARATION {
            make $signature
        }
        else {
            self.attach: $/, $signature
        }
    }

    method parameter($/) {
        my $parameter := $<param-var> || $<named-param> || $<param-term>;
        $parameter := $parameter ?? $parameter.ast !! Nodify('Parameter').new;

        if $*DEFAULT-RW {
            $parameter.set-bindable;
            $parameter.set-default-rw if $*DEFAULT-RW > 1;
        }

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
            my $post-constraint := $<post-constraint>[0];
            if $post-constraint<EXPR> {
                $parameter.set-where($post-constraint<EXPR>.ast);
            }
            elsif $post-constraint<signature> {
                $parameter.set-sub-signature($post-constraint<signature>.ast);
            }
        }
        # Leave the exact time of Parameter's BEGIN to the signature
        make $parameter;
    }

    method param-var($/) {
        # Work out what kind of thing we're binding into, if any.
        my %args;
        my str $name := ~$<declname>;
        if $name {
            my $dynprag := $*LANG.pragma('dynamic-scope');
            my $forced-dynamic := $dynprag
                ?? $dynprag($name)
                !! 0;
            my $decl := Nodify('ParameterTarget', 'Var').new(
              :$name, :$forced-dynamic
            );
            $/.typed-panic('X::Redeclaration', :symbol($name))
              if $decl.can-be-resolved
              && $*DECLARE-TARGETS
              && $*R.declare-lexical($decl);
            %args<target> := $decl;
        }
        elsif $<signature> {
            %args<sub-signature> := $<signature>.ast;
        }

        # Build the parameter.
        make self.set-declarand($/, Nodify('Parameter').new(|%args));
    }

    method param-term($/) {
        if $<defterm> {
            # Create sigilless target to bind into
            my $ast  := $<defterm>.ast;
            my $decl := Nodify('ParameterTarget','Term').new($ast );
            $/.typed-panic('X::Redeclaration', :symbol($ast.canonicalize))
              if $*DECLARE-TARGETS && $*R.declare-lexical($decl);
            make Nodify('Parameter').new(target => $decl);
        }
        else {
            # Anonymous
            make Nodify('Parameter').new();
        }
    }

    method named-param($/) {
        my $ast;

        # Explicitly specified name to attach.
        if $<name> {
            $ast := ($<named-param> || $<param-var>).ast;
            $ast.add-name(~$<name>);
        }

        # Name comes from the parameter variable.
        else {
            my $param-var := $<param-var>;
            $ast     := $param-var.ast;
            $ast.add-name(~($param-var<name> // ''));
        }
        make $ast;
    }

    method default-value($/) {
        make $<EXPR>.ast;
    }

    method type-constraint($/) {
        self.attach: $/, ($<typename> || $<value>).ast;
    }

    method post-constraint($/) {
        my $target := $<EXPR> || $<signature>;
        make $target.ast if $target;
    }

#-------------------------------------------------------------------------------
# Argument lists and captures

    method args($/) {
        self.attach: $/, (my $target := $<semiarglist> || $<arglist>)
          ?? $target.ast
          !! Nodify('ArgList').new
    }

    method semiarglist($/) {
        if nqp::elems($<arglist>) == 1 {
            self.attach: $/, $<arglist>[0].ast
        }
        else {
            my $R := $*R;
            my $context := $*CU.context;
            my $ast := Nodify('ArgList').new;
            for $<arglist> {
                my $infix  := Nodify('Infix').new(',').to-begin-time($R, $context);
                my $apply  := Nodify('ApplyListInfix').new(:$infix, :operands($_.ast.args)).to-begin-time($R, $context);
                my $stmt   := Nodify('Statement', 'Expression').new(:expression($apply)).to-begin-time($R, $context);
                my $semi   := Nodify('SemiList').new($stmt).to-begin-time($R, $context);
                my $parens := Nodify('Circumfix', 'Parentheses').new($semi).to-begin-time($R, $context);
                $ast.push($parens);
            }
            self.attach: $/, $ast
        }
    }

    method arglist($/) {
        my $ast;
        my $ArgList := Nodify('ArgList');
        my $expr    := $<EXPR>;
        if $expr {
            $ast := $expr.ast;
            $ast := nqp::istype($ast,Nodify('ColonPairs'))
              ?? $ArgList.new(|$ast.colonpairs)
              !! nqp::istype($ast,Nodify('ApplyListInfix'))
                   && nqp::istype($ast.infix,Nodify('Infix'))
                ?? $ast.infix.operator eq ','
                  ?? Nodify('ArgList').from-comma-list($ast)
                  !! $ast.infix.operator eq ':'
                    ?? $ArgList.from-invocant-list($ast)
                    !! $ArgList.new($ast)
                !! $ArgList.new($ast);
        }
        else {
            $ast := $ArgList.new;
        }
        self.attach: $/, $ast;
    }

#-------------------------------------------------------------------------------
# Lexer stuff

    method name($/) {
        if $<morename> {
            my @parts;
            if $<identifier> {
                @parts.push(Nodify('Name','Part','Simple').new(~$<identifier>));
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
            make Nodify('Name','Part','Simple').new(~$<identifier>);
        }
        elsif $<EXPR> {
            my $ast := $<EXPR>.ast;
            $ast.to-begin-time($*R, $*CU.context);
            make Nodify('Name', 'Part', 'Expression').new($ast);
        }
        else {
            make Nodify('Name', 'Part', 'Empty');
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
        my $name := Nodify('Name').from-identifier(~$<identifier>);
        for $<colonpair> {
            $name.add-colonpair($_.ast);
        }
        self.attach: $/, $name;
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
        $config<numbered> := Nodify('IntLiteral').new(1)
          if $<doc-numbered>;
        $config<uri> := Nodify('StrLiteral').new(~$<uri>)
          if $<uri>;

        if $<colonpair> {
            for $<colonpair> -> $/ {
                my $key := ~$<identifier>;
                if $<num> {
                    $config{$key} := Nodify('IntLiteral').new(+$<num>);
                }
                elsif $<coloncircumfix> {  # :bar("foo",42)
                    $config{$key} := $<coloncircumfix>.ast;
                }
                elsif $<var> {             # :$bar
                    $config{$key} := $<var>.ast;
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
        if $<type> {
            my $type := $<type>;
            if $type<level> {
                my $level := ~$type<level>;
                $type := ~$type;
                nqp::substr(
                  $type,0,nqp::chars($type) - nqp::chars($level)
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
        +($<type><level> // '0')
    }

    method doc-block:sym<finish>($/) {
        $*CU.replace-finish-content(~$<finish>) if $*CU;
    }

    method doc-origin($/, $node) {
        self.SET-NODE-ORIGIN: $/, $*SEEN{$/.from} := $node;
    }

    method doc-block:sym<alias>($/) {
        unless $*FROM-SEEN{$/.from}++ {
            my @paragraphs := nqp::list(~$<first>);
            if $<line> {
                for $<line> {
                    nqp::push(@paragraphs,~$_);
                }
            }

            self.doc-origin: $/, Nodify('Doc','Block').from-alias:
              :directive, :margin(~$<margin>), :type<alias>,
              :lemma(~$<lemma>), :@paragraphs
        }
    }

    method doc-block:sym<column>($/) {
        unless $*FROM-SEEN{$/.from}++ {
            self.doc-origin: $/, Nodify('Doc','Block').new:
              :directive, :margin(~$<margin>), :type<column>,
              :config(self.extract-config($/))
        }
    }

    method doc-block:sym<row>($/) {
        unless $*FROM-SEEN{$/.from}++ {
            self.doc-origin: $/, Nodify('Doc','Block').new:
              :directive, :margin(~$<margin>), :type<row>,
              :config(self.extract-config($/))
        }
    }

    method doc-block:sym<place>($/) {
        unless $*FROM-SEEN{$/.from}++ {
            self.doc-origin: $/, Nodify('Doc','Block').new:
              :directive, :margin(~$<margin>), :type<place>,
              :config(self.extract-config($/))
        }
    }

    method doc-block:sym<formula>($/) {
        unless $*FROM-SEEN{$/.from}++ {
            my @paragraphs := nqp::list(~$<formula>);
            self.doc-origin: $/, Nodify('Doc','Block').new:
              :abbreviated, :margin(~$<margin>), :type<formula>,
              :config(self.extract-config($/)), :@paragraphs
        }
    }

    method doc-block:sym<config>($/) {
        unless $*FROM-SEEN{$/.from}++ {
            self.doc-origin: $/, Nodify('Doc','Block').from-config:
              :directive, :margin(~$<margin>), :type<config>,
              :config(self.extract-config($/)), :key(~$<doc-identifier>)
        }
    }

    method doc-block:sym<verbatim>($/) {
        unless $*FROM-SEEN{$/.from}++ {
            my $config := self.extract-config($/);

            my @paragraphs;
            if $<lines> {
                my $text := ~$<lines>;
                nqp::push(@paragraphs,$text) if $text;
            }

            self.doc-origin: $/, Nodify('Doc','Block').from-paragraphs:
              :margin(~$<margin>), :type(~$<type>), :$config, :@paragraphs;
        }
    }

    method doc-block:sym<begin>($/) {
        unless $*FROM-SEEN{$/.from}++ {
            my $SEEN := $*SEEN;

            my $config := self.extract-config($/);
            my $type   := self.extract-type($/);
            my $level  := self.extract-level($/);

            my @paragraphs;
            if $<doc-content> {
                for $<doc-content> {
                    my $from := ~$_.from;
                    if nqp::existskey($SEEN,$from) {
                        nqp::push(@paragraphs,nqp::atkey($SEEN,$from));
                        nqp::deletekey($SEEN,$from);
                    }
                    else {
                        my $text := ~$_;
                        nqp::push(@paragraphs,$text) if $text;
                    }
                }
            }

            self.doc-origin: $/, Nodify('Doc','Block').from-paragraphs:
              :margin(~$<margin>), :$type, :$level, :$config, :@paragraphs;
        }
    }

    method doc-block:sym<for>($/) {
        unless $*FROM-SEEN{$/.from}++ {
            my $config := self.extract-config($/);
            my $type   := self.extract-type($/);
            my $level  := self.extract-level($/);

            my @paragraphs;
            if $<lines> {
                my $text := ~$<lines>;
                nqp::push(@paragraphs,$text) if $text;
            }
            self.doc-origin: $/, Nodify('Doc','Block').from-paragraphs:
              :margin(~$<margin>), :for, :$type, :$level, :$config, :@paragraphs;
        }
    }

    method doc-block:sym<abbreviated>($/) {
        unless $*FROM-SEEN{$/.from}++ {
            my $config := self.extract-config($/);
            my $type   := self.extract-type($/);
            my $level  := self.extract-level($/);

            my @paragraphs;
            my $text := ($<header> ?? $<margin> ~ $<header> !! "")
              ~ ($<lines> ?? ~$<lines> !! "");
            @paragraphs := nqp::list($text) if $text;

            self.doc-origin: $/, Nodify('Doc','Block').from-paragraphs:
              :margin(~$<margin>), :abbreviated, :$type, :$level, :$config,
              :@paragraphs;
        }
    }
}

class Raku::QActions is HLL::Actions does Raku::CommonActions {
    # This overrides NQP during the deprecation period for Unicode 1 names
    # not covered by Alias Names
    method charname-panic($/) { $/.panic("Unrecognized character name [$/]") }

    method charname($/) {
        my $codepoint := $<integer>
          ?? nqp::chr($<integer>.made)
          !! nqp::strfromname(~$/);
        $codepoint := self.deprecated-charnames($/) if $codepoint eq '';
        make $codepoint;
    }

    # Check for deprecated charnames: worry if a deprecated one is found
    # and return the appropriate one.  Otherwise panic
    method deprecated-charnames($/) {
        my str $name  := ~$/;
        my str $worry := $name eq "LINE FEED (LF)"
          ?? "LINE FEED, NEW LINE, END OF LINE, LF, NL or EOL"
          !! $name eq "FORM FEED (FF)"
            ?? "FORM FEED or FF"
            !! $name eq "CARRIAGE RETURN (CR)"
              ?? "CARRIAGE RETURN or CR"
              !! $name eq "NEXT LINE (NEL)"
                ?? "NEXT LINE or NEL"
                !! "";

        if $worry {
            $/.worry:
"Deprecated character name [$name] in lookup of Unicode
character by name.  Unicode 1 names are deprecated.
Please use $worry.";
            nqp::strfromname(
              nqp::join(" ",nqp::slice(nqp::split(" ",$name),0,1))
            )
        }
        else {
            self.charname-panic($/);
        }
    }

    method nibbler($/) {
        my @segments;
        my str $sofar  := '';
        my $LITERALS   := $*LITERALS;
        my $StrLiteral := Nodify('StrLiteral');

        for @*NIBBLES {
            if nqp::istype($_, NQPMatch) {
                my $ast := $_.ast;

                # a string was "made" ?
                if nqp::isstr($ast) {
                    $sofar := $sofar ~ $ast;
                }

                # a real AST, but collected string so far
                elsif $sofar {
                    @segments.push:
                      $StrLiteral.new($LITERALS.intern-str($sofar));
                    $sofar := '';
                    @segments.push($ast);
                }

                # a real AST without string
                else {
                    @segments.push($ast);
                }
            }

            # assume string or something stringifiable
            else {
                $sofar := $sofar ~ $_;
            }
        }

        # make sure we have at least an empty string in segments
        @segments.push(
          $StrLiteral.new($LITERALS.intern-str($sofar))
        ) if $sofar || !@segments;

        self.attach: $/, Nodify(
          nqp::can($/,'herelang') ?? 'Heredoc' !! 'QuotedString'
        ).new(
          segments   => @segments,
          processors => nqp::can($/,'postprocessors') ?? $/.postprocessors !! []
        );
    }

    method backslash:sym<qq>($/) { self.attach: $/, $<quote>.ast; }
    method backslash:sym<\\>($/) { make '\\' }
    method backslash:delim ($/) { make $<text>.Str }
    method backslash:sym<miscq>($/) { make '\\' ~ ~$/; }
    method backslash:sym<misc>($/) { make ~$/; }
    method backslash:sym<c>($/) { make $<charspec>.ast }

    method backslash:sym<0>($/) { make nqp::chr( 0) }
    method backslash:sym<a>($/) { make nqp::chr( 7) }
    method backslash:sym<b>($/) { make nqp::chr( 8) }
    method backslash:sym<e>($/) { make nqp::chr(27) }
    method backslash:sym<f>($/) { make nqp::chr(12) }

    sub heredoc-whitespace($/, str $string) {
        nqp::can($/,'parsing-heredoc')
          # In heredocs, we spit out a QAST::SVal here to prevent newlines
          # being taken literally and affecting the dedent.
          ?? Nodify('Heredoc','InterpolatedWhiteSpace').new(
               $*LITERALS.intern-str($string)
             )
          !! $string
    }
    method backslash:sym<n>($/) {
        make heredoc-whitespace(
          $/, $*R.resolve-lexical-constant('$?NL').compile-time-value
        );
    }
    method backslash:sym<r>($/)  { make heredoc-whitespace($/, "\r");   }
    method backslash:sym<rn>($/) { make heredoc-whitespace($/, "\r\n"); }
    method backslash:sym<t>($/)  { make heredoc-whitespace($/, "\t");   }

    method backslash:sym<o>($/) {
        make self.ints_to_string($<octint> || $<octints><octint>);
    }
    method backslash:sym<x>($/) {
        make self.ints_to_string($<hexint> || $<hexints><hexint>);
    }

    method escape:sym<#>($/) { make ''; }

    method escape:sym<\\>($/)  { make $<item>.ast }
    method escape:sym<$>($/)   { self.attach: $/, $<EXPR>.ast  }
    method escape:sym<@>($/)   { self.attach: $/, $<EXPR>.ast  }
    method escape:sym<%>($/)   { self.attach: $/, $<EXPR>.ast  }
    method escape:sym<&>($/)   { self.attach: $/, $<EXPR>.ast  }
    method escape:sym<{ }>($/) { self.attach: $/, $<block>.ast }

    sub qwatom($node) { Nodify('QuoteWordsAtom').new($node.ast) }
    method escape:sym<'>($/)         { self.attach: $/, qwatom($<quote>)     }
    method escape:sym<colonpair>($/) { self.attach: $/, qwatom($<colonpair>) }
}

#-------------------------------------------------------------------------------

class Raku::RegexActions is HLL::Actions does Raku::CommonActions {

    method nibbler($/) {
        self.attach: $/, $<termseq>.ast;
    }

    method termseq($/) {
        self.attach: $/, $<termaltseq>.ast;
    }

    # helper method to handle regex sequences
    method handle-regex-seq($/, str $key, str $class) {
        my $ast;
        my @parts := nqp::atkey($/,$key);

        if nqp::elems(@parts) == 1 {
            $ast := @parts[0].ast;
        }
        else {
            my @branches;
            for @parts {
                @branches.push($_.ast);
            }
            $ast := Nodify('Regex',$class).new(|@branches);
        }

        self.attach: $/, $ast
    }

    method termaltseq($/) {
        self.handle-regex-seq($/, 'termconjseq', 'SequentialAlternation')
    }
    method termconjseq($/) {
        self.handle-regex-seq($/, 'termalt', 'SequentialConjunction')
    }
    method termalt($/) {
        self.handle-regex-seq($/, 'termconj', 'Alternation')
    }
    method termconj($/) {
        self.handle-regex-seq($/, 'termish', 'Conjunction')
    }
    method termish($/) {
        self.handle-regex-seq($/, 'noun', 'Sequence')
    }

    method quantified_atom($/) {
        my $atom       := self.wrap-whitespace($<sigmaybe>, $<atom>.ast);
        my $quantifier := $<quantifier>;

        # Set up separator info
        my %separator;
        my $separator := $<separator>;
        if $separator {
            my str $type := ~$separator<septype>;
            $/.panic(
              "'$type' may only be used immediately following a quantifier"
            ) unless $quantifier;

            %separator<separator>          := $separator.ast;
            %separator<trailing-separator> := 1 if $type eq '%%';
        }

        self.attach: $/, self.wrap-whitespace($<sigfinal>, $quantifier
          ?? Nodify('Regex','QuantifiedAtom').new(
               :$atom, :quantifier($quantifier.ast), |%separator
             )
          !! $<backmod>
            ?? Nodify('Regex','BacktrackModifiedAtom').new(
                 :$atom, :backtrack($<backmod>.ast)
               )
            !! $atom
        );
    }

    method wrap-whitespace($cond, $ast) {
        nqp::chars(~$cond)
          && nqp::istype($ast,Nodify('Regex','Term'))
          && $ast.whitespace-wrappable
          ?? Nodify('Regex', 'WithWhitespace').new($ast)
          !! $ast
    }

    method atom($/) {
        self.attach: $/, (my $metachar := $<metachar>)
          ?? $metachar.ast // Nodify('Regex') # We'll error out later if no real AST
          !! Nodify('Regex','Literal').new(~$/);
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
        my $ast;

        my $backtrack:= $<backmod>.ast;
        if $<codeblock> {
            $ast := Nodify('Regex','Quantifier','BlockRange').new(
              block     => $<codeblock>.ast,
              backtrack => $backtrack
            );
        }
        else {
            my $LITERALS := $*LITERALS;
            my $min := $<min>
              ?? $LITERALS.build-int(~$<min>, 10)
              !! $LITERALS.int-type;

            my $max := $<max>;
            $max := !$max
              ?? $min
              !! $max eq '*'
                ?? $LITERALS.int-type
                !! $LITERALS.build-int(~$max, 10);

            $ast := Nodify('Regex','Quantifier','Range').new(
              excludes-min => $<from> eq '^',
              min          => $min,
              max          => $max,
              excludes-max => $<upto> eq '^',
              backtrack    => $backtrack
            );
        }

        self.attach: $/, $ast
    }

    method backmod($/) {
        my str $backmod := ~$/;
        my str $class   := $backmod eq ':'
          ?? 'Ratchet'
          !! $backmod eq ':?' || $backmod eq '?'
            ?? 'Frugal'
            !! $backmod eq ':!' || $backmod eq '!'
              ?? 'Greedy'
              !! '';

        self.attach: $/, $class
          ?? Nodify('Regex','Backtrack',$class)
          !! Nodify('Regex','Backtrack')
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
        my constant CLASS := nqp::hash(
          'i',          'IgnoreCase',
          'ignorecase', 'IgnoreCase',
          'm',          'IgnoreMark',
          'ignoremark', 'IgnoreMark',
          'r',          'Ratchet',
          'ratchet',    'Ratchet',
          's',          'Sigspace',
          'sigspace',   'Sigspace'
        );
        my str $modifier := $*MODIFIER;
        if CLASS{$modifier} -> $class {
            self.attach: $/, Nodify('Regex','InternalModifier',$class).new(
              modifier => $modifier, negated => $*NEGATED
            );
        }
        else {
            $/.typed-panic: 'X::Syntax::Regex::UnrecognizedModifier',
              modifier => $modifier;
        }
    }

    method metachar:sym<dba>($/) { Nil }

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
        self.attach: $/, $<quantified_atom>
          ?? Nodify('Regex','NamedCapture').new(
               name  => ~($<name> || $<pos>),
               array => ?$<wantarray>,
               regex => $<quantified_atom>[0].ast
             )
          !! $<pos>
            ?? Nodify('Regex','BackReference','Positional').new(+$<pos>)
            !! Nodify('Regex','BackReference','Named').new(~$<name>);
    }

    method metachar:sym<rakvar>($/) {
        if $<var><sigil> eq '%' {
            $<var>.typed-panic('X::Syntax::Reserved', :reserved('use of hash variables in regexes'))
        }
        self.attach: $/, Nodify('Regex','Interpolation').new(
          :var($<var>.ast), :sequential(?$*SEQ)
        );
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
        self.attach: $/, Nodify('Regex','Literal').new(~$/)
    }

    method assertion:sym<?>($/) {
        self.attach: $/, $<assertion>
          ?? Nodify('Regex','Assertion','Lookahead').new(
               assertion => $<assertion>.ast
            )
          !! Nodify('Regex','Assertion','Pass').new
    }

    method assertion:sym<!>($/) {
        self.attach: $/, $<assertion>
          ?? Nodify('Regex','Assertion','Lookahead').new(
               :assertion($<assertion>.ast), :negated
             )
          !! Nodify('Regex', 'Assertion', 'Fail').new
    }

    method assertion:sym<method>($/) {
        my $ast := $<assertion>.ast;
        if nqp::can($ast,'set-capturing') {
            $ast.set-capturing(0);
        }
        self.attach: $/, $ast;
    }

    method assertion:sym<name>($/) {
        my $longname := $<longname>;
        my $name     := $longname.ast;

        self.attach: $/, $<assertion>
          ?? Nodify('Regex','Assertion','Alias').new(
               :name(~$longname), :assertion($<assertion>.ast)
             )
          !! $<arglist>
            ?? Nodify('Regex','Assertion','Named','Args').new(
                 :$name, :capturing, :args($<arglist>.ast)
               )
            !! $<nibbler>
              ?? Nodify('Regex','Assertion','Named','RegexArg').new(
                   :$name, :capturing, :regex-arg($<nibbler>.ast)
                 )
              !! Nodify('Regex','Assertion','Named').new(
                   :$name, :capturing
                 );
    }

    method assertion:sym<{ }>($/) {
        self.attach: $/, Nodify('Regex','Assertion','InterpolatedBlock').new:
          :block($<codeblock>.ast), :sequential(?$*SEQ);
    }

    method assertion:sym<?{ }>($/) {
        self.attach: $/, Nodify('Regex','Assertion','PredicateBlock').new:
          :block($<codeblock>.ast);
    }

    method assertion:sym<!{ }>($/) {
        self.attach: $/, Nodify('Regex', 'Assertion','PredicateBlock').new:
          :negated, :block($<codeblock>.ast);
    }

    method assertion:sym<var>($/) {
        if $<call> {
            my $node := Nodify('Regex','Assertion','Callable');
            self.attach: $/, $<arglist>
              ?? $node.new(:callee($<call>.ast), :args($<arglist>.ast))
              !! $node.new(:callee($<call>.ast));
        }
        else {
            self.attach: $/, Nodify('Regex','Assertion','InterpolatedVar').new:
              :var($<var>.ast), :sequential(?$*SEQ);
        }
    }

    method assertion:sym<[>($/) {
        my @elems := $<cclass_elem>;
        my @asts;
        my int $i;
        my int $n := nqp::elems(@elems);
        while $i < $n {
            my $sign := @elems[$i]<sign>;
            if $i > 0 && $sign eq '' {
                $sign."!clear_highwater"();
                $sign.panic('Missing + or - between character class elements')
            }
            @asts.push(@elems[$i].ast);
            ++$i;
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
        my $ast;

        my int $negated := $<sign> eq '-';
        if $<name> {
            $ast := Nodify('Regex','CharClassElement','Rule').new(
              :name(~$<name>), :$negated
            );
        }
        elsif $<identifier> {
            $ast := Nodify('Regex','CharClassElement','Property').new(
              property  => ~$<identifier>,
              inverted  => $<invert> eq '!',
              predicate => $<coloncircumfix>
                            ?? $<coloncircumfix>.ast
                            !! Nodify('Expression'),
              negated   => $negated
            );
        }
        else {
            my @elements;
            for $<charspec> {
                my $node := $_[0];
                @elements.push: $_[1]
                  ?? Nodify('Regex','CharClassEnumerationElement','Range').new(
                       from => extract-endpoint($node),
                       to   => extract-endpoint($_[1][0])
                     )
                  !! $node<cclass_backslash>
                    ?? $node<cclass_backslash>.ast
                    !! Nodify('Regex','CharClassEnumerationElement','Character').new(
                         ~$node
                       )
            }
            $ast := Nodify('Regex','CharClassElement','Enumeration').new(
              :@elements, :$negated
            );
        }

        self.attach: $/, $ast
    }

    sub extract-endpoint($/) {
        my str $chr := $<cclass_backslash>
          ?? (my $end := $<cclass_backslash>.ast.range-endpoint)
            ?? $end
            !! $/.panic("Illegal range endpoint: " ~ $/)
          !! ~$/;
        %*RX<m>
          ?? nqp::ordbaseat($chr, 0)
          !! non-synthetic-ord($/, $chr)
    }

    sub non-synthetic-ord($/, $chr) {
        nqp::chr(my int $ord := nqp::ord($chr)) eq $chr
          ?? $ord
          !! $/.panic(
               "Cannot use $chr as a range endpoint, as it is not a single codepoint"
             )
    }

    method cclass_backslash:sym<s>($/) {
        self.backslash:sym<s>($/)
    }

    method cclass_backslash:sym<b>($/) {
        self.attach: $/,
          Nodify('Regex','CharClass','BackSpace').new(negated => $<sym> le 'Z')
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
          Nodify('Regex','CharClassEnumerationElement','Character').new(~$/)
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
        self.attach: $/, Nodify('Regex','Assertion','InterpolatedBlock').new:
          :block($<codeblock>.ast), :sequential(?$*SEQ);
    }

    method p5metachar:sym<var>($/) {
        self.attach: $/, Nodify('Regex','Interpolation').new:
          :var($<var>.ast), :sequential(?$*SEQ);
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
