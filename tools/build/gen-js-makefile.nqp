# vi: filetype=perl6:
sub comment($comment) {
    say("# $comment");
}
sub constant($name, $value) {
    say("$name = $value");
}

sub nfp ($file) {
    if $file ~~ /\// {
        return '@nfp(' ~ $file ~ ')@'
    }
    $file
}

sub stage_path($stage) {
    '$(JS_STAGE' ~ $stage ~ ')@slash@';
}

sub make_parents($path) {
    my $parts := nqp::split("/",$path);
    nqp::pop($parts);
    nqp::elems($parts) ?? '$(MKPATH) ' ~ nfp(nqp::join('/',$parts)) !! '';
}

sub rule($target, $source, *@actions) {
    my $rule := "{nfp($target)}: {nfp($source)}\n";
    for @actions -> $action {
        if $rule ne '' {
            $rule := $rule ~ "\t$action\n";
        }
    }
    say($rule);
    $target;
}

constant('JS_BLIB', '@js_blib@');
constant('JS_BUILD_DIR', '@js_build_dir@');
constant('JS_NQP', '@js_nqp@');
constant('JS_RUNNER', '@perl6_js_runner@');

constant('JS_RUNTIME', '@nqp::libdir@' ~ nfp('/nqp-js-on-js/node_modules/nqp-runtime'));
constant('JS_FLAGS', '--nqp-runtime $(JS_RUNTIME) --perl6-runtime @perl6_runtime@ --libpath "@perl6_lowlevel_libs@|||@nqp::libdir@' ~ nfp('/nqp-js-on-js/') ~'"');


my @produced;

sub nqp($file, $output, :$deps=[], :$execname, :$shebang) {
    @produced.push($output);
    nqp::unshift($deps, $file);
    my $options := $execname ?? "--execname $execname" !! "";
    rule($output, nqp::join(' ', $deps),
        make_parents($output),
        "\$(JS_NQP) \$(JS_FLAGS) $options --substagestats --stagestats --target=js --source-map {$shebang ?? '--shebang' !! ''} --output={nfp($output)} {nfp($file)}",
    );
}

sub deps($target, *@deps) {
    say("{nfp($target)} : {nfp(nqp::join(' ',@deps))}");
}

#my $build_dir := 'gen/js';
my $build_dir := '$(JS_BUILD_DIR)';

#my $blib := 'node_modules';
my $blib := '$(JS_BLIB)';

# TODO is the version regenerated as often as it should
sub combine(:$sources, :$file) {

    my $target := $build_dir ~ "/" ~ $file;

    @produced.push($target);

    rule($target, $sources,
        make_parents($target),
        "\$(JS_NQP) \@script(gen-cat.nqp)@ js {nfp($sources)} > {nfp($target)}"
    );
}

my $ModuleLoader-nqp := combine(:sources("src/vm/js/ModuleLoaderVMConfig.nqp src/Perl6/ModuleLoader.nqp"), :file<ModuleLoader.nqp>);


my $Perl6-ModuleLoader := nqp($ModuleLoader-nqp, "$blib/Perl6-ModuleLoader.js");
my $Perl6-Ops := nqp('src/vm/js/Perl6/Ops.nqp', "$blib/Perl6-Ops.js");
my $Perl6-Pod := nqp('src/Perl6/Pod.nqp', "$blib/Perl6-Pod.js");
my $Perl6-World := nqp('src/Perl6/World.nqp', "$blib/Perl6-World.js", :deps([$Perl6-Ops, $Perl6-Pod, $Perl6-ModuleLoader]));

my $Actions-nqp := combine(:sources("src/Perl6/Actions.nqp"), :file<Perl6-Actions.nqp>);
my $Perl6-Actions := nqp($Actions-nqp, "$blib/Perl6-Actions.js", :deps([$Perl6-Ops, $Perl6-World]));

my $Perl6-Grammar := nqp('src/Perl6/Grammar.nqp', "$blib/Perl6-Grammar.js", :deps([$Perl6-World, $Perl6-Actions, $Perl6-Pod]));

my $Optimizer-nqp := combine(:sources("src/Perl6/Optimizer.nqp"), :file<Perl6-Optimizer.nqp>);

my $Perl6-Optimizer := nqp($Optimizer-nqp, "$blib/Perl6-Optimizer.js", :deps([$Perl6-Ops]));

my $Perl6-Compiler := nqp('src/Perl6/Compiler.nqp', "$blib/Perl6-Compiler.js", :deps([$Perl6-Optimizer]));

my $main-version := $build_dir ~ '/main-version.nqp';
@produced.push($main-version);

# TODO - generate a new version on changes
rule($main-version, '', "\$(CONFIGURE) --expand main-version --out {nfp($main-version)}");

my $main-nqp := combine(:sources("src/main.nqp $main-version"), :file<main.nqp>);

my $Perl6-main := nqp($main-nqp, 'rakudo.js', :execname('$(JS_RUNNER)'), :deps([$Perl6-Grammar, $Perl6-Actions, $Perl6-Compiler, $Perl6-Pod]), :shebang);

my $load-compiler := nqp('src/vm/js/load-compiler.nqp', "$blib/load-compiler.js", :deps([$Perl6-Grammar, $Perl6-Actions, $Perl6-Compiler, $Perl6-Pod]));

rule('$(JS_RUNNER)', '', '$(PERL5) @script(create-js-runner.pl)@');

my $Metamodel-combined := $build_dir ~ "/Metamodel.nqp";
rule($Metamodel-combined, '$(COMMON_BOOTSTRAP_SOURCES)',
    "\$(JS_NQP) \@script(gen-cat.nqp)@ js -f \@template(common_bootstrap_sources)@ > {nfp($Metamodel-combined)}"
);
@produced.push($Metamodel-combined);

my $Bootstrap-combined := combine(:sources('$(BOOTSTRAP_SOURCES)'), :file<Perl6-BOOTSTRAP.nqp>);

my $CORE-combined := $build_dir ~ "/CORE.setting";
rule($CORE-combined, '@js_core_sources@',
    '@echo "The following step can take a very long time, please be patient."',
    "\$(JS_NQP) \@script(gen-cat.nqp)@ js  -f \@ctx_template(core_sources)@ > {nfp($CORE-combined)}"

);

say('@for_specs(');
my $CORE-spec-combined := $build_dir ~ "/CORE.@lcspec@.setting";
rule($CORE-spec-combined, '@ctx_template(js_core_sources)@',
    '@echo "The following step can take a very long time, please be patient."',
    "\$(JS_NQP) \@script(gen-cat.nqp)@ js  -f \@ctx_template(js_core_sources)@ > {nfp($CORE-spec-combined)}"
);
say("\n)@");

my $Perl6-Metamodel := nqp($Metamodel-combined, "$blib/Perl6-Metamodel.js",  :deps([$Perl6-Ops]));

my $Perl6-Bootstrap := nqp($Bootstrap-combined, "$blib/Perl6-BOOTSTRAP.js",  :deps([$Perl6-Metamodel]));

my $CORE := "$blib/CORE.setting.js";
rule($CORE, "$CORE-combined rakudo.js $Perl6-Bootstrap",
    "node --max-old-space-size=8192 rakudo.js \$(JS_FLAGS) --source-map --target=js --setting=NULL --output={nfp('node_modules/CORE.setting.js')} {nfp($CORE-combined)}"
);

say('@for_specs(');
my $CORE-spec := "$blib/CORE.@lcspec@.setting.js";
rule($CORE-spec, "$CORE-spec-combined rakudo.js $Perl6-Bootstrap $CORE",
    "node --max-old-space-size=8192 rakudo.js \$(JS_FLAGS) --source-map --target=js --setting=NULL.\@lcspec@ --output={nfp($CORE-spec)} {nfp($CORE-spec-combined)}"
);
say("\n)@");

say("js-all: check_nqp_version " ~ nfp("$ModuleLoader-nqp $Perl6-Grammar $Perl6-Actions $Perl6-Compiler $Perl6-Pod $Perl6-main $Perl6-Bootstrap $CORE \@for_specs($CORE-spec )@\$(JS_RUNNER) $load-compiler\n"));

say("js-clean:\n\t\$(RM_F) " ~ nfp("$ModuleLoader-nqp rakudo.js $CORE $CORE-combined {nqp::join(' ', @produced)}"));

say("js-lint:
	gjslint --strict --max_line_length=200 --nojsdoc {nfp('src/vm/js/perl6-runtime/*.js')}");


rule('js-testable', 'js-all spectest_checkout spectest_update');
rule('js-spectest', 'js-testable', '$(PERL5) t/harness5 --fudge --js --keep-exit-code --tests-from-file=' ~ nfp('t/spectest.js.data'));


rule('check_nqp_version',
    '@script(check-nqp-version.pl)@',
     '$(PERL5) @script(check-nqp-version.pl)@ $(JS_NQP)');

rule('js-install', 'js-all', '@echo "Installing the js backend is not yet implemented."');

# Stub
say("js-runner-default:");
