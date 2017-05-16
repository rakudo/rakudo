# vi: filetype=perl6:
sub comment($comment) {
    say("# $comment");
}
sub constant($name, $value) {
    say("$name = $value");
}

sub stage_path($stage) {
    '$(JS_STAGE' ~ $stage ~ ')/';
}

sub make_parents($path) {
    my $parts := nqp::split("/",$path);
    nqp::pop($parts);
    nqp::elems($parts) ?? '$(MKPATH) ' ~ nqp::join('/',$parts) !! '';
}

sub rule($target, $source, *@actions) {
    my $rule := "$target: $source\n";
    for @actions -> $action {
        if $rule ne '' {
            $rule := $rule ~ "\t$action\n";
        }
    }
    say($rule);
    $target;
}

sub nqp($file, $output, :$deps=[]) {
    nqp::unshift($deps, $file);
    rule($output, nqp::join(' ', $deps),
        make_parents($output),
        "./nqp-js --substagestats --stagestats --target=js --output=$output --encoding=utf8 $file",
    );
}

sub deps($target, *@deps) {
    say("$target : {nqp::join(' ',@deps)}");
}

my $build_dir := 'gen/js';

my $blib := 'node_modules';

# TODO is the version regenerated as often as it should
sub combine(:$sources, :$file) {

    my $target := $build_dir ~ "/" ~ $file;

    rule($target, $sources,
        make_parents($target),
        "./nqp-js tools/build/gen-cat.nqp js $sources > $target"
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

# TODO - generate a new version on changes
rule($main-version, '', "\$(PERL5) tools/build/gen-version.pl > $main-version");

my $main-nqp := combine(:sources("src/main.nqp $main-version"), :file<main.nqp>);

my $Perl6-main := nqp($main-nqp, 'rakudo.js', :deps([$Perl6-Grammar, $Perl6-Actions, $Perl6-Compiler, $Perl6-Pod]));


my $Metamodel-combined := $build_dir ~ "/Metamodel.nqp";
rule($Metamodel-combined, '$(COMMON_BOOTSTRAP_SOURCES)',
    "./nqp-js tools/build/gen-cat.nqp js -f tools/build/common_bootstrap_sources > $Metamodel-combined"
); 

my $Bootstrap-combined := combine(:sources('$(BOOTSTRAP_SOURCES)'), :file<Perl6-BOOTSTRAP.nqp>);

my $CORE-combined := $build_dir ~ "/CORE.setting";
rule($CORE-combined, '@js_core_sources@',
    '@echo "The following step can take a very long time, please be patient."',
    "./nqp-js tools/build/gen-cat.nqp js  -f tools/build/js_core_sources > $CORE-combined"

);

my $CORE := "$blib/CORE.setting.js";
rule($CORE, "$CORE-combined rakudo.js",
    "node --max-old-space-size=8192 rakudo.js --target=js --setting=NULL --output=node_modules/CORE.setting.js $CORE-combined"
);

my $Perl6-Metamodel := nqp($Metamodel-combined, "$blib/Perl6-Metamodel.js",  :deps([$Perl6-Ops]));

my $Perl6-Bootstrap := nqp($Bootstrap-combined, "$blib/Perl6-BOOTSTRAP.js",  :deps([$Perl6-Metamodel]));

say("js-all: $ModuleLoader-nqp $Perl6-Grammar $Perl6-Actions $Perl6-Compiler $Perl6-Pod $Perl6-main $Perl6-Bootstrap $CORE\n");


say("js-clean:\n\t\$(RM_F) $ModuleLoader-nqp");

say("js-lint:
	gjslint --strict --max_line_length=200 --nojsdoc src/vm/js/perl6-runtime/*.js");

# Stub
say("js-runner-default:")
