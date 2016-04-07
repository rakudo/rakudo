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
    '$(MKPATH) ' ~ nqp::join('/',$parts);
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
    rule($output, nqp::join(' ', $deps),
        make_parents($output),
        "./nqp-js --target=js --module-path blib --output=$output --encoding=utf8 $file",
    );
}

sub deps($target, *@deps) {
    say("$target : {nqp::join(' ',@deps)}");
}

my $build_dir := 'gen/js/';

my $blib := 'node_modules';

# TODO is the version regenerated as often as it should
sub combine(:$sources, :$file) {

    my $target := $build_dir ~ $file;

    rule($target, $sources,
        make_parents($target),
        "nqp-m tools/build/gen-cat.nqp js $sources > $target"
    ); 
}


#$(PERL6_ML_MOAR): src/Perl6/ModuleLoader.nqp src/vm/moar/ModuleLoaderVMConfig.nqp
#	$(M_NQP) $(M_GEN_CAT) src/vm/moar/ModuleLoaderVMConfig.nqp src/Perl6/ModuleLoader.nqp > $(M_BUILD_DIR)/m-ModuleLoader.nqp
#	$(M_NQP) --target=mbc --output=$(PERL6_ML_MOAR) --encoding=utf8 \
#	    $(M_BUILD_DIR)/m-ModuleLoader.nqp

my $ModuleLoader-nqp := combine(:sources("src/vm/js/ModuleLoaderVMConfig.nqp src/Perl6/ModuleLoader.nqp"), :file<js-ModuleLoader.nqp>);


my $Perl6-ModuleLoader := nqp($ModuleLoader-nqp, "$blib/Perl6/ModuleLoader.js");
my $Perl6-Ops := nqp('src/vm/js/Perl6/Ops.nqp', "$blib/Perl6/Ops.js");
my $Perl6-Pod := nqp('src/Perl6/Pod.nqp', "$blib/Perl6/Pod.js");
my $Perl6-World := nqp('src/Perl6/World.nqp', "blib/Perl6/World.js", :deps([$Perl6-Ops, $Perl6-Pod, $Perl6-ModuleLoader]));
my $Perl6-Actions := nqp('src/Perl6/Actions.nqp', "$blib/Perl6/Action.js", :deps([$Perl6-Ops, $Perl6-World]));
my $Perl6-Grammar := nqp('src/Perl6/Grammar.nqp', "$blib/Perl6/Grammar.js", :deps([$Perl6-World, $Perl6-Actions, $Perl6-Pod]));

my $Optimizer-nqp := combine(:sources("src/Perl6/Optimizer.nqp"), :file<m-Perl6-Optimizer.nqp>);

my $Perl6-Optimizer := nqp($Optimizer-nqp, "$blib/Perl6/Optimizer.js", :deps([$Perl6-Ops]));

my $Perl6-Compiler := nqp('src/Perl6/Compiler.nqp', "$blib/Perl6/Compiler.js");

say("js-all: $ModuleLoader-nqp $Perl6-Ops $Perl6-Pod $Perl6-World\n");

say("js-clean:\n\t\$(RM_F) $ModuleLoader-nqp");

# Stub
say("js-runner-default:")
