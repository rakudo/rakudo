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

sub nqp($prefix, $file, $stage, :$source=$prefix ~ '/' ~ $file ~ '.nqp', :$deps=[]) {
    my $path := stage_path($stage);
    my $mbc := $path ~ $file ~ '.moarvm';

    my $installed_pbc := 'gen/moar/stage2/' ~ $file ~ '.moarvm';

    nqp::unshift($deps, $source);

    rule($mbc, nqp::join(' ', $deps),
        make_parents($mbc),
        "\$(JS_NQP) --module-path=\$(JS_STAGE1) --target=mbc --output=$mbc $source",
        # HACK - workaround for not being able to supply multiple directories to --module-path
        make_parents($installed_pbc),
        "\$(CP) $mbc $installed_pbc"
    );
}

sub deps($target, *@deps) {
    say("$target : {nqp::join(' ',@deps)}");
}

my $build_dir := 'gen/js/';
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

say("js-all: $ModuleLoader-nqp\n");

say("js-clean:\n\t\$(RM_F) $ModuleLoader-nqp");

