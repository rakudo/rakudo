class CompUnit {
    has Lock     $!lock;
    has Str      $.from;
    has Str      $.name;
    has Str      $.extension;
    has Str      $.precomp-ext;
    has IO::Path $.path;
    has Str      $!WHICH;
    has Bool     $.has-source;
    has Bool     $.has-precomp;
    has Bool     $.is-loaded;
    has Mu       $!module_ctx;

    my Lock $global = Lock.new;
    my $default-from = 'Perl6';
    my %instances;

    method new(CompUnit:U:
      $path,
      :$name is copy,
      :$extension is copy,
      :$from = $default-from,
      :$has-source is copy,
      :$has-precomp is copy,
    ) {

        # set name / extension if not already given
        if !$name or !$extension.defined {
            my IO::Spec $SPEC := $*SPEC;
            $name      ||= $SPEC.basename($path);
            $extension ||= $SPEC.extension($name);
        }

        # sanity test
        my $precomp-ext = $*VM.precomp-ext;
        $has-source  //= ?$path.IO.f;
        $has-precomp //= ?"$path.$precomp-ext".IO.f;
        return Nil unless $has-source or $has-precomp;

        $global.protect( { %instances{$path} //= self.bless(
          :path(IO::Path.new-from-absolute-path($path)),
          :lock(Lock.new),
          :$name,
          :$extension,
          :$precomp-ext,
          :$from,
          :$has-source,
          :$has-precomp,
          :!is-loaded,
        ) } );
    }

    multi method WHICH(CompUnit:D:) { $!WHICH //= "{self.^name}|$!path.abspath()" }
    multi method Str(CompUnit:D: --> Str)  { $!path.abspath }
    multi method gist(CompUnit:D: --> Str) { "{self.name}:{$!path.abspath}" }

    method key(CompUnit:D: --> Str) {
        $!has-precomp ?? $!precomp-ext !! $!extension;
    }

    method precomp-path(CompUnit:D: --> Str) { "$!path.$!precomp-ext" }

    method precomp(CompUnit:D:
      $out  = self.precomp-path,
      :$INC = @*INC,
      :$force,
    ) {

        my $io = $out.IO;
        die "Cannot pre-compile over a newer existing file: $out"
          if $io.e && !$force && $io.modified > $!path.modified;

        my Mu $opts := nqp::atkey(%*COMPILING, '%?OPTIONS');
        my $lle = !nqp::isnull($opts) && !nqp::isnull(nqp::atkey($opts, 'll-exception'))
          ?? ' --ll-exception'
          !! '';
        %*ENV<RAKUDO_PRECOMP_WITH> = CREATE-INCLUDE-SPECS(@$INC);

RAKUDO_MODULE_DEBUG("Precomping with %*ENV<RAKUDO_PRECOMP_WITH>")
  if $?RAKUDO_MODULE_DEBUG;

        my $cmd = "$*EXECUTABLE$lle --target={$*VM.precomp-target} --output=$out $!path";
        my $proc = shell("$cmd 2>&1", :out, :!chomp);
        %*ENV<RAKUDO_PRECOMP_WITH>:delete;

        my $result = '';
        $result ~= $_ for $proc.out.lines;
        $proc.out.close;
        if $proc.status -> $status {  # something wrong
            $result ~= "Return status $status\n";
            fail $result if $result;
        }
        note $result if $result;


        $!has-precomp = True if $out eq self.precomp-path;
        True;
    }

    proto method load(CompUnit:D: |) { * }
    multi method load(CompUnit:D: ) { self.load(Any) }
    multi method load(CompUnit:D: \GLOBALish is rw, :$line) {
        $global.protect( {
            RAKUDO_MODULE_DEBUG("going to load $!name") if $?RAKUDO_MODULE_DEBUG;

            my %chosen;
            %chosen<pm>   = ~$!path           if $!has-source;
            %chosen<load> = self.precomp-path if $!has-precomp;
            %chosen<key>  = %chosen<pm> // %chosen<load>;

            my @MODULES = nqp::clone(@*MODULES // ());
            for @MODULES -> $m {
                if $m<module> eq $!name {
                    nqp::die("Circular module loading detected involving module '$!name'");
                }
            }

            if $?RAKUDO_MODULE_DEBUG {
                my $text := "chosen:";
                for %chosen {
                    $text := $text ~ "\n " ~ $_.key ~ ' => ' ~ $_.value;
                }
                RAKUDO_MODULE_DEBUG($text);
            }

            # If we didn't already do so, load the module and capture
            # its mainline. Otherwise, we already loaded it so go on
            # with what we already have.
            unless $!is-loaded {
                my @*MODULES := @MODULES;

                if +@*MODULES  == 0 {
                    @*MODULES[0] = { line => $line, filename => nqp::getlexdyn('$?FILES') };
                }
                else {
                    @*MODULES[*-1] = { line => $line };
                }

                my %trace            = { module => $!name, filename => %chosen<pm> };
                my $preserve_global := nqp::ifnull(nqp::gethllsym('perl6', 'GLOBAL'), Mu);
                @*MODULES.push: %trace;

                if %chosen<load> {
                    %trace<precompiled> = %chosen<load>;
                    RAKUDO_MODULE_DEBUG("loading ", %chosen<load>) if $?RAKUDO_MODULE_DEBUG;
                    my %*COMPILING := nqp::hash();
                    my $*CTXSAVE := self;
                    my $*MAIN_CTX;
                    nqp::loadbytecode(%chosen<load>);
                    $!module_ctx := $*MAIN_CTX;
                    RAKUDO_MODULE_DEBUG("done loading ", %chosen<load>) if $?RAKUDO_MODULE_DEBUG;
                }
                else {
                    # If we're doing module pre-compilation, we should only
                    # allow the modules we load to be pre-compiled also.
                    if $*W && $*W.is_precompilation_mode() {
                        nqp::die(
                            "When pre-compiling a module, its dependencies must be pre-compiled first.\n" ~
                            "Please pre-compile " ~ %chosen<pm>);
                    }

                    # Read source file.
                    RAKUDO_MODULE_DEBUG("loading ", %chosen<pm>) if $?RAKUDO_MODULE_DEBUG;
                    my $fh := nqp::open(%chosen<pm>, 'r');
                    nqp::setencoding($fh, 'utf8');
                    my $source := nqp::readallfh($fh);
                    nqp::closefh($fh);

                    # Get the compiler and compile the code, then run it
                    # (which runs the mainline and captures UNIT).
                    my $?FILES   := %chosen<pm>;
                    my $eval     := nqp::getcomp('perl6').compile($source);
                    my $*CTXSAVE := self;
                    my $*MAIN_CTX;
                    $eval();
                    $!module_ctx := $*MAIN_CTX;
                    RAKUDO_MODULE_DEBUG("done loading ", %chosen<pm>) if $?RAKUDO_MODULE_DEBUG;
                }

                nqp::bindhllsym('perl6', 'GLOBAL', $preserve_global);
                $!is-loaded = True;

                CATCH {
                    default {
                        nqp::bindhllsym('perl6', 'GLOBAL', $preserve_global);
                        .throw;
                    }
                }
            }

            # Provided we have a mainline and need to do global merging...
            if nqp::defined($!module_ctx) {
                # Merge any globals.
                my $UNIT := nqp::ctxlexpad($!module_ctx);
                if GLOBALish.^name eq 'GLOBAL' {
                    unless nqp::isnull(nqp::atkey($UNIT, 'GLOBALish')) {
                        merge_globals(GLOBALish, nqp::atkey($UNIT, 'GLOBALish'));
                    }
                }
                return $UNIT;
            }
            else {
                return {};
            }
    } ) }

    method ctxsave() {
        $*MAIN_CTX := nqp::ctxcaller(nqp::ctx());
        $*CTXSAVE := 0;
    }

    my $stub_how     := 'Perl6::Metamodel::PackageHOW';
    my $nqp_stub_how := 'KnowHOW';
    sub merge_globals(Mu \target is rw, Mu \source is rw) {
        # Start off merging top-level symbols. Easy when there's no
        # overlap. Otherwise, we need to recurse.
        my %known_symbols;
        for % = target.WHO {
            %known_symbols{$_.key} := 1;
        }
        for % = source.WHO {
            my $sym := $_.key;
            my $val := nqp::decont($_.value);
            my $tgt := (target.WHO){$sym};

            if !%known_symbols{$sym} {
                (target.WHO){$sym} := $val;
            }
            elsif $tgt =:= $val {
                # No problemo; a symbol can't conflict with itself.
            }
            else {
                my $source_mo      := $val.HOW;
                my $source_is_stub := $source_mo.HOW.name($source_mo) eq $stub_how
                                   || $source_mo.HOW.name($source_mo) eq $nqp_stub_how;
                my $target_mo      := (target.WHO){$sym}.HOW;
                my $target_is_stub := $target_mo.HOW.name($target_mo) eq $stub_how
                                   || $source_mo.HOW.name($source_mo) eq $nqp_stub_how;
                if $source_is_stub && $target_is_stub {
                    # Both stubs. We can safely merge the symbols from
                    # the source into the target that's importing them.
                    merge_globals($tgt, $val);
                }
                elsif $source_is_stub {
                    # The target has a real package, but the source is a
                    # stub. Also fine to merge source symbols into target.
                    merge_globals($tgt, $val);
                }
                elsif $target_is_stub {
                    # The tricky case: here the interesting package is the
                    # one in the module. So we merge the other way around
                    # and install that as the result.
                    merge_globals($val, $tgt);
                    (target.WHO){$sym} := $val;
                }
                else {
                    nqp::die("CU Merging GLOBAL symbols failed: duplicate definition of symbol $sym");
                }
            }
        }
    }
}

# TEMPORARY ACCESS TO COMPUNIT INTERNALS UNTIL WE CAN LOAD DIRECTLY
multi sub postcircumfix:<{ }> (CompUnit:D \c, "provides" ) {
    my % = (
      c.name => {
        pm => {
          file => c.path
        },
        c.key => {
          file => c.has-precomp ?? c.precomp-path !! c.path
        }
      }
    );
}
multi sub postcircumfix:<{ }> (CompUnit:D \c, "key" ) {
    c.key;
}
multi sub postcircumfix:<{ }> (CompUnit:D \c, "ver" ) {
    Version.new('0');
}
