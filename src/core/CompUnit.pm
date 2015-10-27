class CompUnit {
    has Lock $!lock;
    has Str  $.from;
    has Str  $.name;
    has Str  $.extension;
    has Str  $.path;
    has Str  $.precomp-path;
    has Str  $!WHICH;
    has Bool $.has-source;
    has Bool $.has-precomp;
    has Bool $.is-loaded;

    # The CompUnit::Repository that loaded this CompUnit.
    has CompUnit::Repository $.repo is required;
    # That repository's identifier for the compilation unit. This is not globally unique.
    # has Str:D $.repo-id is required;

    # The low-level handle.
    has CompUnit::Handle $.handle; # is required;

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
      :$repo,
    ) {

        # set name / extension if not already given
        if !$name or !$extension.defined {
            my IO::Spec $SPEC := $*SPEC;
            $name      ||= $SPEC.basename($path);
            $extension ||= $SPEC.extension($name);
        }

        # sanity test
        my $VM = $*VM;
        my $precomp-path = $path ~ '.' ~ $VM.precomp-ext;  # XXX temporary
        $precomp-path = 
          $VM.precomp-dir ~ '/' ~ $name ~ '.' ~ $VM.precomp-ext
          unless $precomp-path.IO.f;
        $has-source  //= ?$path.IO.f;
        $has-precomp //= ?$precomp-path.IO.f;
        return Nil unless $has-source or $has-precomp;

        $global.protect( { %instances{$path} //= self.bless(
          :$path,
          :lock(Lock.new),
          :$name,
          :$extension,
          :$precomp-path,
          :$from,
          :$has-source,
          :$has-precomp,
          :$repo,
          :!is-loaded,
        ) } );
    }

    multi method WHICH(CompUnit:D:) { $!WHICH //= "{self.^name}|$!path.abspath()" }
    multi method Str(CompUnit:D: --> Str)  { $!path.abspath }
    multi method gist(CompUnit:D: --> Str) { "{self.name}:{$!path.abspath}" }

    method key(CompUnit:D: --> Str) {
        $!has-precomp ?? $*VM.precomp-ext !! $!extension;
    }

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
  if $*RAKUDO_MODULE_DEBUG;

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
    multi method load(CompUnit:D: \GLOBALish, :$line) {
        $global.protect( {
            my int $DEBUG = $*RAKUDO_MODULE_DEBUG;
            RAKUDO_MODULE_DEBUG("going to load $!name") if $DEBUG;

            my %chosen;
            %chosen<pm>   = ~$!path           if $!has-source;
            %chosen<load> = self.precomp-path if $!has-precomp;
            %chosen<key>  = %chosen<pm> // %chosen<load>;

            my @MODULES = nqp::clone(@*MODULES // ());
            for @MODULES -> $m {
                if $m<module> && $m<module> eq $!name {
                    nqp::die("Circular module loading detected involving module '$!name'");
                }
            }

            if $DEBUG {
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

                my $trace            = { module => $!name, filename => %chosen<pm> };
                my $preserve_global := nqp::ifnull(nqp::gethllsym('perl6', 'GLOBAL'), Mu);
                @*MODULES.push: $trace;

                if %chosen<load> {
                    $trace<precompiled> = %chosen<load>;
                    RAKUDO_MODULE_DEBUG("loading ", %chosen<load>) if $DEBUG;
                    my $*CTXSAVE := self;
                    $!handle := CompUnit::Loader.load-precompilation-file(%chosen<load>);
                    RAKUDO_MODULE_DEBUG("  done: ", %chosen<load>) if $DEBUG;
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
                    RAKUDO_MODULE_DEBUG("loading ", %chosen<pm>) if $DEBUG;

                    my $*CTXSAVE := self;
                    $!handle := CompUnit::Loader.load-source-file(%chosen<pm>);
                    RAKUDO_MODULE_DEBUG("done: ", %chosen<pm>) if $DEBUG;
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
            my $globalish := $!handle.globalish-package;
            if $globalish !=== Stash {
                # Merge any globals.
                nqp::gethllsym('perl6', 'ModuleLoader').merge_globals(GLOBALish, $globalish);
            }
    } ) }

    method unit() {
        $.handle.unit
    }

    method ctxsave() {
        $*MAIN_CTX := nqp::ctxcaller(nqp::ctx());
        $*CTXSAVE := 0;
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

# vim: ft=perl6 expandtab sw=4
