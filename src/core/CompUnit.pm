class CompUnit {
    has Lock $!lock;
    has Str  $.from;
    has Str  $.name;
    has Str  $.extension;
    has Str  $.precomp-ext;
    has Str  $.abspath;
    has Str  $!WHICH;
    has Bool $.has-source;
    has Bool $.has-precomp;
    has Bool $.is-loaded;

    my Lock $global = Lock.new;
    my $default-from = 'Perl6';
    my %instances;

    method new(CompUnit:U:
      $abspath,
      :$name is copy,
      :$extension is copy,
      :$from = $default-from,
      :$has-source is copy,
      :$has-precomp is copy,
    ) {

        # set name / extension if not already given
        if !$name or !$extension.defined {
            $name      ||= MAKE-BASENAME($abspath);
            $extension ||= MAKE-EXT($name);
        }

        # sanity test
        my $precomp-ext = $*VM.precomp-ext;
        $has-source  //= FILETEST-E($abspath);
        $has-precomp //= FILETEST-E("$abspath.$precomp-ext");
        return Nil unless $has-source or $has-precomp;

        $global.protect( { %instances{$abspath} //= self.bless(
          :$abspath,
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

    multi method WHICH(CompUnit:D:) { $!WHICH //= "{self.^name}|$!abspath" }
    multi method Str(CompUnit:D: --> Str)  { $!abspath }
    multi method gist(CompUnit:D: --> Str) { "$!name:$!abspath" }

    method path(CompUnit:D:) {
        DEPRECATED('abspath',|<2014.12 2015.12>);
        $!abspath;
    }

    method key(CompUnit:D: --> Str) {
        $!has-precomp ?? $!precomp-ext !! $!extension;
    }

    # same magic I'm not sure we need
    my Mu $p6ml := nqp::gethllsym('perl6', 'ModuleLoader');
    method p6ml() { $p6ml }
    method ctxsave() { $p6ml.ctxsave() }
    method absolute_path($path) { $p6ml.absolute_path($path) }
    method load_setting($setting_name) { $p6ml.load_setting($setting_name) }
    method resolve_repossession_conflicts(@conflicts) {
        $p6ml.resolve_repossession_conflicts(
          nqp::findmethod(@conflicts, 'FLATTENABLE_LIST')(@conflicts)
        );
    }

    # do the actual work
    method load(CompUnit:D:
      $module_name,
      %opts,
      *@GLOBALish is rw,
      :$line,
      :$file
    ) {
        $!lock.protect( {

            # nothing to do
            return $!is-loaded if $!is-loaded;

            my $candi = self.candidates($module_name, :auth(%opts<auth>), :ver(%opts<ver>))[0];
            my %chosen;
            if $candi {
                %chosen<pm>   :=
                  $candi<provides>{$module_name}<pm><file>;
                %chosen<pm>   := ~%chosen<pm> if %chosen<pm>.DEFINITE;
                %chosen<load> :=
                  $candi<provides>{$module_name}{$!precomp-ext}<file>;
                %chosen<key>  := %chosen<pm> // %chosen<load>;
            }
            $p6ml.load_module(
              $module_name,
              %opts,
              |@GLOBALish,
              :$line,
              :$file,
              :%chosen
            );
        } );
    }

    method precomp-path(CompUnit:D: --> Str) { "$!abspath.$!precomp-ext" }

    method precomp(CompUnit:D:
      $out  = self.precomp-path,
      :@INC = @*INC,
      :$force,
      --> Bool) {
        die "Cannot pre-compile an already pre-compiled file: $!path"
          if $.has-precomp;
        die "Cannot pre-compile over an existing file: $out"
          if !$force and FILETEST-E($out);
        my Mu $opts := nqp::atkey(%*COMPILING, '%?OPTIONS');
        my $lle = !nqp::isnull($opts) && !nqp::isnull(nqp::atkey($opts, 'll-exception'))
          ?? ' --ll-exception'
          !! '';
        %*ENV<RAKUDO_PRECOMP_WITH> = CREATE-INCLUDE-SPEC(@INC);
        my Bool $result = ?shell(
          "$*EXECUTABLE$lle --target={$*VM.precomp-target} --output=$out $!abspath"
        );
        %*ENV<RAKUDO_PRECOMP_WITH>:delete;

        $!has-precomp = $result if $out eq self.precomp-path;
        $result;
    }
}

# TEMPORARY ACCESS TO COMPUNIT INTERNALS UNTIL WE CAN LOAD DIRECTLY
multi sub postcircumfix:<{ }> (CompUnit:D \c, "provides" ) {
    my % = (
      c.name => {
        pm => {
          file => c.abspath
        },
        c.key => {
          file => c.has-precomp ?? c.precomp-path !! c.abspath
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
