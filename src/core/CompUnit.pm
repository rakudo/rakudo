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

    my $slash := IO::Spec.rootdir;
    my Lock $global = Lock.new;
    my $default-from = 'Perl6';
    my %instances;

    method new(
      $path is copy,
      :$name is copy,
      :$extension is copy,
      :$from = $default-from,
      :$has-source is copy,
      :$has-precomp is copy,
    ) {

        # set name / extension if not already given
        if !$name or !$extension.defined {
            my $file;
            for $path.rindex($slash) -> $i {
                $file = $i.defined ?? $path.substr($i+1) !! $path;
            }

            # no $slash in char class
            if $file ~~ m/ (<-[\\/.]>+) . (<-[.]>+) $/ {
                $name      ||= ~$0;
                $extension ||= ~$1;
            }
        }

        # sanity test
        my $precomp-ext = $*VM.precomp-ext;
        $path = IO::Spec.rel2abs($path);
        $has-source  //= ?$path.IO.f;
        $has-precomp //= ?"$path.$precomp-ext".IO.f;
        return Nil unless $has-source or $has-precomp;

        $global.protect( { %instances{$path} //= self.bless(
          :$path,
          :$name,
          :$extension,
          :$precomp-ext,
          :$from,
          :$has-source,
          :$has-precomp,
          :!is-loaded,
        ) } );
    }

    method BUILD(
      :$path,
      :$!name,
      :$!extension,
      :$!precomp-ext,
      :$!from,
      :$!has-source,
      :$!has-precomp,
      :$!is-loaded,
    ) {
        $!lock  = Lock.new;
        $!WHICH = "{self.^name}|$path";
        $!path  = $path.path;
        self
    }

    method WHICH(--> Str) { self.DEFINITE ?? $!WHICH !! self.^name }
    method Str(--> Str)   { self.DEFINITE ?? $!path.Str !! Nil }
    method gist(--> Str)  {
        self.DEFINITE ?? "{self.name}:{$!path.Str}" !! self.^name;
    }
    method perl(--> Str)  { self.DEFINITE ?? nextsame() !! self.^name }

    method key(--> Str) {
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
    method load(
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

    method precomp-path(--> Str) { "$!path.$!precomp-ext" }

    method precomp($output = self.precomp-path, :$force --> Bool) {
        die "Cannot pre-compile an already pre-compiled file: $!path"
          if $.has-precomp;
        die "Cannot pre-compile over an existing file: $output"
          if !$force and $output.IO.e;
        my Bool $result = ?shell("$*EXECUTABLE --target={$*VM.precomp-target} --output=$output $!path");

        $!has-precomp = $result if $output eq self.precomp-path;
        $result;
    }
}

# TEMPORARY ACCESS TO COMPUNIT INTERNALS UNTIL WE CAN LOAD DIRECTLY
multi postcircumfix:<{ }> (CompUnit \c, "provides" ) {
    my % = (
      c.name => {
        c.key => {
          file => c.has-precomp ?? c.precomp-path !! c.path
        }
      }
    );
}
multi postcircumfix:<{ }> (CompUnit \c, "key" ) {
    c.key;
}
multi postcircumfix:<{ }> (CompUnit \c, "ver" ) {
    Version.new('0');
}
