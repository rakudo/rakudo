class CompUnit {
    has Lock     $!lock;
    has Str      $.from;
    has Str      $.name;
    has Str      $.extension;
    has IO::Path $.path;
    has Str      $!WHICH;
    has Bool     $.loaded;

    my Lock $global = Lock.new;
    my $default-from = 'Perl6';
    my %instances;

    method new( $path is copy, :$name, :$extension, :$from = $default-from ) {
        $path = IO::Spec.rel2abs($path);
        for $path.IO -> $io {
            return Nil if !$io.e or $io.d;
        }
        $global.protect( { %instances{$path} //=
          self.bless(:$path,:$name,:$extension,:$from) } );
    }

    method BUILD( :$path, :$!name, :$!extension, :$!from ) {
        $!lock  = Lock.new;
        $!WHICH = "{self.^name}|$path";
        $!path  = $path.path;
        self
    }

    method WHICH() { self.DEFINITE ?? $!WHICH !! self.^name }
    method Str()   { self.DEFINITE ?? $!path.Str !! Nil }
    method gist()  { self.DEFINITE ?? "{self.name}:{$!path.Str}" !! self.^name }
    method perl()  { self.DEFINITE
      ?? "CompUnit.new('{$!path.Str}',:name<$!name>,:extension<$!extension>{",:from<$!from>" if $!from ne $default-from})"
      !! self.^name;
    }

    method key() {
        $!extension eq $*VM.precomp-ext ?? $*VM.precomp-ext !! 'pm';
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
            return $!loaded if $!loaded;

            my $candi = self.candidates($module_name, :auth(%opts<auth>), :ver(%opts<ver>))[0];
            my %chosen;
            if $candi {
                %chosen<pm>   :=
                  $candi<provides>{$module_name}<pm><file>;
                %chosen<load> :=
                  $candi<provides>{$module_name}{$*VM.precomp-ext}<file>;
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

    method precomped() { $!extension eq $*VM.precomp-ext }

    method precomp-path() {
        my $ext := $!extension;  # cannot use attributes in regex
        $!path.subst(/$ext$/,$*VM.precomp-ext);
    }

    method precomp($output = self.precomp-path, :$force) {
        die "Cannot pre-compile an already pre-compiled file: $!path"
          if self.precomped;
        die "Cannot pre-compile over an existing file: $output"
          if !$force and $output.IO.e;
        ?shell("$*EXECUTABLE --target={$*VM.precomp-target} --output=$output $!path");
    }
}

# TEMPORARY ACCESS TO COMPUNIT INTERNALS UNTIL WE CAN LOAD DIRECTLY
multi postcircumfix:<{ }> (CompUnit \c, "provides" ) {
    my % = ( c.name => { c.key => { file => c.path } } );
}
multi postcircumfix:<{ }> (CompUnit \c, "ver" ) {
    Version.new('0');
}
