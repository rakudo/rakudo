#
package NQP::Configure::Macros;
use v5.10.1;
use Text::ParseWords;
use File::Spec;
use Data::Dumper;
require NQP::Config;

my %preexpand = map { $_ => 1 } qw<
  include include_with_backends include_capture
  insert insert_capture insert_filelist
  inclide_with_specs tmpl_with_specs
  nfn sp_escape nl_escape fixup uc lc nfp
  bprefixes_with expand
>;

sub new {
    my $class = shift;
    my $self  = bless {}, $class;
    return $self->init(@_);
}

sub init {
    my $self   = shift;
    my %params = @_;

    $self->{config_obj} = %params{config};

    for $p (qw<on_fail>) {
        $self->{$p} = $params{$p} if $params{$p};
    }

    return $self;
}

sub fail {
    my $self = shift;
    my $msg  = shift;

    if ( ref( $self->{on_fail} ) eq 'CODE' ) {
        $self->{on_fail}->($msg);
    }

    die $msg;
}

sub execute {
    my $self   = shift;
    my $macro  = shift;
    my $param  = shift;
    my %params = @_;

    $self->fail("Macro name is missing in call to method execute()")
      unless $macro;

    my $method = "_m_$macro";

    $self->fail("Unknown macro $macro") unless $self->can($method);

    if ( !$params{no_preexapnd} && $preexpand{$macro} ) {
        $param = $self->expand($param);
    }

    $self->$method($param);
}

sub expand {
    my $self = shift;
    my $text = shift;

    return $text if index( $text, '@' ) < 0;

    my %params = @_;

    my $cfg    = $self->{config_obj};
    my $config = $cfg->{config};

    my $mobj = $self;

    if ( $params{isolate} ) {
        $mobj = NQP::Configure::Macros->new( config => $cfg );
    }

    my $text_out = "";

    # @mfunc()@ @!mfunc()@
    while (
        $text =~ /
                 (?<text>.*? (?= @ | \z))
                 (
                     (?<msym> (?: @@ | @))
                     (?:
                         (?<macro_var> [:\w]+ )
                       | (?: (?<mfunc_noexp>!)? (?<macro_func> [:\w]+ )
                           (?>
                             \( 
                               (?<mparam>
                                 (
                                     (?2)
                                   | [^\)]
                                   | \) (?! \k<msym> )
                                   | \z (?{ $self->fail( "Can't find closing \)$+{msym} for macro '$+{macro_func}'" ) })
                                 )*
                               )
                             \) 
                           )
                       )
                       | \z
                     )
                     \k<msym>
                 )?
                /sgcx
      )
    {
        my %m = %+;
        $text_out .= $m{text} // "";
        my $chunk;
        if ( $m{macro_var} ) {
            $chunk = $cfg->cfg( $m{macro_var} ) // '';

            #$self->fail( "No configuration variable '$m{macro_var}' found" )
            #  unless defined $chunk;
        }
        elsif ( $m{macro_func} ) {
            my %params;
            $params{no_preexapnd} = !!$m{mfunc_noexp};
            $chunk = $mobj->execute( $m{macro_func}, $m{mparam}, %params );
        }

        if ( defined $chunk ) {
            $text_out .=
                $m{msym} eq '@@'
              ? $mobj->_m_sp_escape($chunk)
              : $chunk;
        }
    }

    return $text_out;
}

sub inc_comment {
    my $self    = shift;
    my $comment = shift;

    chomp $comment;

    my $len = length($comment) + 4;
    my $bar = '#' x $len;
    return "$bar\n# $comment #\n$bar\n";
}

sub is_including {
    my $self = shift;
    my $file = shift;
    my $cfg  = $self->{config_obj};

    for my $ctx ( $cfg->contexts ) {
        return 1
          if $ctx->{including_file}
          && File::Spec->rel2abs( $ctx->{including_file} ) eq
          File::Spec->rel2abs($file);
    }
    return 0;
}

sub include {
    my $self      = shift;
    my $filenames = shift;
    my @filenames = ref($filenames) ? @$filenames : shellwords($filenames);
    my %params    = @_;
    my $text      = "";
    my $cfg       = $self->{config_obj};

    $params{required} //= 1;

    my %tmpl_params;
    for $p (qw<subdir subdirs subdirs_only>) {
        $tmpl_params{$p} = $params{$p} if $params{$p};
    }

    for my $file ( map { $self->_m_unescape($_) } @filenames ) {
        next unless $file;
        $file = $cfg->template_file_path( $file, required => 1, %tmpl_params );
        my $ctx = $cfg->cur_ctx;
        $self->fail( "Circular dependency detected on including $file"
              . $cfg->include_path )
          if $self->is_including;
        $ctx->{including_file} = $file;
        $text .= $self->inc_comment("Included from $file")
          unless $params{as_is};
        $text .= $self->expand( NQP::Config::slurp($file) );
        $text .= $self->inc_comment("End of section included from $file")
          unless $params{as_is};
    }
    return $text;
}

sub specs_iterate {
    my $self = shift;
    my $cb   = shift;

    my $cfg = $self->{config_obj};
    for my $spec ( $cfg->perl6_specs ) {
        my $spec_subdir = "6.$spec->[0]";
        my %config      = ( spec_subdir => $spec_subdir, );
        my $spec_ctx =
          $cfg->make_spec_ctx( spec => $spec, configs => [ \%config ] );
        my $s = $cfg->push_ctx($spec_ctx);
        $cb->(@_);
    }
    return $out;
}

sub include_with_specs {
    my $self      = shift;
    my $filenames = shift;
    my %params    = @_;
    my @filelist  = ref($filenames) ? @$filenames : shellwords($filenames);

    my $cfg = $self->{config_obj};
    my $out = "";
    for my $spec ( $cfg->perl6_specs ) {
        my $spec_subdir = "6.$spec->[0]";
        my %config      = ( spec_subdir => $spec_subdir, );
        my $spec_ctx =
          $cfg->make_spec_ctx( spec => $spec, configs => [ \%config ] );
        my $s = $cfg->push_ctx($spec_ctx);
        for my $f (@filelist) {
            my $tmpl_f;
            if ( $params{no_subdir} ) {
                $tmpl_f = $f;
            }
            else {
                die "File path ("
                  . $f
                  . ") cannot be absolute when including for a specification"
                  . $self->include_path
                  if File::Spec->file_name_is_absolute($f);
                my $tmpl_f = File::Spec->catfile( $spec_subdir, $f );
            }
            $out .= $self->include( $tmpl_f, %params );
        }
    }
    return $out;
}

# include(file1 file2)
# Include a file. Parameter is expanded first, then the result is used a the
# file name. File content is been expanded.
# Multiple filenames are split by spaces. If file path contains a space in it it
# must be quoted with \
sub _m_include {
    shift->include(@_);
}

# insert(file1 file2)
# Similar to include() but insert files as-is, no comments added.
sub _m_insert {
    shift->include( @_, as_is => 1 );
}

# include_with_backends(file1 file2)
# Appends backed suffix (-moar) for each active backend and tries to include
# this template.
sub _m_include_with_backends {
    my $self     = shift;
    my $cfg      = $self->{config_obj};
    my @filelist = shellwords(shift);
    my $out      = "";
    for my $b ( $cfg->active_backends ) {
        my $s = $cfg->push_ctx(
            {
                backend => $b,
                configs => [ $cfg->backend_config($b) ],
            }
        );
        for my $f (@filelist) {
            my $b_inc = "$f-$b";
            $out .= $self->include($b_inc);
        }
    }
    return $out;
}

# include_with_specs(file1 file2)
# Includes templates from @templates_dir@/6.@spec@/fileN
# File can have path to a subdir in the name but never be absolute path.
sub _m_include_with_specs {
    my $self      = shift;
    my $filenames = shift;

    my $out = "";
    my sub _iws_iter {
        $out .= $self->include(
            $filenames,
            subdir       => $self->{config_obj}->cfg('spec_subdir'),
            subdirs_only => 1
        );
    }
    $self->specs_iterate( \&_iws_iter );
    return $out;
}

# expand_with_specs(text)
# Expands text within each language spec context. The text is not pre-expanded.
sub _m_expand_with_specs {
    my $self = shift;
    my $text = shift;

    my $out = "";
    my sub _ews_iter {
        $out .= $self->expand($text);
    }
    $self->specs_iterate( \&_ews_iter );
    return $out;
}

# expand(text)
# Simply expands the text. Could be useful when:
# @expand(@!nfp(@var1@/@macro(...)@)@)@
# NOTE that input of expand() is pre-expanded first. So, use with extreme care!
sub _m_expand {
    my $self = shift;
    my $text = shift;
    my $out =  $self->expand($text);
}

# template(file1 file2)
# Finds corresponding template file for file names in parameter. Templates are
# been searched in templates_dir and possibly spec_subdir if in a spec context.
sub _m_template {
    my $self = shift;
    my $filenames = shift;
    my @filenames = shellwords($filenames);
    my $cfg = $self->{config_obj};
    my $spec_subdir = $cfg->cfg('spec_subdir');
    my %params = (required => 1);
    $params{subdir} = $spec_subdir if $spec_subdir;
    my @out;
    for my $src (@filenames) {
        push @out, $cfg->template_file_path($src, %params);
    }

    return join " ", @out;
}

# tmpl_with_specs(file1 file2)
# Repedeatly includes templates just applying contexts for different specs.
sub _m_tmpl_with_specs {
    my $self      = shift;
    my $filenames = shift;

    my $out = "";
    my sub _tws_iter {
        $out .= $self->include(
            $filenames,
            subdir => $self->{config_obj}->cfg('spec_subdir'),
            as_is  => 1,
        );
    }
    $self->specs_iterate( \&_tws_iter );
    return $out;
}

# insert_capture(command line)
# Captures output of the command line and inserts it.
sub _m_insert_capture {
    my $self = shift;
    my $cmd  = shift;
    return `$cmd`;
}

# include_capture(command line)
# Captures output of the command line and includes it.
sub _m_include_capture {
    my $self = shift;
    my $text = $self->_m_insert_capture(@_);
    return
        "\n"
      . $self->inc_comment("Included from `$_[0]`")
      . $text
      . $self->inc_comment("End of section included from `$_[0]`");
}

# fixup(makefile rules)
# Fixup input makefile rules. I.e. changes dir separators / for current OS and
# install timing measure where needed.
sub _m_fixup {
    my $self = shift;
    my $text = shift;
    return $self->{config_obj}->fixup_makefile($text);
}

# insert_filelist(filename)
# Inserts a list of files defined in file filename. File content is not
# expanded.
# All file names in the list will be indented by 4 spaces except for the first
# one. 
sub _m_insert_filelist {
    my $self   = shift;
    my $cfg    = $self->{config_obj};
    my $indent = " " x ( $cfg->{config}{filelist_indent} || 4 );
    my $file   = $cfg->template_file_path( shift, required => 1 );
    my $text   = NQP::Config::slurp($file);
    my @flist = map { NQP::Config::nfp($_) } grep { $_ } split /\s+/s, $text;
    $text = join " \\\n$indent", @flist;
    return $text;
}

# bprefixes_with(suffix)
# Will return space-separated list of active backend abbreviations with appended
# suffix:
#
# @brefixes_with(-all)@
#
# will result in something like:
#
# m-all j-all js-all
sub _m_bprefixes_with {
    my $self = shift;
    my $sfx  = shift;
    my $cfg  = $self->{config_obj};

    my @elems;
    for my $pfx ( $cfg->active_abbrs ) {
        push @elems, "$pfx$sfx";
    }
    return join " ", @elems;
}

# sp_escape(a string)
# Escapes all spaces in a string with \
# Implicitly called by @@ macros
sub _m_sp_escape {
    my $self = shift;
    my $str  = shift;
    $str =~ s{ }{\\ }g;
    $str;
}

# sp_escape(a string)
# Escapes all newlines in a string with \.
# Implicitly called by @@ macros
sub _m_nl_escape {
    my $self = shift;
    my $str  = shift;
    $str =~ s{(\n)}{\\$1}g;
    $str;
}

# unescape(a\ st\ring)
# Simlpe unescaping from backslashes. Replaces any \<char> sequence with <char>
sub _m_unescape {
    my $self = shift;
    my $str  = shift;
    $str =~ s/\\(.)/$1/g;
    return $str;
}

# nfp(dir/file)
# Normalizes a Unix-style file path for the current OS. Mostly for replacing
# / with \ for Win*
sub _m_nfp {
    my $self = shift;
    my @elems = split /(\s+)/s, shift;
    my $out = "";
    while (@elems) {
        my ($file, $ws) = (shift @elems, shift @elems);
        if ($file) { # If text starts with spaces $file will be empty
            $file = NQP::Config::nfp($file);
        }
        $out .= $file . $ws;
    }
    return $out;
}

# uc(str)
# Converts string to all uppercase
sub _m_uc {
    uc $_[1];
}

# lc(str)
# Converts string to all lowercase
sub _m_lc {
    lc $_[1];
}

1;
