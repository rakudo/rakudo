#
package NQP::Configure::Macros;
use v5.10.1;
use NQP::Versions;
use Text::ParseWords;
use File::Spec;
require NQP::Config;

my %preexpand = map { $_ => 1 } qw<
  include include_with_backends include_capture
  insert insert_capture insert_filelist 
  nfn sp_escape nl_escape fixup uc lc
  bprefixes_with
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
    my $msg = shift;

    if (ref($self->{on_fail}) eq 'CODE') {
        $self->{on_fail}->($msg);
    }

    die $msg;
}

sub execute {
    my $self  = shift;
    my $macro = shift;
    my $param = shift;

    $self->fail( "Macro name is missing in call to method execute()" ) unless $macro;

    my $method = "_m_$macro";

    $self->fail( "Unknown macro $macro" ) unless $self->can($method);

    if ( $preexpand{$macro} ) {
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
    while (
        $text =~ /
                 (?<text>.*? (?= @ | \z))
                 (
                     (?<msym> (?: @@ | @))
                     (?:
                         (?<macro_var> [:\w]+ )
                       | (?: (?<macro_func> [:\w]+ )
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
            $chunk = $cfg->cfg_var( $m{macro_var} ) // '';
            #$self->fail( "No configuration variable '$m{macro_var}' found" )
            #  unless defined $chunk;
        }
        elsif ( $m{macro_func} ) {
            $chunk = $mobj->execute( $m{macro_func}, $m{mparam} );
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

sub include {
    my $self      = shift;
    my $filenames = shift;
    my @filenames =
      ref($filenames) ? @$filenames : shellwords($filenames);
    my %params = @_;
    my $text   = "";
    my $cfg    = $self->{config_obj};

    for my $file ( map { $self->_m_unescape($_) } @filenames ) {
        next unless $file;    # Split may result in some empty items.
        $file = $cfg->template_file_path( $file, required => 1 );
        $self->fail( "Circular dependency detected on including $file" )
          if $self->{'-including'}{$file};
        $self->{'-including'}{$file} = 1;
        $text .= $self->inc_comment("Included from $file")
          unless $params{as_is};
        $text .= $self->expand( NQP::Config::slurp($file) );
        $text .= $self->inc_comment("End of section included from $file")
          unless $params{as_is};
        delete $self->{'-including'}{$file};
    }
    return $text;
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
    my $cfg = $self->{config_obj};
    my @filelist = shellwords(shift);
    my $out = "";
    for my $f (@filelist) {
        for my $b ( $cfg->active_backends ) {
            my $b_inc = "$f-$b";
            my $s = $cfg->push_cur_backend($b);
            $out .= $self->include($b_inc);
        }
    }
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
# All file names in the list will be indeted by 4 spaces except for the first
# one. Newlines in from the source will be escaped with \
sub _m_insert_filelist {
    my $self   = shift;
    my $cfg    = $self->{config_obj};
    my $indent = " " x ( $cfg->{config}{filelist_indent} || 4 );
    my $file   = $cfg->template_file_path( shift, required => 1 );
    my $text   = NQP::Config::slurp($file);
    $text =~ s/(\n)\h*(\H)/\\$1$indent$2/gs;
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
    return NQP::Config::nfp(shift);
}

# uc(str)
# Converts string to all uppercase
sub _m_uc {
    uc $_[1]
}

# lc(str)
# Converts string to all lowercase
sub _m_lc {
    lc $_[1]
}

1;
