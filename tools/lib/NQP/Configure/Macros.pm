#
package NQP::Configure::Macros;
use v5.10;
use NQP::Versions;
use File::Spec;

sub new {
    my $class = shift;
    my $self = bless {}, $class;
    return $self->init(@_);
}

sub init {
    my $self = shift;
    my %params = @_;

    $self->{config} = %params{config};

    return $self;
}

sub execute {
    my $self = shift;
    my $macro = shift;

    die "Macro name is missing in call to method execute()" unless $macro;

    my $method = "_m_$macro";

    die "Unknown macro $macro" unless $self->can($method);

    $self->$method(@_);
}

sub expand {
    my $self = shift;
    my $text = shift;

    return $text if index($text, '@') < 0;

    my %params = @_;

    my $config = $self->{config};

    my $mobj = $self;

    if ( $params{isolate} ) {
        $mobj = NQP::Configure::Macros->new( config => $config );
    }

    my $text_out = "";
    while ( $text =~ /
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
                                   | \z (?{ die "Can't find closing \)$+{msym} for macro '$+{macro_func}'" })
                                 )*
                               )
                             \) 
                           )
                       )
                       | \z
                     )
                     \k<msym>
                 )?
                /sgcx ) {
            my %m = %+;
            $text_out .= $m{text} // "";
            my $chunk;
            if ( $m{macro_var} ) {
                $chunk = $config->{ $m{macro_var} };
            }
            elsif ( $m{macro_func} ) {
                $chunk = $mobj->execute( $m{macro_func}, $m{mparam} );
            }

            if (defined $chunk) {
                $text_out .= $m{msym} eq '@@' ? 
                                $mobj->_m_sp_escape($chunk) : 
                                $chunk;
            }
    }

    return $text_out;
}

sub split_spaced {
    my $self = shift;

    return split /(?<!\\)\s/s, shift;
}

# include(file1 file2)
# Include a file. Parameter is expanded first, then the result is used a the
# file name. File content is been expanded.
# Multiple filenames are split by spaces. If file path contains a space in it it
# must be quoted with \
sub _m_include {
    my $self = shift;
    my @filenames = $self->split_spaced( $self->expand(shift) );
    my $text = "";

    for my $file ( map { $self->_m_unescape($_) } @filenames ) {
        next unless $file; # Split may result in some empty items.
        die "Circular dependency detected on including $file" 
            if $self->{'-including'}{$file};
        $self->{'-including'}{$file} = 1;
        $text .= "\n## Included from $file\n";
        $text .= $self->expand( NQP::Configure::slurp( $file ) );
        $text .= "\n## End of included $file\n"; 
        delete $self->{'-including'}{$file};
    }
    return $text;
}

# sp_escape(a string)
# Escapes all spaces in a string with \
# Implicitly called by @@ macros
sub _m_sp_escape {
    my $self = shift;
    my $str = self->expand($_[1]);
    $str =~ s{ }{\\ }g;
    $str;
}

# unescape(a\ st\ring)
# Simlpe unescaping from backslashes. Replaces any \<char> sequence with <char>
sub _m_unescape {
    my $self = shift;
    my $str = shift;
    $str =~ s/\\(.)/$1/g;
    return $str;
}

# nfs(dir/file)
# Normalizes a Unix-style file name for the current OS.
sub _m_nfn {
    my $self = shift;
    my ($vol, $dirs, $file) = File::Spec->splitpath(shift);
    return File::Spec->catpath(
        $vol,
        File::Spec->catdir( File::Spec->splitdir( $dirs ) ),
        $file
    );
}

1;
