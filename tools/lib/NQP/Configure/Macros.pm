#
package NQP::Configure::Macros;
use v5.10;
use NQP::Versions;

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

sub _m_test {
    #my $self = shift;
    return "*this is a test* ($_[1])";
}

sub _m_sp_escape {
    my $str = $_[1];
    $str =~ s{ }{\\ }g;
    $str;
}

1;
