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
