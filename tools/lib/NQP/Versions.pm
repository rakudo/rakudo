#

package NQP::Versions;
use v5.10;
use base qw<Exporter>;

my @EXPORT_OK = qw<%perl6lang>;

our %perl6lang = (
    c => {},
    d => { variants => [qw<PREVIEW>], },
);
1;
