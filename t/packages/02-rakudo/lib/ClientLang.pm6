# Have this module language version differ from calling package one.
use v6.c;
unit module ClientLang;
use nqp;

sub client-ctx( --> Mu ) is raw is export {
    nqp::p6clientctx()
}

sub client-core-ctx( --> Mu ) is raw is export {
    nqp::p6clientcorectx()
}

sub client-core-rev( --> Str ) is export {
    nqp::p6clientcorerev()
}

sub client-core-ver( --> Str ) is export {
    nqp::p6clientcorever()
}

# The following subs are wrappers to check for CLIENT::-like behaviour of the ops

sub client-rev( --> Str ) is export {
    client-core-rev
}

sub client-ver( --> Str ) is export {
    client-core-ver
}
