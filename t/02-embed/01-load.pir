=head1 NAME

t/02-embed/01-load.pir - Tests loading of bytecode

=head1 SYNOPSIS

    % parrot t/02-embed/01-load.pir

=head1 DESCRIPTION

Tests the loading of perl6.pbc . This file is used by t/02-embed/01-load.t

=cut

.sub 'main' :main
    .include 'test_more.pir'

    plan(1)

    test_load()
.end

.sub test_load
    lives_ok(<<'CODE',"can load_bytecode perl6.pbc")
.sub main
    load_bytecode "perl6.pbc"
.end
CODE
.end
