#!/usr/bin/perl

use strict;
use warnings;
use lib qw(languages/perl6/lib perl6/lib ./lib ../lib ../../lib ../../../lib);
use Parrot::Test::Perl6 tests => 3;


=head1 NAME

languages/perl6/t/basic.t - Perl6 basic tests

=head1 SYNOPSIS

    % cd languages/perl6
    % prove t/basic.t

=cut


perl6_output_is( <<'CODE', <<'OUTPUT', 'say 1 (output)' );
say 1;
CODE
"VAR1" => PMC 'PGE::Rule' => "say 1;\n" @ 0 {
    <Perl6::Grammar::program> => PMC 'Perl6::Grammar' => "say 1;\n" @ 0 {
        <statement_list> => PMC 'Perl6::Grammar' => "say 1;" @ 0 {
            <statement> => ResizablePMCArray (size:1) [
                PMC 'Perl6::Grammar' => "say 1;" @ 0 {
                    <expression> => PMC 'Perl6::Grammar' => "say 1;" @ 0 {
                        <opparse> => PMC 'Perl6::Grammar' => "say 1;" @ 0 {
                            <expr> => PMC 'PGE::Match' => ";" @ 5 {
                                <type> => "infix:;"
                                [0] => PMC 'Perl6::Grammar' => "say" @ 0 {
                                    <ident> => PMC 'Perl6::Grammar' => "say" @ 0
                                    <type> => "prelist:"
                                    [0] => PMC 'Perl6::Grammar' => "1" @ 4 {
                                        <integer> => PMC 'Perl6::Grammar' => "1" @ 4
                                        <type> => "term:"
                                    }
                                }
                                [1] => PMC 'PGE::Match' => "" @ 6
                            }
                        }
                    }
                }
            ]
        }
    }
}
OUTPUT


perl6_output_is( <<'CODE', <<'OUTPUT', 'say 1 (stdout)' );
say 1;
CODE
"VAR1" => PMC 'PGE::Rule' => "say 1;\n" @ 0 {
    <Perl6::Grammar::program> => PMC 'Perl6::Grammar' => "say 1;\n" @ 0 {
        <statement_list> => PMC 'Perl6::Grammar' => "say 1;" @ 0 {
            <statement> => ResizablePMCArray (size:1) [
                PMC 'Perl6::Grammar' => "say 1;" @ 0 {
                    <expression> => PMC 'Perl6::Grammar' => "say 1;" @ 0 {
                        <opparse> => PMC 'Perl6::Grammar' => "say 1;" @ 0 {
                            <expr> => PMC 'PGE::Match' => ";" @ 5 {
                                <type> => "infix:;"
                                [0] => PMC 'Perl6::Grammar' => "say" @ 0 {
                                    <ident> => PMC 'Perl6::Grammar' => "say" @ 0
                                    <type> => "prelist:"
                                    [0] => PMC 'Perl6::Grammar' => "1" @ 4 {
                                        <integer> => PMC 'Perl6::Grammar' => "1" @ 4
                                        <type> => "term:"
                                    }
                                }
                                [1] => PMC 'PGE::Match' => "" @ 6
                            }
                        }
                    }
                }
            ]
        }
    }
}
OUTPUT


perl6_stderr_is( <<'CODE', <<'OUTPUT', 'say 1 (stderr)' );
say 1;
CODE
OUTPUT
