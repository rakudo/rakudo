#!/usr/bin/perl
# Copyright (C) 2008, The Perl Foundation.
# $Id$

=head1 NAME

gen_objectref_pmc.pl - Generates the ObjectRef PMC

=head1 SYNOPSIS

    % perl gen_objectref_pmc.pl pmc_template output_file

=head1 DESCRIPTION

Takes a template for the ObjectRef PMC, and fills in the missing v-table
methods with methods that simply delegate them to the held value.

=head1 FUNCTIONS

=over 4

=cut

use strict;

use Parrot::Config;

# Get and check parameters.
my ($template, $output) = @ARGV;
unless ($template && $output) {
    die "Usage: perl gen_objectref_pmc.pl pmc_template output_file";
}
unless (-e $template) {
    die "ObjectRef PMC template file '$template' does not exist";
}

# Read template.
my $template_contents = slurp($template);

# Read v-tables list and get list of functions from it.
my $vtable_list = slurp($PConfig{build_dir}.'/src/vtable.tbl');
my @all_vtables = extract_matches($vtable_list, '(\w+)\\(');

# Find v-table methods defined in the ObjectRef template and exclude
# them from the list.
my @nogen = extract_matches($template_contents, 'VTABLE\s*\w+\s*\*?\s*(\w+)\\(');
my %nogen;
$nogen{$_}++ for @nogen;
my @generate = grep { !$nogen{$_} } @all_vtables;

# Generate methods and insert into file.
my $gen_code = join("\n", map { generate_meth($_, $vtable_list) } @generate);
$template_contents =~ s/(pmclass[^{]+\{.+?)(VTABLE)/$1\n$gen_code$2/s;

# Write it.
spew($output, $template_contents);


=item generate_meth

Generates a forwarder method for the PMC.

=cut

sub generate_meth {
    my ($name, $vtable_file) = @_;

    # Extract signature, parameter names and MMD info.
    unless ($vtable_file =~ /^(\w+\s*\*?)\s*$name\((.*)\)(\s+MMD)?/m) {
        die "Can't get type info for vtable method $name";
    }
    if ($3) {
        # Don't generate forwarders for MMD methods.
        return "";
    }
    my $ret_type = $1;
    my $param_list = $2;
    $ret_type =~ s/\s+//g;
    # Get list of parameters to pass on in forwarding (basically,
    # strip types).
    my @params = split(/\s*,\s*/, $param_list);
    my $pass_list = join(', ', map { /(?:^|\s)(\w+)$/; $1 } @params);
    $pass_list = ", $pass_list" if $pass_list;

    # Any return?
    my $return = $ret_type eq 'void' ? '' : 'return ';

    # Generate method.
    my $meth = <<METH;
    VTABLE $ret_type $name($param_list) {
        PMC * _VALUE;
        GET_ATTR_value(INTERP, SELF, _VALUE);
        ${return}VTABLE_$name(INTERP, _VALUE$pass_list);
    }
METH
    return $meth;
}


=item slurp

Reads in an entire file.

=cut

sub slurp {
    my $filename = shift;
    open my $fh, "< $filename" or die "Could not open $filename: $!";
    my $content = join('', <$fh>);
    close $fh;
    return $content;
}


=item spew

Writes a string to a file, replacing or creating it as needed.

=cut

sub spew {
    my ($filename, $content) = @_;
    open my $fh, "> $filename" or die "Could not open $filename: $!";
    print $fh $content;
    close $fh
}


=item extract_matches

Extracts all items matching the given pattern and puts $1 into an
array.

=cut

sub extract_matches {
    my ($text, $pattern) = @_;
    my @found = ();
    while ($text =~ /$pattern/g) {
        push @found, $1;
    }
    return @found;
}


=back

=cut
