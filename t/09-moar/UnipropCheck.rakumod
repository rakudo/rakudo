#!/usr/bin/env perl6
use Test;

my %prop-data =
    NAME => 'name',
    Line_Break => 'str',
    General_Category => 'str'
;
my %expected-loses =
    "extracted/DerivedGeneralCategory.txt" => {
        General_Category => {
            Cn => 27,
        },
    },
    # Many codepoints return XX instead of ID. These codepoints are undefined, but unicode
    # spec has specified that they should regardless be ID
    "LineBreak.txt" => {
        Line_Break => {
            ID => 66841,
            PR => 16
        }
    }
;
# Number of wins/loses for each property value
my (%LOSES, %WINS); # %( Property_Name => Filename => Property_Value => Num_Wins/Loses )
my %FAILED;  # %( Property_Name => Filename => [ 10, 20, 30, 44 ] ) List of failed codepoints
my %WIN-NUM; # %( Property_Name => Filename => Total_Num_Win/Lose )
my %LOSE-NUM;
use nqp;
sub test-file (IO::Path $folder is copy, Str:D $file-name, Str:D $uniprop, :$answer-column is copy) is export {
    note "File: $file-name" if $*DEBUG;
    $answer-column = 1 if !defined $answer-column;
    $folder = $*CWD.child("t/3rdparty/Unicode/13.0.0") if !$folder;
    %LOSE-NUM{$file-name}{$uniprop} = 0 if %LOSE-NUM{$file-name}{$uniprop}:!exists;
    %WIN-NUM{$file-name}{$uniprop}  = 0 if %WIN-NUM{$file-name}{$uniprop}:!exists;
    my IO::Path:D $file = $folder.child: $file-name;
    die $uniprop if %prop-data{$uniprop}:!exists;
    my Str:D $call = %prop-data{$uniprop} orelse die "Property type is not set. Please set to 'str', 'int', or 'name'";
    my int $propcode = $call eq 'name' ?? 0 !! nqp::unipropcode($uniprop);
    for $file.lines.grep({ nqp::isne_s($_, '') && !nqp::eqat($_, '#', 0) }) -> $line {
        my str @array = $uniprop eq 'NAME' ?? nqp::split(';', $line) !! $line.split([';', "#"]);
        my str $range = @array[0].trim;
        my str $value = @array[$answer-column].trim;
        my str @ranges = nqp::split("..", $range);
        my int $lowest =  hex(@ranges[0]);
        my int $highest = 1 < @ranges.elems ?? hex(@ranges[1]) !! $lowest;
        if $propcode {
            for $lowest..$highest {
                if $value eq ($call eq 'str'
                    ?? nqp::getuniprop_str($_, $propcode)
                    !! ($call eq 'int'
                        ?? nqp::getuniprop_int($_, $propcode)
                        !! die "\$call is set to '$call'\nIt should be either 'int', 'str' or 'name'")
                        ) {
                    correct($value, $_, $uniprop, $file-name);
                }
                else {
                    if $*DEBUG {
                        my $actual = $call eq 'str' ?? nqp::getuniprop_str($_, $propcode) !! nqp::getuniprop_int($_, $propcode);
                        wrong($value, "cp $_ expected $value actually $actual", $uniprop, $file-name);
                    }
                    else {
                        wrong($value, $_, $uniprop, $file-name);
                    }
                }
            }
        }
        else {
            check-name($lowest, $value, $file-name);
        }
        CATCH { die "$_\nError: Line: $line\n\@array: @array.raku()" }
    }
    my Bool:D $has-tested = False;
    for %LOSES{$file-name}{$uniprop}.keys.sort -> $pvalue {
        $has-tested = True;
        %expected-loses{$file-name}{$uniprop}{$pvalue} = 0 if %expected-loses{$file-name}{$uniprop}:!exists or %expected-loses{$file-name}{$uniprop}{$pvalue}:!exists;
        todo "Expecting %expected-loses{$file-name}{$uniprop}{$pvalue} failures from $uniprop=$pvalue", 1
            if %LOSES{$file-name}{$uniprop}{$pvalue} <  %expected-loses{$file-name}{$uniprop}{$pvalue};
        ok %LOSES{$file-name}{$uniprop}{$pvalue} <= %expected-loses{$file-name}{$uniprop}{$pvalue},
        format-test-text($uniprop, $file-name, %WINS{$file-name}{$uniprop}{$pvalue}, %LOSES{$file-name}{$uniprop}{$pvalue}, $pvalue, %expected-loses{$file-name}{$uniprop}{$pvalue});
    }
    for %expected-loses{$file-name}{$uniprop}.keys.sort -> $pvalue {
        next if %LOSES{$file-name}{$uniprop}{$pvalue}:exists;
        $has-tested = True;
        %LOSES{$file-name}{$uniprop}{$pvalue} = 0 if %LOSES{$file-name}{$uniprop}{$pvalue}:!exists;
        todo "Expecting %expected-loses{$file-name}{$uniprop}{$pvalue} failures from $uniprop=$pvalue", 1
            if %LOSES{$file-name}{$uniprop}{$pvalue} < %expected-loses{$file-name}{$uniprop}{$pvalue};
        ok(%LOSES{$file-name}{$uniprop}{$pvalue} <= %expected-loses{$file-name}{$uniprop}{$pvalue},
            format-test-text($uniprop, $file-name,
                %WINS{$file-name}{$uniprop}{$pvalue},
                %LOSES{$file-name}{$uniprop}{$pvalue}, $pvalue,
                %expected-loses{$file-name}{$uniprop}{$pvalue}));
    }
    if !$has-tested {
        ok True, format-test-text($uniprop, $file-name, %WIN-NUM{$file-name}{$uniprop}, %LOSE-NUM{$file-name}{$uniprop});
    }
    if $*DEBUG {
        my @list = %FAILED{$file-name}{$uniprop}.list;
        my @debug-out;
        while @list {
            @debug-out.push: @list.splice(0, 10).join(', ');
        }
        note "\%FAILED\{$file-name\}\{$uniprop\}:\n", @debug-out.join("\n").indent(4);
        note "Property: $uniprop win %WIN-NUM{$file-name}{$uniprop} lose %LOSE-NUM{$file-name}{$uniprop} percent: " ~ (%LOSE-NUM{$file-name}{$uniprop} == 0 ?? 1 !! %LOSE-NUM{$file-name}{$uniprop}/%WIN-NUM{$file-name}{$uniprop}) * 100;
    }
}
sub format-test-text ($uniprop, $file-name, $correct, $wrong, Str:D $pvalue = "", Int $looking-for?) {
    "Property: $uniprop"
    ~ ($pvalue ?? "=$pvalue" !! "")
    ~ " from file: $file-name (Correct: $correct Wrong: $wrong)"
    ~ ($looking-for ?? ". Todo'd if < $looking-for failures." !! "")
}
sub ends-with (\a, \b) {
    nqp::eqat(a, b, nqp::chars(a) - nqp::chars(b))
}
sub check-name (Int:D $code, Str:D $name, Str:D $file-name) {
    state %firsts;
    my str $expected = $name;
    my str $uniname = nqp::getuniname($code);
    if nqp::eqat($expected, '<', 0) {
        if nqp::eqat($expected, '<Hangul Syllable', 0) {
            # XXX TODO Hangul Syllables
            return;
        }
        elsif ends-with($expected, ', First>') {
            $expected = nqp::substr($expected, 0, nqp::chars($expected) - nqp::chars(", First>")) ~ ">";
            %firsts{$expected} = $code;
        }
        elsif ends-with($expected, ', Last>') {
            $expected = nqp::substr($expected, 0, nqp::chars($expected) - nqp::chars(", Last>")) ~ ">";
            die "\%firsts{$expected} does not exist" unless %firsts{$expected};
            my $ex = $expected;
            if nqp::eqat($expected, "<CJK Ideograph", 0) {
                $expected = "CJK UNIFIED IDEOGRAPH";
            }
            elsif nqp::eqat($expected, "<Tangut", 0) {
                $expected = "TANGUT IDEOGRAPH";
            }
            elsif $expected.ends-with('Private Use>') {
                $expected = '<private-use>';
            }
            elsif $expected.ends-with('Surrogate>') {
                $expected = '<surrogate>';
            }
            for %firsts{$ex}..$code -> $curr-code {
                $uniname = nqp::getuniname($curr-code);
                my str $this-expected = get-hex-name($expected, $curr-code);
                #say "before $expected after $this-expected";
                nqp::iseq_s($uniname, $this-expected)
                    ?? correct("", $curr-code, "NAME", $file-name)
                    !! wrong("", $curr-code, "NAME", $file-name);
            }

        }
        elsif nqp::iseq_s($expected, "<control>") {
            $expected = get-hex-name($expected, $code);
        }
        else {
            die $expected;
        }

    }
    else {
        nqp::iseq_s($uniname, $expected)
            ?? correct("", $code, "NAME", $file-name)
            !! wrong($expected, $code, "NAME", $file-name);
    }
}
sub get-hex-name ($name is copy, $code is raw) {
    my $has-brack = nqp::eqat($name, '>', nqp::chars($name) - 1);
    $name = nqp::substr($name, 0, nqp::chars($name) - 1)
        if $has-brack;
    my str $base = nqp::base_I(nqp::decont($code), 16);
    my int $diff = nqp::sub_i(4, nqp::chars($base));
    if nqp::islt_i(0, $diff) {
        $base = nqp::concat(nqp::x('0', $diff), $base);
    }
    nqp::concat(
        nqp::concat(
            nqp::concat($name, '-'),
        $base),
    $has-brack ?? '>' !! '');

}
sub correct ($value, $code, $uniprop, $file-name) {
    %WINS{$file-name}{$uniprop}{$value}++;
    %WIN-NUM{$file-name}{$uniprop}++;
}
sub wrong ($value, $code, $uniprop, $file-name) {
    %LOSES{$file-name}{$uniprop}{$value}++;
    %FAILED{$file-name}{$uniprop}.push: $code;
    %LOSE-NUM{$file-name}{$uniprop}++;
}
sub hex (Str:D $string) {
     nqp::radix_I(16, $string,  0, 0, Int)[0]
}

# vim: expandtab shiftwidth=4
