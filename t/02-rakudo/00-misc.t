use Test;
plan 1;

# https://github.com/rakudo/rakudo/issues/1566
subtest 'role-applying traits on SETTING routines are late enough (R#1566)' => {
  # To fix failures in this test, move all routines that have role-applying
  # traits on them into src/core/traited-routines.pm6 or any place later
  # in the setting

  plan 2;
  for <tools/build/moar_core_sources  tools/build/jvm_core_sources> -> $list {
    subtest "$list source list" => {
      plan +my @sources = $list.IO.lines.grep: {
        # file all source files before and including all the code files
        state $code-files = SetHash.new: <
            src/core/Code.pm6
            src/core/Routine.pm6
            src/core/Block.pm6
            src/core/Submethod.pm6
            src/core/Method.pm6
        >;
        LEAVE $code-files{$_}--;
        $code-files
      }

      for @sources -> $file {
        my @seen;
        for $file.IO.lines.kv -> $n, $line {
          next unless $line.contains:
            any «'is pure'  'is nodal'  'is default'  'is hidden-from-USAGE'»;
          next if .contains: «'# IGNORE' '# TRAITED'».any
            & 'traited-routines.pm6';
          push @seen, "$file:{$n+1}: $line";
        }
        cmp-ok +@seen, '==', 0, "not seen any traits in $file"
          or diag @seen.join: "\n";
      }
    }
  }
}
