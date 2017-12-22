use Test;

plan 2;

is-deeply  +combinations(100, 70), 29372339821610944823963760,
  'combinations() iterator implements efficient .count-only';
is-deeply ?+combinations(100, 70), True,
  'combinations() iterator implements efficient .bool-only';
