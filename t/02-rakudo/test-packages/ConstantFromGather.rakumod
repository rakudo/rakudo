unit module ConstantFromGather;

# A `%` constant initialized from `gather` is computed at BEGIN time and
# materialized with .Map, so the serialized value is a plain Map that survives
# precompilation and is reusable, not the gather block or a single-use Seq.
our constant %RULES = gather { take 'a' => 1; take 'b' => 2 };
