use MONKEY-SEE-NO-EVAL;
# Pins the nested-EVAL wrapper in RakuAST::CompUnit. BEGIN holds
# non-serializable state (a Lock and a ConditionVariable). The wrapper
# terminates the EVAL'd Sub's outer chain walk before the serializer
# reaches BEGIN's lexpad.
our &x;
BEGIN {
    my $lock = Lock.new;
    my $cv = $lock.condition;
    &x = EVAL 'sub () { 42 }';
}
