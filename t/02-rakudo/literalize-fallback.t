use Test;

# An indirect package name built from an expression that is not known at
# compile time must fail with a proper diagnostic. The literalize fallback
# for an unhandled node must report "not literalizable", which surfaces here
# as "not compile-time known", rather than leaking an internal NYI message.

plan 2;

throws-like 'my $*x; class ::($*x ?? "A" !! "B") { }', Exception,
  message => /'not compile-time known'/,
  'indirect package name from a runtime expression is reported as not compile-time known';

# The fallback returns the CannotLiteralize sentinel (an undefined type
# object) instead of throwing when no specific candidate matches.
{
    use experimental :rakuast;
    nok RakuAST::Statement::Empty.new.literalize.defined,
      'literalize of a node with no specific candidate returns an undefined sentinel';
}
