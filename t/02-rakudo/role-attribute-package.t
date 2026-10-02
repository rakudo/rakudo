use Test;

plan 5;

# An attribute a role brings into a class belongs to the class, and the
# object keeps it in the part of its layout that is keyed on the class.

role Counting { has $.count = 1 }
class Counter does Counting { }
class Tally is Counter does Counting { }

my $attribute := Counter.^attributes.first(*.name eq '$!count');
ok $attribute.package === Counter,
  'the attribute of a role belongs to the class that does the role';
is $attribute.get_value(Counter.new), 1,
  'the attribute reads the value through the class';
is Counter.new(count => 5).count, 5,
  'the accessor the role made reads the value';

my $again := Tally.^attributes(:local).first(*.name eq '$!count');
ok $again.package === Tally,
  'a subclass doing the role again has a copy that belongs to it';
is Tally.new(count => 7).count, 7,
  'the subclass reads its own copy';

# vim: expandtab shiftwidth=4
