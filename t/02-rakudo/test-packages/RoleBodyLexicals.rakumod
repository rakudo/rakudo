unit module RoleBodyLexicals;

my $v = 7;

our sub for-modifier() { (my role R1 { method m { $v } } for 1..1)[0].m }

my role R2 { method m { $v } } for ();
my class C2 does R2 { }
our sub for-modifier-over-nothing() { C2.new.m }

my $r3 = try my role R3 { method m { $v } };
our sub under-try() { $r3.m }

my $r4 = (gather take my role R4 { method m { $v } })[0];
our sub under-gather() { $r4.m }

my $r5 = (42 andthen (Nil andthen my role R5 { method m { $v } }));
our sub in-thunk-not-run() { R5.m }

constant K6 = (42 andthen my role R6 { method m { $v } });
our sub constant-andthen() { K6.m }

constant K7 = (42 andthen (my role R7 { method m { $v } }));
our sub constant-andthen-parens() { K7.m }

constant K8 = [my role R8 { method m { $v } }][0];
our sub constant-array() { K8.m }

constant K9 = ((my role R9 { method m { $v } } for 1..1)[0]);
our sub constant-for-modifier() { K9.m }

my role R10 { my $u = $v + 1; method m { $u } } for 1..1;
our sub body-lexical() { R10.m }

my $r11 = once my role R11 { method m { $v } };
our sub under-once() { $r11.m }

my $r12;
multi trait_mod:<is>(Mu:U $c, :$tagged!) { $r12 = $tagged }
my class C12 is tagged(my role R12 { method m { $v } }) { }
our sub class-trait() { $r12.new.m }

my %h13 = { a => (role { method m { $v } }) };
our sub hash-composer() { %h13<a>.new.m }
