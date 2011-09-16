use Test;
plan 38;
my $r;

=begin table
        The Shoveller   Eddie Stevens     King Arthur's singing shovel
        Blue Raja       Geoffrey Smith    Master of cutlery
        Mr Furious      Roy Orson         Ticking time bomb of fury
        The Bowler      Carol Pinnsler    Haunted bowling ball
=end table

$r = $=POD[0];
isa_ok $r, Pod::Block::Table;
is $r.content.elems, 4;
is $r.content[0].join('|'),
   "The Shoveller|Eddie Stevens|King Arthur's singing shovel";
is $r.content[1].join('|'),
   "Blue Raja|Geoffrey Smith|Master of cutlery";
is $r.content[2].join('|'),
   "Mr Furious|Roy Orson|Ticking time bomb of fury";
is $r.content[3].join('|'),
   "The Bowler|Carol Pinnsler|Haunted bowling ball";

=table
    Constants           1
    Variables           10
    Subroutines         33
    Everything else     57

$r = $=POD[1];
is $r.content.elems, 4;
is $r.content[0].join('|'), "Constants|1";
is $r.content[1].join('|'), "Variables|10";
is $r.content[2].join('|'), "Subroutines|33";
is $r.content[3].join('|'), "Everything else|57";

=for table
    mouse    | mice
    horse    | horses
    elephant | elephants

$r = $=POD[2];
is $r.content.elems, 3;
is $r.content[0].join('|'), "mouse|mice";
is $r.content[1].join('|'), "horse|horses";
is $r.content[2].join('|'), "elephant|elephants";

=table
    Animal | Legs |    Eats
    =======================
    Zebra  +   4  + Cookies
    Human  +   2  +   Pizza
    Shark  +   0  +    Fish

$r = $=POD[3];
is $r.headers.join('|'), "Animal|Legs|Eats";
is $r.content.elems, 3;
is $r.content[0].join('|'), "Zebra|4|Cookies";
is $r.content[1].join('|'), "Human|2|Pizza";
is $r.content[2].join('|'), "Shark|0|Fish";

=table
        Superhero     | Secret          | 
                      | Identity        | Superpower
        ==============|=================|================================
        The Shoveller | Eddie Stevens   | King Arthur's singing shovel

$r = $=POD[4];
is $r.headers.join('|'), "Superhero|Secret Identity|Superpower";
is $r.content.elems, 1;
is $r.content[0].join('|'),
   "The Shoveller|Eddie Stevens|King Arthur's singing shovel";

=begin table

                        Secret
        Superhero       Identity          Superpower
        =============   ===============   ===================
        The Shoveller   Eddie Stevens     King Arthur's
                                          singing shovel

        Blue Raja       Geoffrey Smith    Master of cutlery

        Mr Furious      Roy Orson         Ticking time bomb
                                          of fury

        The Bowler      Carol Pinnsler    Haunted bowling ball

=end table

$r = $=POD[5];
is $r.headers.join('|'), "Superhero|Secret Identity|Superpower";
is $r.content.elems, 4;
is $r.content[0].join('|'),
   "The Shoveller|Eddie Stevens|King Arthur's singing shovel";
is $r.content[1].join('|'),
   "Blue Raja|Geoffrey Smith|Master of cutlery";
is $r.content[2].join('|'),
   "Mr Furious|Roy Orson|Ticking time bomb of fury";
is $r.content[3].join('|'),
   "The Bowler|Carol Pinnsler|Haunted bowling ball";

=table
    X | O |
   ---+---+---
      | X | O
   ---+---+---
      |   | X

$r = $=POD[6];
is $r.content.elems, 3;
is $r.content[0].join(','), 'X,O,';
is $r.content[1].join(','), ',X,O';
is $r.content[2].join(','), ',,X';

=table
    X   O     
   ===========
        X   O 
   ===========
            X 

$r = $=POD[7];
is $r.content.elems, 3;
is $r.content[0].join(','), 'X,O,';
is $r.content[1].join(','), ',X,O';
is $r.content[2].join(','), ',,X';

=begin table

foo
bar

=end table

$r = $=POD[8];
is $r.content.elems, 2;
