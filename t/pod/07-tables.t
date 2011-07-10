use Test;
plan 61;
my $r;

=begin table
        The Shoveller   Eddie Stevens     King Arthur's singing shovel
        Blue Raja       Geoffrey Smith    Master of cutlery
        Mr Furious      Roy Orson         Ticking time bomb of fury
        The Bowler      Carol Pinnsler    Haunted bowling ball
=end table

$r = $=POD[0];
isa_ok $r, Pod::Block::Table;
is $r.content[0][0], 'The Shoveller';
is $r.content[0][1], 'Eddie Stevens';
is $r.content[0][2], "King Arthur's singing shovel";
is $r.content[1][0], 'Blue Raja';
is $r.content[1][1], 'Geoffrey Smith';
is $r.content[1][2], 'Master of cutlery';
is $r.content[2][0], 'Mr Furious';
is $r.content[2][1], 'Roy Orson';
is $r.content[2][2], 'Ticking time bomb of fury';
is $r.content[3][0], 'The Bowler';
is $r.content[3][1], 'Carol Pinnsler';
is $r.content[3][2], 'Haunted bowling ball';

=table
    Constants           1
    Variables           10
    Subroutines         33
    Everything else     57

$r = $=POD[1];
is $r.content[0][0], 'Constants';
is $r.content[1][0], 'Variables';
is $r.content[2][0], 'Subroutines';
is $r.content[3][0], 'Everything else';
is $r.content[0][1], '1';
is $r.content[1][1], '10';
is $r.content[2][1], '33';
is $r.content[3][1], '57';

=for table
    mouse    | mice
    horse    | horses
    elephant | elephants

$r = $=POD[2];
is $r.content[0][0], 'mouse';
is $r.content[0][1], 'mice';
is $r.content[1][0], 'horse';
is $r.content[1][1], 'horses';
is $r.content[2][0], 'elephant';
is $r.content[2][1], 'elephants';

=table
    Animal | Legs |    Eats
    =======================
    Zebra  |   4  | Cookies
    Human  |   2  |   Pizza
    Shark  |   0  |    Fish

$r = $=POD[3];
is $r.headers[0], 'Animal';
is $r.headers[1], 'Legs';
is $r.headers[2], 'Eats';
is $r.content[0][0], 'Zebra';
is $r.content[0][1], '4';
is $r.content[0][1], '4';
is $r.content[0][2], 'Cookies';
is $r.content[1][0], 'Human';
is $r.content[1][1], '2';
is $r.content[1][2], 'Pizza';
is $r.content[2][0], 'Shark';
is $r.content[2][1], '0';
is $r.content[2][2], 'Fish';

=table
        Superhero     | Secret          | 
                      | Identity        | Superpower
        ==============|=================|================================
        The Shoveller | Eddie Stevens   | King Arthur's singing shovel

$r = $=POD[4];
is $r.headers[0], 'Superhero';
is $r.headers[1], 'Secret Identity';
is $r.headers[2], 'Superpower';
is $r.content[0][0], 'The Shoveller';
is $r.content[0][1], 'Eddie Stevens';
is $r.content[0][2], "King Arthur's singing shovel";

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
is $r.headers[0], 'Superhero';
is $r.headers[1], 'Secret Identity';
is $r.headers[2], 'Superpower';
is $r.content[0][0], 'The Shoveller';
is $r.content[0][1], 'Eddie Stevens';
is $r.content[0][2], "King Arthur's singing shovel";
is $r.content[1][0], 'Blue Raja';
is $r.content[1][1], 'Geoffrey Smith';
is $r.content[1][2], 'Master of cutlery';
is $r.content[2][0], 'Mr Furious';
is $r.content[2][1], 'Roy Orson';
is $r.content[2][2], 'Ticking time bomb of fury';
is $r.content[3][0], 'The Bowler';
is $r.content[3][1], 'Carol Pinnsler';
is $r.content[3][2], 'Haunted bowling ball';
