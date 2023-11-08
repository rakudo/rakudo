functie answer geeft-terug Int is rauw {
    42
}
mijn klasse Frobnicate {
    heeft $.a = 42;
    methode foo {
        zeg zelf.a
    }
}
zeg Frobnicate.nieuw.foo;
mijn $a = 41;
mijn $b = 666;
als $a {
    $a++
}
andersals $b {
    sterf "That is not the right number!"
}
anders {
    merk-op "Shouldn't see this"
}
zeg "incremented" als $a == answer;
met $a {
    zeg $a
}
ofmet $b {
    waarschuw "still not the right number"
}
gegeven 666 {
    indien 42 {
        zeg "the answer"
    }
    gebrek {
        stilletjes waarschuw "No answer"
    }
}
geen strict;
$c = 137;
dd $c.indenteer(4);
gebruik strict;
mijn @a = ^10;
zeg @a.eerste(* %% 2, :vaneinde);
mijn %h = a => 666;
zeg %h<a>:verwijder;
zeg-het dq:tot/HERE/;
heredoc
HERE
vooralle ^3 {
    .zeg
}
.zeg vooralle <a b c>;
lus {
    zeg "loop";
    laatste
}
