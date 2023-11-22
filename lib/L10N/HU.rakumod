# This file contains the Hungarian Slang of the Raku Programming Language

#- start of generated part of localization ------------------------------------
#- Generated on 2023-11-22T11:54:33+01:00 by tools/build/makeL10N.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

role L10N::HU {
    use experimental :rakuast;
    token block-default { alapból}
    token block-else { különben}
    token block-elsif { különbenha}
    token block-for { ezekre}
    token block-given { adva}
    token block-if { ha}
    token block-loop { ciklus}
    token block-orwith { vagyezzel}
    token block-repeat { ismétel}
    token block-unless { kivéveha}
    token block-until { mígvégül}
    token block-when { amikor}
    token block-whenever { valahányszor}
    token block-while { amíg}
    token block-with { ezzel}
    token block-without { enélkül}
    token constraint-where { ahol}
    token infix-pcontp { "(tartalmaz)"}
    token infix-pelemp { "(eleme)"}
    token infix-cff { "^ff"}
    token infix-cffc { "^ff^"}
    token infix-cfff { "^fff"}
    token infix-cfffc { "^fff^"}
    token infix-after { ezután}
    token infix-and { és}
    token infix-andthen { ésakkor}
    token infix-before { ezelőtt}
    token infix-but { de}
    token infix-cmp { hasonlít}
    token infix-coll { coll}
    token infix-div { oszt}
    token infix-does { csinál}
    token infix-eq { egyenlő}
    token infix-ff { ff}
    token infix-ffc { "ff^"}
    token infix-fff { fff}
    token infix-fffc { "fff^"}
    token infix-gcd { gcd}
    token infix-ge { nagyobbegyenlő}
    token infix-gt { nagyobb}
    token infix-lcm { lcm}
    token infix-le { kisebbegyenlő}
    token infix-leg { kne}
    token infix-lt { kisebb}
    token infix-max { max}
    token infix-min { min}
    token infix-minmax { minmax}
    token infix-mod { moduló}
    token infix-ne { nemegyenlő}
    token infix-notandthen { nemésakkor}
    token infix-o { o}
    token infix-or { vagy}
    token infix-orelse { vagyamúgy}
    token infix-unicmp { unicmp}
    token infix-x { x}
    token infix-X { X}
    token infix-xx { xx}
    token infix-Z { Z}
    token meta-R { R}
    token meta-X { X}
    token meta-Z { Z}
    token modifier-for { ezekre}
    token modifier-given { adva}
    token modifier-if { ha}
    token modifier-unless { kivéveha}
    token modifier-until { mígvégül}
    token modifier-when { amikor}
    token modifier-while { amíg}
    token modifier-with { ezzel}
    token modifier-without { enélkül}
    token multi-multi { multi}
    token multi-only { egyedüli}
    token multi-proto { proto}
    token package-class { osztály}
    token package-grammar { nyelvtan}
    token package-module { modul}
    token package-package { csomag}
    token package-role { szerep}
    token phaser-BEGIN { BEGIN}
    token phaser-CATCH { LEKEZELÉS}
    token phaser-CHECK { CHECK}
    token phaser-CLOSE { BEZÁRÁS}
    token phaser-CONTROL { VEZÉRLÉS}
    token phaser-DOC { DOC}
    token phaser-END { VÉGE}
    token phaser-ENTER { BELÉPÉS}
    token phaser-FIRST { ELSŐ}
    token phaser-INIT { INIT}
    token phaser-KEEP { MEGTARTÁS}
    token phaser-LAST { UTOLSÓ}
    token phaser-LEAVE { KILÉPÉS}
    token phaser-NEXT { KÖVETKEZŐ}
    token phaser-POST { POSZT}
    token phaser-PRE { PRE}
    token phaser-QUIT { BEZÁRÁS}
    token phaser-UNDO { VISSZAVONÁS}
    token prefix-not { nem}
    token prefix-so { tehát}
    token quote-lang-m { m}
    token quote-lang-ms { ms}
    token quote-lang-q { q}
    token quote-lang-Q { Q}
    token quote-lang-qq { qq}
    token quote-lang-rx { rx}
    token quote-lang-s { s}
    token quote-lang-S { S}
    token quote-lang-ss { ss}
    token quote-lang-Ss { Ss}
    token routine-method { metódus}
    token routine-regex { regex}
    token routine-rule { szabály}
    token routine-sub { szub}
    token routine-submethod { szubmetódus}
    token routine-token { token}
    token scope-anon { anonim}
    token scope-augment { bővít}
    token scope-constant { konstans}
    token scope-has { bír}
    token scope-HAS { BÍR}
    token scope-my { enyém}
    token scope-our { miénk}
    token scope-state { állapot}
    token scope-unit { egység}
    token stmt-prefix-also { szintén}
    token stmt-prefix-do { csinál}
    token stmt-prefix-eager { buzgó}
    token stmt-prefix-gather { gyűjt}
    token stmt-prefix-hyper { hyper}
    token stmt-prefix-lazy { lusta}
    token stmt-prefix-quietly { halkan}
    token stmt-prefix-race { verseny}
    token stmt-prefix-react { reagál}
    token stmt-prefix-sink { süllyeszt}
    token stmt-prefix-start { indít}
    token stmt-prefix-supply { ellát}
    token stmt-prefix-try { próbál}
    token term-nano { nano}
    token term-now { most}
    token term-pi { pi}
    token term-rand { véletlen}
    token term-self { maga}
    token term-tau { tau}
    token term-time { idő}
    token traitmod-does { csinál}
    token traitmod-handles { kezel}
    token traitmod-hides { elrejt}
    token traitmod-is { ez}
    token traitmod-of { ebből}
    token traitmod-returns { visszatér}
    token traitmod-trusts { megbízik}
    token typer-enum { enum}
    token typer-subset { részhalmaz}
    token use-import { importál}
    token use-need { szükséges}
    token use-no { nincs}
    token use-require { require}
    token use-use { használ}
    method core2ast {
        my %mapping = "absz", "abs", "mind", "all", "antipárok", "antipairs", "bármelyik", "any", "hozzáfűz", "append", "kivár", "await", "zsák", "bag", "kiment", "bail-out", "megáld", "bless", "rendszerez", "categorize", "felső-egészrész", "ceiling", "betűk", "chars", "levág-sorvég", "chomp", "levág", "chop", "betűként", "chr", "betűkként", "chrs", "osztályoz", "classify", "bezár", "close", "kódok", "codes", "fésű", "comb", "kombinációk", "combinations", "tartalmaz", "contains", "kereszt", "cross", "dekódol", "decode", "mély-hozzárendel", "deepmap", "definiált", "defined", "meghal", "die", "kész", "done", "kacsa-hozzárendel", "duckmap", "elemek", "elems", "kiküld", "emit", "enkódol", "encode", "vég", "end", "ezzel-végződik", "ends-with", "eval-meghal-ok", "eval-dies-ok", "eval-él-ok", "eval-lives-ok", "kilép", "exit", "meghiúsul", "fail", "meghiúsul-mint", "fails-like", "első", "first", "lapos", "flat", "visszafelé", "flip", "alsó-egészrész", "floor", "bekér-sor", "get", "bekér-betű", "getc", "lényeg", "gist", "szűr", "grep", "szótár", "hash", "fej", "head", "behúzás", "indent", "indexek", "indices", "ez", "is", "nagyjából", "is-approx", "teljesen", "is-deeply", "prím-e", "is-prime", "ezegy-ok", "isa-ok", "eznem", "isnt", "elem", "item", "illeszt", "join", "kulcs", "key", "kulcsok", "keys", "ké", "kv", "utolsó", "last", "utolsó-hívás", "lastcall", "mint", "like", "sorok", "lines", "felsorol", "list", "él-ok", "lives-ok", "alsó-bit", "lsb", "készít", "make", "hozzárendel", "map", "paar", "match", "mozgat", "move", "felső-bit", "msb", "új", "new", "következő", "next", "nem-ok", "nok", "semelyik", "none", "nem", "not", "megjegyez", "note", "egy", "one", "sorszám", "ord", "sorszámok", "ords", "pár", "pair", "párok", "pairs", "jó", "pass", "permutációk", "permutations", "választ", "pick", "tervez", "plan", "eléfűz", "prepend", "nyomtat", "print", "nyomtatf", "printf", "továbbhalad", "proceed", "mögészúr", "push", "kitesz", "put", "véletlen", "rand", "újra", "redo", "redukál", "reduce", "ismétlődő", "repeated", "megfordít", "reverse", "jobbindex", "rindex", "megpörget", "roll", "gyökök", "roots", "forgat", "rotate", "kerekít", "round", "futtat", "run", "mond", "say", "halmaz", "set", "kiléptet", "shift", "jelző", "signal", "átlép", "skip", "átlép-maradék", "skip-rest", "alszik", "sleep", "vekker", "sleep-timer", "alszik-mígvégül", "sleep-until", "schlürfe", "slurp", "elcsen", "snitch", "tehát", "so", "rendez", "sort", "összetold", "splice", "felvág", "split", "négyzetgyök", "sqrt", "összeránt", "squish", "ezzel-kezdődik", "starts-with", "cserél", "subst", "alteszt", "subtest", "sikerül", "succeed", "összegez", "sum", "szimbolikus-link", "symlink", "farok", "tail", "vesz", "take", "vesz-ío", "take-rw", "dob-mint", "throws-like", "teendő", "todo", "fordít", "trans", "nyír", "trim", "nyír-elöl", "trim-leading", "nyír-hátul", "trim-trailing", "csonkít", "truncate", "egyedi", "unique", "nem-ilyen", "unlike", "elold", "unlink", "elészúr", "unshift", "használ-ok", "use-ok", "érték", "value", "értékek", "values", "figyelmeztet", "warn", "szavak", "words";
        my $ast := self.ast;
        my $name := $ast ?? $ast.simple-identifier !! self.Str;
        if %mapping{$name} -> $original {
            RakuAST::Name.from-identifier($original)
        }
        else {
            $ast // RakuAST::Name.from-identifier($name)
        }
    }
    method trait-is2ast {
        my %mapping = "másolat", "copy", "alapvető", "default", "ELAVULT", "DEPRECATED", "ekvivalens", "equiv", "implementációs-részlet", "implementation-detail", "lazább", "looser", "nodális", "nodal", "tiszta", "pure", "nyers", "raw", "írható-olvasható", "rw", "szűkebb", "tighter";
        my $ast := self.ast;
        my $name := $ast ?? $ast.simple-identifier !! self.Str;
        if %mapping{$name} -> $original {
            RakuAST::Name.from-identifier($original)
        }
        else {
            $ast // RakuAST::Name.from-identifier($name)
        }
    }
    method adverb-pc2str (str $key) {
        my %mapping = "töröl", "delete", "létezik", "exists", "ké", "kv", "é", "v";
        %mapping{$key} // $key
    }
    method adverb-q2str (str $key) {
        $key
    }
    method adverb-rx2str (str $key) {
        my %mapping = "f", "c", "folytat", "continue", "kim", "ex", "kimerítő", "exhaustive", "globális", "global", "odik", "nd", "edik", "nth", "átf", "ov", "átfedés", "overlap", "poz", "pos", "racsni", "ratchet", "te", "rd", "ödik", "st", "ig", "to", "szer", "x";
        %mapping{$key} // $key
    }
    method named2str (str $key) {
        my %mapping = "abszolút", "absolute", "hozzáfűz", "append", "bezár", "close", "teljesen", "completely", "folytat", "continue", "darabszám", "count", "dátum", "date", "nap", "day", "töröl", "delete", "elemek", "elems", "végétől", "end", "minden", "every", "exkluzív", "exclusive", "kimerítő", "exhaustive", "lejár", "expires", "formázó", "formatter", "globális", "global", "óra", "hour", "ebben", "in", "ebbe", "into", "kulcs", "key", "ké", "kv", "hallgat", "listen", "perc", "minute", "mód", "mode", "hónap", "month", "név", "name", "dik", "nd", "dik", "nth", "ki", "off", "átf", "ov", "átfedés", "overlap", "részleges", "partial", "részek", "parts", "poz", "pos", "dik", "rd", "csere", "replacement", "másodperc", "second", "másodpercek", "seconds", "méret", "size", "quetschen", "squash", "dik", "st", "szigorú", "strict", "alsóindex", "subscript", "felsőindex", "superscript", "teszt", "test", "dik", "th", "ennyiszer", "times", "időzóna", "timezone", "é", "v", "érték", "value", "ahol", "where", "év", "year";
        %mapping{$key} // $key
    }
    method pragma2str (str $key) {
        $key
    }
    method system2str (str $key) {
        $key
    }
}

# The EXPORT sub that actually does the slanging
my sub EXPORT($dontslang?) {
    unless $dontslang {
        my $LANG := $*LANG;
        $LANG.define_slang('MAIN',
          $LANG.slang_grammar('MAIN').^mixin(L10N::HU)
        );
    }

    BEGIN Map.new
}

#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of localization --------------------------------------

# vim: expandtab shiftwidth=4
