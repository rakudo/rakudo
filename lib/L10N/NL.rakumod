# This file contains the Dutch Slang of the Raku Programming Language

#- start of generated part of localization ------------------------------------
#- Generated on 2023-10-17T13:57:58+02:00 by tools/build/makeL10N.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

role L10N::NL {
    use experimental :rakuast;
    token block-default { gebrek}
    token block-else { anders}
    token block-elsif { andersals}
    token block-for { vooralle}
    token block-given { gegeven}
    token block-if { als}
    token block-loop { lus}
    token block-orwith { ofmet}
    token block-repeat { herhaal}
    token block-unless { tenzij}
    token block-until { totdat}
    token block-when { indien}
    token block-whenever { zodra}
    token block-while { zolang}
    token block-with { met}
    token block-without { zonder}
    token constraint-where { waar}
    token infix-pcontp { "(bevat)"}
    token infix-pelemp { "(element)"}
    token infix-cff { "^ff"}
    token infix-cffc { "^ff^"}
    token infix-cfff { "^fff"}
    token infix-cfffc { "^fff^"}
    token infix-after { na}
    token infix-and { en}
    token infix-andthen { endan}
    token infix-before { voor}
    token infix-but { maar}
    token infix-cmp { vergelijk}
    token infix-coll { coll}
    token infix-div { deel}
    token infix-does { doet}
    token infix-eq { gelijk}
    token infix-ff { ff}
    token infix-ffc { "ff^"}
    token infix-fff { fff}
    token infix-fffc { "fff^"}
    token infix-gcd { gcd}
    token infix-ge { grotergelijk}
    token infix-gt { groter}
    token infix-lcm { lcm}
    token infix-le { kleinergelijk}
    token infix-leg { lgg}
    token infix-lt { kleiner}
    token infix-max { max}
    token infix-min { min}
    token infix-minmax { minmax}
    token infix-mod { modulo}
    token infix-ne { ongelijk}
    token infix-notandthen { nietendan}
    token infix-o { o}
    token infix-or { of}
    token infix-orelse { ofanders}
    token infix-unicmp { unicmp}
    token infix-x { x}
    token infix-X { X}
    token infix-xx { xx}
    token infix-Z { R}
    token meta-R { O}
    token meta-X { X}
    token meta-Z { R}
    token modifier-for { vooralle}
    token modifier-given { gegeven}
    token modifier-if { als}
    token modifier-unless { tenzij}
    token modifier-until { tot}
    token modifier-when { indien}
    token modifier-while { terwijl}
    token modifier-with { met}
    token modifier-without { zonder}
    token multi-multi { multi}
    token multi-only { alleen}
    token multi-proto { proto}
    token package-class { klasse}
    token package-grammar { grammatica}
    token package-module { module}
    token package-package { pakket}
    token package-role { rol}
    token phaser-BEGIN { BEGIN}
    token phaser-CATCH { VANGFOUT}
    token phaser-CHECK { CHECK}
    token phaser-CLOSE { CLOSE}
    token phaser-CONTROL { VANGBERICHT}
    token phaser-DOC { DOC}
    token phaser-END { EINDE}
    token phaser-ENTER { BINNENKOMST}
    token phaser-FIRST { EERSTE}
    token phaser-INIT { INIT}
    token phaser-KEEP { ACCEPTEER}
    token phaser-LAST { LAATSTE}
    token phaser-LEAVE { AFSCHEID}
    token phaser-NEXT { VOLGENDE}
    token phaser-POST { ACHTERAF}
    token phaser-PRE { VOORAF}
    token phaser-QUIT { STOP}
    token phaser-UNDO { NEGEER}
    token prefix-not { niet}
    token prefix-so { wel}
    token routine-method { methode}
    token routine-regex { regex}
    token routine-rule { regel}
    token routine-sub { sub}
    token routine-submethod { submethode}
    token routine-token { token}
    token scope-anon { anoniem}
    token scope-augment { verbeter}
    token scope-constant { constant}
    token scope-has { heeft}
    token scope-HAS { HEEFT}
    token scope-my { mijn}
    token scope-our { onze}
    token scope-state { steeds}
    token scope-unit { eenheid}
    token stmt-prefix-also { eveneens}
    token stmt-prefix-do { doe}
    token stmt-prefix-eager { vlijtig}
    token stmt-prefix-gather { verzamel}
    token stmt-prefix-hyper { hyper}
    token stmt-prefix-lazy { lui}
    token stmt-prefix-quietly { stilletjes}
    token stmt-prefix-race { race}
    token stmt-prefix-react { reageer}
    token stmt-prefix-sink { zink}
    token stmt-prefix-start { start}
    token stmt-prefix-supply { lever}
    token stmt-prefix-try { probeer}
    token term-nano { nano}
    token term-now { nu}
    token term-pi { pi}
    token term-rand { willekeurig}
    token term-self { zelf}
    token term-tau { tau}
    token term-time { tijd}
    token traitmod-does { doet}
    token traitmod-handles { begrijpt}
    token traitmod-hides { verbergt}
    token traitmod-is { is}
    token traitmod-of { netals}
    token traitmod-returns { geeftterug}
    token traitmod-trusts { vertrouwd}
    token typer-enum { enum}
    token typer-subset { subset}
    token use-import { importeer}
    token use-need { behoeft}
    token use-no { geen}
    token use-require { require}
    token use-use { gebruik}
    method core2ast {
        my %mapping = "alle", "all", "antiparen", "antipairs", "elke", "any", "voeg-achteraan", "append", "wacht-op", "await", "tas", "bag", "geef-op", "bail-out", "zegen", "bless", "kan-ok", "can-ok", "categoriseer", "categorize", "plafond", "ceiling", "letters", "chars", "kap-regeleinde", "chomp", "kap", "chop", "als-letter", "chr", "als-letters", "chrs", "classificeer", "classify", "sluit", "close", "kam", "comb", "combinaties", "combinations", "bevat", "contains", "kruis", "cross", "decodeer", "decode", "diep-arrangeer", "deepmap", "gedefinieerd", "defined", "sterf", "die", "klaar", "done", "duik-arrangeer", "duckmap", "elementen", "elems", "zend", "emit", "codeer", "encode", "einde", "end", "eindigt-met", "ends-with", "eval-sterft-ok", "eval-dies-ok", "eval-leeft-ok", "eval-lives-ok", "verlaat", "exit", "faal", "fail", "faalt-als", "fails-like", "vouw-kast", "fc", "eerste", "first", "plat", "flat", "draaiom", "flip", "vloer", "floor", "pak", "get", "pakc", "getc", "kern", "gist", "filter", "grep", "moes", "hash", "hoofd", "head", "indenteer", "indent", "is-ongeveer", "is-approx", "is-helemaal", "is-deeply", "is-priem", "is-prime", "iseen-ok", "isa-ok", "isniet", "isnt", "plak", "join", "sleutel", "key", "sleutels", "keys", "sw", "kv", "laatste", "last", "laatste-aanroep", "lastcall", "onder-kast", "lc", "lijkt-op", "like", "regels", "lines", "koppeling", "link", "lijst", "list", "leeft-ok", "lives-ok", "minst-belangrijke-bit", "lsb", "maak", "make", "arrangeer", "map", "verplaats", "move", "belangrijkste-bit", "msb", "nieuw", "new", "volgende", "next", "niet-ok", "nok", "geen", "none", "niet", "not", "merk-op", "note", "een", "one", "als-getal", "ord", "als-getallen", "ords", "paar", "pair", "paren", "pairs", "ontleed-nummer", "parse-base", "goed", "pass", "permutaties", "permutations", "kies", "pick", "verwacht", "plan", "floep", "pop", "voeg-voor", "prepend", "druk", "print", "drukf", "printf", "ga-door", "proceed", "vraag", "prompt", "stapel-op", "push", "zeg-het", "put", "willkeurig", "rand", "nog-eens", "redo", "reduceer", "reduce", "herhaaldelijk", "repeated", "retour", "return", "retour-rw", "return-rw", "keer-om", "reverse", "om-index", "rindex", "gooi", "roll", "wortels", "roots", "roteer", "rotate", "rond-af", "round", "ieder-een", "roundrobin", "voer-uit", "run", "zelfde-kast", "samecase", "zelfde-accent", "samemark", "zelfde-met", "samewith", "zeg", "say", "verzameling", "set", "onderuit", "shift", "teken", "sign", "signaal", "signal", "sla-over", "skip", "sla-rest-over", "skip-rest", "slaap", "sleep", "wekker", "sleep-timer", "slaap-tot", "sleep-until", "glip", "slip", "knip", "snip", "spiek", "snitch", "dus", "so", "sorteer", "sort", "splits-lijst", "splice", "splits-letters", "split", "sdrukf", "sprintf", "spuit", "spurt", "wortel", "sqrt", "plet", "squish", "zo-willekeurig", "srand", "begint-met", "starts-with", "substr-gelijk", "substr-eq", "slaag", "succeed", "sommeer", "sum", "symbolische-koppeling", "symlink", "staart", "tail", "neem", "take", "neem-rw", "take-rw", "titel-kast", "tc", "titel-onder-kast", "tclc", "knalt-als", "throws-like", "tedoen", "todo", "vertaal", "trans", "trim-vooraan", "trim-leading", "trim-achteraan", "trim-trailing", "kap-af", "truncate", "boven-kast", "uc", "uniek", "unique", "lijkt-niet-als", "unlike", "ontkoppel", "unlink", "onderin", "unshift", "gebruik-ok", "use-ok", "als-nummers", "val", "waarde", "value", "waardes", "values", "waarschuw", "warn", "woord-kast", "wordcase", "woorden", "words", "rits", "zip";
        my $ast := self.ast();
        if %mapping{$ast.simple-identifier()} -> $original {
            RakuAST::Name.from-identifier($original)
        }
        else {
            $ast
        }
    }
    method trait-is2ast {
        my %mapping = "kopie", "copy", "verstek", "default", "VEROUDERD", "DEPRECATED", "equivalent", "equiv", "implementatie-detail", "implementation-detail", "losser", "looser", "puur", "pure", "rauw", "raw", "lezen-schrijven", "rw", "test-assertie", "test-assertion", "strakker", "tighter";
        my $ast := self.ast();
        if %mapping{$ast.simple-identifier()} -> $original {
            RakuAST::Name.from-identifier($original)
        }
        else {
            $ast
        }
    }
    method adverb-pc2str (str $key) {
        my %mapping = "verwijder", "delete", "bestaat", "exists", "s", "k", "sw", "kv", "w", "v";
        %mapping{$key} // $key
    }
    method adverb-rx2str (str $key) {
        my %mapping = "continueer", "continue", "ui", "ex", "uitputtend", "exhaustive", "globaal", "global", "nk", "i", "negeerkast", "ignorecase", "negeeraccent", "ignoremark", "zk", "ii", "na", "m", "za", "mm", "de", "nd", "de", "nth", "ratel", "ratchet", "de", "rd", "zelfdekast", "samecase", "zelfdeaccent", "samemark", "zelfdespatie", "samespace", "sigspatie", "sigspace", "zs", "ss", "ste", "st", "tot", "to", "keer", "x";
        %mapping{$key} // $key
    }
    method named2str (str $key) {
        my %mapping = "absoluut", "absolute", "acties", "actions", "voegtoe", "append", "volledig", "completely", "continueer", "continue", "aantal", "count", "datum", "date", "dag", "day", "verwijder", "delete", "elementen", "elems", "vaneinde", "end", "elke", "every", "ui", "ex", "exclusief", "exclusive", "uitputtend", "exhaustive", "verloopt", "expires", "globaal", "global", "uur", "hour", "nk", "i", "negeerkast", "ignorecase", "negeeraccent", "ignoremark", "zk", "ii", "s", "k", "sleutel", "key", "sw", "kv", "luister", "listen", "minuut", "minute", "za", "mm", "maand", "month", "naam", "name", "de", "nd", "de", "nth", "uit", "off", "gedeeltelijk", "partial", "delen", "parts", "de", "rd", "vervanging", "replacement", "zelfdekast", "samecase", "zelfdeaccent", "samemark", "zelfdespatie", "samespace", "seconde", "second", "seconden", "seconds", "grootte", "size", "drukplat", "squash", "zs", "ss", "ste", "st", "de", "th", "herhalingen", "times", "tijdzone", "timezone", "w", "v", "waarde", "value", "waar", "where", "keer", "x", "jaar", "year";
        %mapping{$key} // $key
    }
    method pragma2str (str $key) {
        my %mapping = "dynamische-ruimte", "dynamic-scope", "fataal", "fatal", "interne-functies", "internals", "aanroeper", "invocant", "bibliotheek", "lib", "regeleinde", "newline", "compilatie-vooraf", "precompilation", "zacht", "soft", "verloop", "trace", "variablen", "variables", "zorgen", "worries";
        %mapping{$key} // $key
    }
}

# The EXPORT sub that actually does the slanging
my sub EXPORT() {
    my $LANG := $*LANG;

    $LANG.define_slang('MAIN',
      $LANG.slang_grammar('MAIN').^mixin(L10N::NL),
      $LANG.slang_actions('MAIN')
    );

    BEGIN Map.new
}

#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of localization --------------------------------------

# vim: expandtab shiftwidth=4
