# This file contains the Dutch Slang of the Raku Programming Language

#- start of generated part of localization ------------------------------------
#- Generated on 2023-10-04T16:44:17+02:00 by tools/build/makeL10N.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

my role NL is export {
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
    token infix-after { na}
    token infix-and { en}
    token infix-andthen { endan}
    token infix-before { voor}
    token infix-but { maar}
    token infix-cmp { vergelijk}
    token infix-div { deel}
    token infix-does { doet}
    token infix-eq { gelijk}
    token infix-ge { grotergelijk}
    token infix-gt { groter}
    token infix-le { kleinergelijk}
    token infix-leg { lgg}
    token infix-lt { kleiner}
    token infix-mod { modulo}
    token infix-ne { ongelijk}
    token infix-notandthen { nietendan}
    token infix-or { of}
    token infix-orelse { ofanders}
    token meta-R { O}
    token modifier-for { vooralle}
    token modifier-given { gegeven}
    token modifier-if { als}
    token modifier-unless { tenzij}
    token modifier-until { tot}
    token modifier-when { indien}
    token modifier-while { terwijl}
    token modifier-with { met}
    token modifier-without { zonder}
    token multi-only { alleen}
    token package-class { klasse}
    token package-grammar { grammatica}
    token package-module { module}
    token package-package { pakket}
    token package-role { rol}
    token phaser-CATCH { VANGFOUT}
    token phaser-CLOSE { CLOSE}
    token phaser-CONTROL { VANGBERICHT}
    token phaser-END { EINDE}
    token phaser-ENTER { BINNENKOMST}
    token phaser-FIRST { EERSTE}
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
    token routine-rule { regel}
    token routine-submethod { submethode}
    token routine-token { merkteken}
    token scope-HAS { HEEFT}
    token scope-anon { anoniem}
    token scope-augment { verbeter}
    token scope-constant { constant}
    token scope-has { heeft}
    token scope-my { mijn}
    token scope-our { onze}
    token scope-state { steeds}
    token scope-supersede { vervang}
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
    token term-rand { willekeurig}
    token term-self { zelf}
    token term-time { tijd}
    token trait-does { doet}
    token trait-handles { begrijpt}
    token trait-hides { verbergt}
    token trait-is { is}
    token trait-of { netals}
    token trait-returns { geeftterug}
    token use-import { importeer}
    token use-need { behoeft}
    token use-no { geen}
    token use-use { gebruik}
    method xlated2ast {
        my constant %core = "alle", "all", "elke", "any", "voeg-achteraan", "append", "wacht-op", "await", "tas", "bag", "categoriseer", "categorize", "plafond", "ceiling", "letters", "chars", "kap-regeleinde", "chomp", "kap", "chop", "als-letter", "chr", "als-letters", "chrs", "classificeer", "classify", "sluit", "close", "kam", "comb", "combinaties", "combinations", "diep-arrangeer", "deepmap", "gedefinieerd", "defined", "sterf", "die", "klaar", "done", "duik-arrangeer", "duckmap", "elementen", "elems", "zend", "emit", "einde", "end", "verlaat", "exit", "faal", "fail", "vouw-kast", "fc", "eerste", "first", "plat", "flat", "draaiom", "flip", "vloer", "floor", "pak", "get", "pakc", "getc", "kern", "gist", "filter", "grep", "moes", "hash", "hoofd", "head", "indenteer", "indent", "plak", "join", "sleutel", "key", "sleutels", "keys", "sw", "kv", "laatste", "last", "laatste-aanroep", "lastcall", "onder-kast", "lc", "regels", "lines", "koppeling", "link", "lijst", "list", "maak", "make", "arrangeer", "map", "verplaats", "move", "volgende", "next", "geen", "none", "niet", "not", "merk-op", "note", "een", "one", "als-getal", "ord", "als-getallen", "ords", "paar", "pair", "paren", "pairs", "permutaties", "permutations", "kies", "pick", "voeg-voor", "prepend", "druk", "print", "drukf", "printf", "ga-door", "proceed", "vraag", "prompt", "stapel-op", "push", "zeg-het", "put", "willkeurig", "rand", "nog-eens", "redo", "reduceer", "reduce", "herhaaldelijk", "repeated", "retour", "return", "retour-rw", "return-rw", "keer-om", "reverse", "om-index", "rindex", "gooi", "roll", "roteer", "rotate", "rond-af", "round", "ieder-een", "roundrobin", "voer-uit", "run", "zelfde-kast", "samecase", "zelfde-accent", "samemark", "zeg", "say", "verzameling", "set", "onderuit", "shift", "teken", "sign", "signaal", "signal", "sla-over", "skip", "slaap", "sleep", "wekker", "sleep-timer", "slaap-tot", "sleep-until", "glip", "slip", "knip", "snip", "spiek", "snitch", "dus", "so", "sorteer", "sort", "splits-lijst", "splice", "splits-letters", "split", "sdrukf", "sprintf", "spuit", "spurt", "wortel", "sqrt", "plet", "squish", "zo-willekeurig", "srand", "slaag", "succeed", "sommeer", "sum", "symbolische-koppeling", "symlink", "staart", "tail", "neem", "take", "neem-rw", "take-rw", "titel-kast", "tc", "titel-onder-kast", "tclc", "trim", "trim", "trim-vooraan", "trim-leading", "trim-achteraan", "trim-trailing", "kap-af", "truncate", "boven-kast", "uc", "uniek", "unique", "ontkoppel", "unlink", "onderin", "unshift", "als-nummers", "val", "waarde", "value", "waardes", "values", "waarschuw", "warn", "woord-kast", "wordcase", "woorden", "words";
        my $ast := self.ast();
        if %core{$ast.simple-identifier()} -> $original {
            RakuAST::Name.from-identifier($original)
        }
        else {
            $ast
        }
    }
}

# The EXPORT sub that actually does the slanging
my sub EXPORT() {
    my $LANG := $*LANG;

    $LANG.define_slang('MAIN',
      $LANG.slang_grammar('MAIN').^mixin(NL),
      $LANG.slang_actions('MAIN')
    );

    BEGIN Map.new
}

#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of localization --------------------------------------

# vim: expandtab shiftwidth=4
