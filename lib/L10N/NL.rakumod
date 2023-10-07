# This file contains the Dutch Slang of the Raku Programming Language

#- start of generated part of localization ------------------------------------
#- Generated on 2023-10-07T11:18:51+02:00 by tools/build/makeL10N.raku
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
    token infix-X { X}
    token infix-Z { Z}
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
    token infix-xx { xx}
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
    token package-knowhow { knowhow}
    token package-module { module}
    token package-native { native}
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
    token rakudoc-alias { alias}
    token rakudoc-begin { begin}
    token rakudoc-column { column}
    token rakudoc-config { config}
    token rakudoc-end { end}
    token rakudoc-finish { finish}
    token rakudoc-for { for}
    token rakudoc-row { row}
    token routine-method { methode}
    token routine-regex { regex}
    token routine-rule { regel}
    token routine-sub { sub}
    token routine-submethod { submethode}
    token routine-token { token}
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
    token typer-enum { enum}
    token typer-subset { subset}
    token use-import { importeer}
    token use-need { behoeft}
    token use-no { geen}
    token use-require { require}
    token use-use { gebruik}
    method xlated2ast {
        my constant %core = "abs", "abs", "alle", "all", "elke", "any", "voeg-achteraan", "append", "ast", "ast", "atomic-add-fetch", "atomic-add-fetch", "atomic-assign", "atomic-assign", "atomic-dec-fetch", "atomic-dec-fetch", "atomic-fetch", "atomic-fetch", "atomic-fetch-add", "atomic-fetch-add", "atomic-fetch-dec", "atomic-fetch-dec", "atomic-fetch-inc", "atomic-fetch-inc", "atomic-fetch-sub", "atomic-fetch-sub", "atomic-inc-fetch", "atomic-inc-fetch", "atomic-sub-fetch", "atomic-sub-fetch", "wacht-op", "await", "tas", "bag", "bless", "bless", "callframe", "callframe", "callsame", "callsame", "callwith", "callwith", "cas", "cas", "categoriseer", "categorize", "plafond", "ceiling", "letters", "chars", "chdir", "chdir", "chmod", "chmod", "kap-regeleinde", "chomp", "kap", "chop", "chown", "chown", "als-letter", "chr", "als-letters", "chrs", "classificeer", "classify", "sluit", "close", "kam", "comb", "combinaties", "combinations", "diep-arrangeer", "deepmap", "gedefinieerd", "defined", "sterf", "die", "dir", "dir", "klaar", "done", "duik-arrangeer", "duckmap", "elementen", "elems", "zend", "emit", "einde", "end", "verlaat", "exit", "exp", "exp", "expmod", "expmod", "faal", "fail", "vouw-kast", "fc", "eerste", "first", "plat", "flat", "draaiom", "flip", "vloer", "floor", "full-barrier", "full-barrier", "pak", "get", "pakc", "getc", "kern", "gist", "filter", "grep", "moes", "hash", "hoofd", "head", "indenteer", "indent", "index", "index", "indices", "indices", "indir", "indir", "item", "item", "plak", "join", "sleutel", "key", "sleutels", "keys", "sw", "kv", "laatste", "last", "laatste-aanroep", "lastcall", "onder-kast", "lc", "regels", "lines", "koppeling", "link", "lijst", "list", "lsb", "lsb", "maak", "make", "arrangeer", "map", "max", "max", "min", "min", "minmax", "minmax", "mix", "mix", "mkdir", "mkdir", "verplaats", "move", "msb", "msb", "volgende", "next", "nextcallee", "nextcallee", "nextsame", "nextsame", "nextwith", "nextwith", "geen", "none", "niet", "not", "merk-op", "note", "een", "one", "open", "open", "als-getal", "ord", "als-getallen", "ords", "paar", "pair", "paren", "pairs", "parse-base", "parse-base", "permutaties", "permutations", "kies", "pick", "pop", "pop", "voeg-voor", "prepend", "druk", "print", "drukf", "printf", "ga-door", "proceed", "vraag", "prompt", "stapel-op", "push", "zeg-het", "put", "willkeurig", "rand", "nog-eens", "redo", "reduceer", "reduce", "herhaaldelijk", "repeated", "repl", "repl", "retour", "return", "retour-rw", "return-rw", "keer-om", "reverse", "om-index", "rindex", "rmdir", "rmdir", "gooi", "roll", "roteer", "rotate", "rond-af", "round", "ieder-een", "roundrobin", "voer-uit", "run", "zelfde-kast", "samecase", "zelfde-accent", "samemark", "zelfde-met", "samewith", "zeg", "say", "verzameling", "set", "shell", "shell", "onderuit", "shift", "teken", "sign", "signaal", "signal", "sla-over", "skip", "slaap", "sleep", "wekker", "sleep-timer", "slaap-tot", "sleep-until", "glip", "slip", "slurp", "slurp", "knip", "snip", "spiek", "snitch", "dus", "so", "sorteer", "sort", "splits-lijst", "splice", "splits-letters", "split", "sdrukf", "sprintf", "spuit", "spurt", "wortel", "sqrt", "plet", "squish", "zo-willekeurig", "srand", "subbuf", "subbuf", "subbuf-rw", "subbuf-rw", "slaag", "succeed", "sommeer", "sum", "symbolische-koppeling", "symlink", "staart", "tail", "neem", "take", "neem-rw", "take-rw", "titel-kast", "tc", "titel-onder-kast", "tclc", "trim", "trim", "trim-vooraan", "trim-leading", "trim-achteraan", "trim-trailing", "kap-af", "truncate", "boven-kast", "uc", "unimatch", "unimatch", "uniname", "uniname", "uninames", "uninames", "uniparse", "uniparse", "uniprop", "uniprop", "uniprops", "uniprops", "uniek", "unique", "unival", "unival", "univals", "univals", "ontkoppel", "unlink", "onderin", "unshift", "als-nummers", "val", "waarde", "value", "waardes", "values", "waarschuw", "warn", "woord-kast", "wordcase", "woorden", "words";
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
      $LANG.slang_grammar('MAIN').^mixin(L10N::NL),
      $LANG.slang_actions('MAIN')
    );

    BEGIN Map.new
}

#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of localization --------------------------------------

# vim: expandtab shiftwidth=4
