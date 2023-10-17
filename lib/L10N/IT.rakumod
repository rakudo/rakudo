# This file contains the Italian Slang of the Raku Programming Language

#- start of generated part of localization ------------------------------------
#- Generated on 2023-10-17T13:57:58+02:00 by tools/build/makeL10N.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

role L10N::IT {
    use experimental :rakuast;
    token block-default { predefinita}
    token block-else { altro}
    token block-elsif { "altro-se"}
    token block-for { per}
    token block-given { dato}
    token block-if { se}
    token block-loop { ciclo}
    token block-orwith { "o-con"}
    token block-repeat { ripeti}
    token block-unless { "salvo-che"}
    token block-until { finché}
    token block-when { quando}
    token block-whenever { "ogni-volta-che"}
    token block-while { mentre}
    token block-with { con}
    token block-without { senza}
    token constraint-where { dove}
    token infix-pcontp { "(cont)"}
    token infix-pelemp { "(elem)"}
    token infix-cff { "^ff"}
    token infix-cffc { "^ff^"}
    token infix-cfff { "^fff"}
    token infix-cfffc { "^fff^"}
    token infix-after { dopo}
    token infix-and { e}
    token infix-andthen { "e-poi"}
    token infix-before { "prima-di"}
    token infix-but { però}
    token infix-cmp { cmp}
    token infix-coll { coll}
    token infix-div { div}
    token infix-does { fa}
    token infix-eq { eq}
    token infix-ff { ff}
    token infix-ffc { "ff^"}
    token infix-fff { fff}
    token infix-fffc { "fff^"}
    token infix-gcd { gcd}
    token infix-ge { ge}
    token infix-gt { gt}
    token infix-lcm { lcm}
    token infix-le { le}
    token infix-leg { leg}
    token infix-lt { lt}
    token infix-max { max}
    token infix-min { min}
    token infix-minmax { minmax}
    token infix-mod { mod}
    token infix-ne { ne}
    token infix-notandthen { "no-e-poi"}
    token infix-o { c}
    token infix-or { o}
    token infix-orelse { oppure}
    token infix-unicmp { unicmp}
    token infix-x { x}
    token infix-X { X}
    token infix-xx { xx}
    token infix-Z { Z}
    token meta-R { R}
    token meta-X { X}
    token meta-Z { Z}
    token modifier-for { per}
    token modifier-given { dato}
    token modifier-if { se}
    token modifier-unless { "salvo-che"}
    token modifier-until { "fino-a"}
    token modifier-when { quando}
    token modifier-while { mentre}
    token modifier-with { con}
    token modifier-without { senza}
    token multi-multi { multi}
    token multi-only { soltanto}
    token multi-proto { proto}
    token package-class { classe}
    token package-grammar { grammatica}
    token package-module { modulo}
    token package-package { pacchetto}
    token package-role { ruolo}
    token phaser-BEGIN { COMINCIA}
    token phaser-CATCH { PRENDI}
    token phaser-CHECK { CONTROLLA}
    token phaser-CLOSE { CHIUDE}
    token phaser-CONTROL { CONTROLLO}
    token phaser-DOC { DOC}
    token phaser-END { FINE}
    token phaser-ENTER { ENTRA}
    token phaser-FIRST { PRIMA}
    token phaser-INIT { INIT}
    token phaser-KEEP { TIENI}
    token phaser-LAST { ULTIMO}
    token phaser-LEAVE { VATTENE}
    token phaser-NEXT { PROSSIMO}
    token phaser-POST { POST}
    token phaser-PRE { PRE}
    token phaser-QUIT { LASCIA}
    token phaser-UNDO { ANNULLA}
    token prefix-not { non}
    token prefix-so { così}
    token routine-method { metodo}
    token routine-regex { regex}
    token routine-rule { regola}
    token routine-sub { sub}
    token routine-submethod { submethod}
    token routine-token { gettone}
    token scope-anon { anon}
    token scope-augment { augment}
    token scope-constant { constante}
    token scope-has { ha}
    token scope-HAS { HA}
    token scope-my { "il-mio"}
    token scope-our { "il-nostro"}
    token scope-state { stato}
    token scope-unit { unità}
    token stmt-prefix-also { anche}
    token stmt-prefix-do { fate}
    token stmt-prefix-eager { impaziente}
    token stmt-prefix-gather { raccogliere}
    token stmt-prefix-hyper { hyper}
    token stmt-prefix-lazy { pigro}
    token stmt-prefix-quietly { tranquillamente}
    token stmt-prefix-race { gara}
    token stmt-prefix-react { reagisce}
    token stmt-prefix-sink { cala}
    token stmt-prefix-start { comincio}
    token stmt-prefix-supply { fornitura}
    token stmt-prefix-try { prova}
    token term-nano { nano}
    token term-now { gia}
    token term-pi { pi}
    token term-rand { rand}
    token term-self { "se-stesso"}
    token term-tau { tau}
    token term-time { tempo}
    token traitmod-does { fa}
    token traitmod-handles { gestisce}
    token traitmod-hides { nasconde}
    token traitmod-is { è}
    token traitmod-of { da}
    token traitmod-returns { ritorna}
    token traitmod-trusts { trusts}
    token typer-enum { enum}
    token typer-subset { subset}
    token use-import { importare}
    token use-need { bisognare}
    token use-no { non}
    token use-require { richiedere}
    token use-use { usare}
    method core2ast {
        my %mapping = "tutti", "all", "ogni", "any", "aggiungi", "append", "aspetta", "await", "borsa", "bag", "categorizzare", "categorize", "soffitto", "ceiling", "classificare", "classify", "chide", "close", "pettine", "comb", "combinazioni", "combinations", "mappa-profonda", "deepmap", "definito", "defined", "muori", "die", "fatto", "done", "mappa-anatra", "duckmap", "emettere", "emit", "fine", "end", "uscire", "exit", "fallire", "fail", "primo", "first", "piatto", "flat", "pavimento", "floor", "barriera-completa", "full-barrier", "prendi", "get", "essenza", "gist", "testa", "head", "indenta", "indent", "indice", "index", "indici", "indices", "articolo", "item", "unirsi", "join", "chiave", "key", "chiavi", "keys", "cv", "kv", "ultimo", "last", "ultima-chiamata", "lastcall", "linee", "lines", "elenco", "list", "fare", "make", "mappa", "map", "muove", "move", "prossimo", "next", "prossimo-chiamato", "nextcallee", "prossimo-esteso", "nextsame", "prossimo-con", "nextwith", "nessuno", "none", "non", "not", "nota", "note", "uno", "one", "aperto", "open", "paio", "pair", "pai", "pairs", "permutazioni", "permutations", "prendi", "pick", "stampa", "print", "f-stampa", "printf", "procedi", "proceed", "richiesta", "prompt", "spinge", "push", "metti", "put", "rifai", "redo", "riduci", "reduce", "ripetuto", "repeated", "restituisci", "return", "restituisci-rw", "return-rw", "stesso-caso", "samecase", "stessa-marca", "samemark", "stessa-con", "samewith", "dillo", "say", "sposta", "shift", "firma", "sign", "segno", "signal", "salta", "skip", "dormi", "sleep", "dormi-fino-a", "sleep-until", "scivola", "slip", "bevi", "slurp", "taglia", "snip", "fail-la-spia", "snitch", "così", "so", "ordina", "sort", "unisci", "splice", "divitevi", "split", "spruzza", "spurt", "schiaccia", "squish", "riuscirci", "succeed", "somma", "sum", "coda", "tail", "prendi", "take", "prendi-rw", "take-rw", "taglia", "trim", "taglia-in-testa", "trim-leading", "taglia-in-coda", "trim-trailing", "troncare", "truncate", "valore", "value", "valori", "values", "avviso", "warn", "caso-della-parola", "wordcase", "parole", "words";
        my $ast := self.ast();
        if %mapping{$ast.simple-identifier()} -> $original {
            RakuAST::Name.from-identifier($original)
        }
        else {
            $ast
        }
    }
    method trait-is2ast {
        self.ast()
    }
    method adverb-pc2str (str $key) {
        $key
    }
    method adverb-rx2str (str $key) {
        $key
    }
    method named2str (str $key) {
        $key
    }
    method pragma2str (str $key) {
        $key
    }
}

# The EXPORT sub that actually does the slanging
my sub EXPORT() {
    my $LANG := $*LANG;

    $LANG.define_slang('MAIN',
      $LANG.slang_grammar('MAIN').^mixin(L10N::IT),
      $LANG.slang_actions('MAIN')
    );

    BEGIN Map.new
}

#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of localization --------------------------------------

# vim: expandtab shiftwidth=4
