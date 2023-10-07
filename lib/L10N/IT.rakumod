# This file contains the Italian Slang of the Raku Programming Language

#- start of generated part of localization ------------------------------------
#- Generated on 2023-10-07T19:28:02+02:00 by tools/build/makeL10N.raku
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
    token infix-X { X}
    token infix-Z { Z}
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
    token infix-xx { xx}
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
    token package-knowhow { "so-come"}
    token package-module { modulo}
    token package-native { nativo}
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
    token scope-HAS { HA}
    token scope-anon { anon}
    token scope-augment { augment}
    token scope-constant { constante}
    token scope-has { ha}
    token scope-my { "il-mio"}
    token scope-our { "il-nostro"}
    token scope-state { stato}
    token scope-supersede { sostituire}
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
    token term-rand { rand}
    token term-self { "se-stesso"}
    token term-time { tempo}
    token trait-does { fa}
    token trait-handles { gestisce}
    token trait-hides { nasconde}
    token trait-is { è}
    token trait-of { da}
    token trait-returns { ritorna}
    token typer-enum { enum}
    token typer-subset { subset}
    token use-import { importare}
    token use-need { bisognare}
    token use-no { non}
    token use-require { richiedere}
    token use-use { usare}
    method xlated2ast {
        my constant %core = "abs", "abs", "tutti", "all", "ogni", "any", "aggiungi", "append", "ast", "ast", "atomic-add-fetch", "atomic-add-fetch", "atomic-assign", "atomic-assign", "atomic-dec-fetch", "atomic-dec-fetch", "atomic-fetch", "atomic-fetch", "atomic-fetch-add", "atomic-fetch-add", "atomic-fetch-dec", "atomic-fetch-dec", "atomic-fetch-inc", "atomic-fetch-inc", "atomic-fetch-sub", "atomic-fetch-sub", "atomic-inc-fetch", "atomic-inc-fetch", "atomic-sub-fetch", "atomic-sub-fetch", "aspetta", "await", "borsa", "bag", "bail-out", "bail-out", "bless", "bless", "callframe", "callframe", "callsame", "callsame", "callwith", "callwith", "can-ok", "can-ok", "cas", "cas", "categorizzare", "categorize", "soffitto", "ceiling", "chars", "chars", "chdir", "chdir", "chmod", "chmod", "chomp", "chomp", "chop", "chop", "chown", "chown", "chr", "chr", "chrs", "chrs", "classificare", "classify", "chide", "close", "cmp-ok", "cmp-ok", "pettine", "comb", "combinazioni", "combinations", "cross", "cross", "mappa-profonda", "deepmap", "definito", "defined", "diag", "diag", "muori", "die", "dies-ok", "dies-ok", "dir", "dir", "does-ok", "does-ok", "fatto", "done", "mappa-anatra", "duckmap", "elems", "elems", "emettere", "emit", "fine", "end", "eval-dies-ok", "eval-dies-ok", "eval-lives-ok", "eval-lives-ok", "uscire", "exit", "exp", "exp", "expmod", "expmod", "fallire", "fail", "fails-like", "fails-like", "fc", "fc", "primo", "first", "piatto", "flat", "flip", "flip", "pavimento", "floor", "flunk", "flunk", "barriera-completa", "full-barrier", "prendi", "get", "getc", "getc", "essenza", "gist", "grep", "grep", "hash", "hash", "testa", "head", "indenta", "indent", "indice", "index", "indici", "indices", "indir", "indir", "is", "is", "is-approx", "is-approx", "is-deeply", "is-deeply", "isa-ok", "isa-ok", "isnt", "isnt", "articolo", "item", "unirsi", "join", "chiave", "key", "chiavi", "keys", "cv", "kv", "ultimo", "last", "ultima-chiamata", "lastcall", "lc", "lc", "like", "like", "linee", "lines", "link", "link", "elenco", "list", "lives-ok", "lives-ok", "lsb", "lsb", "fare", "make", "mappa", "map", "max", "max", "min", "min", "minmax", "minmax", "mix", "mix", "mkdir", "mkdir", "muove", "move", "msb", "msb", "prossimo", "next", "prossimo-chiamato", "nextcallee", "prossimo-esteso", "nextsame", "prossimo-con", "nextwith", "nok", "nok", "nessuno", "none", "non", "not", "nota", "note", "ok", "ok", "uno", "one", "aperto", "open", "ord", "ord", "ords", "ords", "paio", "pair", "pai", "pairs", "parse-base", "parse-base", "pass", "pass", "permutazioni", "permutations", "prendi", "pick", "plan", "plan", "pop", "pop", "prepend", "prepend", "stampa", "print", "f-stampa", "printf", "procedi", "proceed", "richiesta", "prompt", "spinge", "push", "metti", "put", "rand", "rand", "rifai", "redo", "riduci", "reduce", "ripetuto", "repeated", "repl", "repl", "restituisci", "return", "return-rw", "return-rw", "reverse", "reverse", "rindex", "rindex", "rmdir", "rmdir", "roll", "roll", "rotate", "rotate", "round", "round", "roundrobin", "roundrobin", "run", "run", "stesso-caso", "samecase", "stessa-marca", "samemark", "stessa-con", "samewith", "dillo", "say", "set", "set", "shell", "shell", "sposta", "shift", "firma", "sign", "segno", "signal", "salta", "skip", "skip-rest", "skip-rest", "dormi", "sleep", "sleep-timer", "sleep-timer", "dormi-fino-a", "sleep-until", "scivola", "slip", "bevi", "slurp", "taglia", "snip", "fail-la-spia", "snitch", "così", "so", "ordina", "sort", "unisci", "splice", "divitevi", "split", "sprintf", "sprintf", "spruzza", "spurt", "sqrt", "sqrt", "schiaccia", "squish", "srand", "srand", "subbuf", "subbuf", "subbuf-rw", "subbuf-rw", "subtest", "subtest", "riuscirci", "succeed", "somma", "sum", "symlink", "symlink", "coda", "tail", "prendi", "take", "prendi-rw", "take-rw", "tc", "tc", "tclc", "tclc", "throws-like", "throws-like", "todo", "todo", "taglia", "trim", "taglia-in-testa", "trim-leading", "taglia-in-coda", "trim-trailing", "troncare", "truncate", "uc", "uc", "unimatch", "unimatch", "uniname", "uniname", "uninames", "uninames", "uniparse", "uniparse", "uniprop", "uniprop", "uniprops", "uniprops", "unique", "unique", "unival", "unival", "univals", "univals", "unlike", "unlike", "unlink", "unlink", "unshift", "unshift", "use-ok", "use-ok", "val", "val", "valore", "value", "valori", "values", "avviso", "warn", "caso-della-parola", "wordcase", "parole", "words", "zip", "zip";
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
      $LANG.slang_grammar('MAIN').^mixin(L10N::IT),
      $LANG.slang_actions('MAIN')
    );

    BEGIN Map.new
}

#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of localization --------------------------------------

# vim: expandtab shiftwidth=4
