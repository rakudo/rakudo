# This file contains the French Slang of the Raku Programming Language

#- start of generated part of localization ------------------------------------
#- Generated on 2023-10-22T12:04:39+02:00 by tools/build/makeL10N.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

role L10N::FR {
    use experimental :rakuast;
    token block-default { défaut}
    token block-else { sinon}
    token block-elsif { ousi}
    token block-for { pour}
    token block-given { étantdonné}
    token block-if { si}
    token block-loop { boucle}
    token block-orwith { ouavec}
    token block-repeat { répète}
    token block-unless { saufsi}
    token block-until { jusqua}
    token block-when { quand}
    token block-whenever { lorsque}
    token block-while { tantque}
    token block-with { avec}
    token block-without { sans}
    token constraint-where { où}
    token infix-pcontp { "(cont)"}
    token infix-pelemp { "(elem)"}
    token infix-cff { "^ff"}
    token infix-cffc { "^ff^"}
    token infix-cfff { "^fff"}
    token infix-cfffc { "^fff^"}
    token infix-after { après}
    token infix-and { et}
    token infix-andthen { etalors}
    token infix-before { avant}
    token infix-but { mais}
    token infix-cmp { cmp}
    token infix-coll { coll}
    token infix-div { div}
    token infix-does { fait}
    token infix-eq { eq}
    token infix-ff { ff}
    token infix-ffc { "ff^"}
    token infix-fff { fff}
    token infix-fffc { "fff^"}
    token infix-gcd { pgcd}
    token infix-ge { pge}
    token infix-gt { pg}
    token infix-lcm { ppcm}
    token infix-le { ppe}
    token infix-leg { ppepg}
    token infix-lt { pp}
    token infix-max { max}
    token infix-min { min}
    token infix-minmax { minmax}
    token infix-mod { mod}
    token infix-ne { pe}
    token infix-notandthen { pasetalors}
    token infix-o { o}
    token infix-or { ou}
    token infix-orelse { oualors}
    token infix-unicmp { unicmp}
    token infix-x { x}
    token infix-X { X}
    token infix-xx { xx}
    token infix-Z { Z}
    token meta-R { R}
    token meta-X { X}
    token meta-Z { Z}
    token modifier-for { pour}
    token modifier-given { étantdonné}
    token modifier-if { si}
    token modifier-unless { saufsi}
    token modifier-until { jusquà}
    token modifier-when { quand}
    token modifier-while { tansque}
    token modifier-with { avec}
    token modifier-without { sans}
    token multi-multi { multi}
    token multi-only { seulement}
    token multi-proto { proto}
    token package-class { class}
    token package-grammar { grammaire}
    token package-module { module}
    token package-native { natif}
    token package-package { paquet}
    token package-role { rôle}
    token phaser-BEGIN { DÉBUT}
    token phaser-CATCH { CATCH}
    token phaser-CHECK { VERIF}
    token phaser-CLOSE { FERM}
    token phaser-CONTROL { CONTROLE}
    token phaser-DOC { DOC}
    token phaser-END { FIN}
    token phaser-ENTER { ENTRÉE}
    token phaser-FIRST { DABORD}
    token phaser-INIT { INIT}
    token phaser-KEEP { GARDE}
    token phaser-LAST { FINAL}
    token phaser-LEAVE { SORS}
    token phaser-NEXT { SUIVANT}
    token phaser-POST { POST}
    token phaser-PRE { PRE}
    token phaser-QUIT { QUITTE}
    token phaser-UNDO { DÉFAIRE}
    token prefix-not { pas}
    token prefix-so { donc}
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
    token routine-method { méthode}
    token routine-regex { regex}
    token routine-rule { règle}
    token routine-sub { sub}
    token routine-submethod { sousméthode}
    token routine-token { jeton}
    token scope-anon { anon}
    token scope-augment { augmente}
    token scope-constant { constante}
    token scope-has { a}
    token scope-HAS { A}
    token scope-my { ma}
    token scope-our { notre}
    token scope-state { state}
    token scope-supersede { supplante}
    token scope-unit { unité}
    token stmt-prefix-also { aussi}
    token stmt-prefix-do { fait}
    token stmt-prefix-eager { impatient}
    token stmt-prefix-gather { collecte}
    token stmt-prefix-hyper { hyper}
    token stmt-prefix-lazy { nonchalamment}
    token stmt-prefix-quietly { silencieusement}
    token stmt-prefix-race { course}
    token stmt-prefix-react { réagis}
    token stmt-prefix-sink { plonge}
    token stmt-prefix-start { commence}
    token stmt-prefix-supply { fournis}
    token stmt-prefix-try { essaie}
    token term-nano { nano}
    token term-now { maintenant}
    token term-pi { pi}
    token term-rand { rand}
    token term-self { sois}
    token term-tau { tau}
    token term-time { temps}
    token traitmod-does { fait}
    token traitmod-handles { gère}
    token traitmod-hides { dissimule}
    token traitmod-is { est}
    token traitmod-of { de}
    token traitmod-returns { renvoie}
    token traitmod-trusts { trusts}
    token typer-enum { énum}
    token typer-subset { sousens}
    token use-import { importe}
    token use-need { abesoin}
    token use-no { non}
    token use-require { requiert}
    token use-use { utilise}
    method core2ast {
        my %mapping = "tout", "all", "nimporte", "any", "ajoute", "append", "asa", "ast", "attends", "await", "sac", "bag", "escampe", "bail-out", "bénit", "bless", "appelmeme", "callsame", "appelavec", "callwith", "peut-passable", "can-ok", "plafond", "ceiling", "cars", "chars", "chrep", "chdir", "classifie", "classify", "ferm", "close", "combinaisons", "combinations", "défini", "defined", "meurs", "die", "meurt-passable", "dies-ok", "rep", "dir", "fait-passable", "does-ok", "fait", "done", "élems", "elems", "émit", "emit", "fin", "end", "sortie", "exit", "échoue", "fail", "premier", "first", "applati", "flat", "retourne", "flip", "plancher", "floor", "rate", "flunk", "barrière-totale", "full-barrier", "dict", "hash", "tête", "head", "est", "is", "est-environ", "is-approx", "est-profondément", "is-deeply", "estun-passable", "isa-ok", "nestpas", "isnt", "article", "item", "joins", "join", "clef", "key", "clefs", "keys", "cv", "kv", "dernier", "last", "dernierappel", "lastcall", "mi", "lc", "comme", "like", "lignes", "lines", "lien", "link", "liste", "list", "vit-passable", "lives-ok", "bms", "lsb", "fais", "make", "mkrep", "mkdir", "bouge", "move", "bps", "msb", "nouveau", "new", "suivant", "next", "suivantappelé", "nextcallee", "suivantmeme", "nextsame", "suivantavec", "nextwith", "nonpassable", "nok", "aucun", "none", "pas", "not", "passable", "ok", "un", "one", "ouvre", "open", "paire", "pair", "paires", "pairs", "passe", "pass", "choisis", "pick", "éclos", "pop", "préfixe", "prepend", "imprime", "print", "imprimef", "printf", "poursuis", "proceed", "demande", "prompt", "pousse", "push", "mets", "put", "refais", "redo", "réduis", "reduce", "répété", "repeated", "renvoie", "return", "renvoie-le", "return-rw", "renverse", "reverse", "sprep", "rmdir", "tire", "roll", "tourne", "rotate", "rond", "round", "tourniquet", "roundrobin", "lance", "run", "memecasse", "samecase", "samediac", "samemark", "memeavec", "samewith", "dis", "say", "ens", "set", "coque", "shell", "décale", "shift", "signe", "sign", "signale", "signal", "omets", "skip", "omets-reste", "skip-rest", "dors", "sleep", "dors-minuteur", "sleep-timer", "dors-jusquà", "sleep-until", "enfile", "slip", "engloutis", "slurp", "découpe", "snip", "dénonce", "snitch", "donc", "so", "trie", "sort", "noue", "splice", "coupe", "split", "cimprmf", "sprintf", "gicle", "spurt", "racc", "sqrt", "soustampon", "subbuf", "soustampon-le", "subbuf-rw", "soustest", "subtest", "réussis", "succeed", "somme", "sum", "liensymb", "symlink", "queue", "tail", "prends", "take", "prends-le", "take-rw", "jete-comme", "throws-like", "àfaire", "todo", "rogne", "trim", "rogne-gauche", "trim-leading", "rogne-droite", "trim-trailing", "tronque", "truncate", "préviens", "warn", "motcasse", "wordcase", "mots", "words";
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
    method adverb-q2str (str $key) {
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
      $LANG.slang_grammar('MAIN').^mixin(L10N::FR),
      $LANG.slang_actions('MAIN')
    );

    BEGIN Map.new
}

#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of localization --------------------------------------

# vim: expandtab shiftwidth=4
