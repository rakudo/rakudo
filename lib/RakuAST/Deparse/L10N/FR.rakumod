# This file contains the French deparsing logic for the Raku
# Programming Language.

#- start of generated part of localization ------------------------------------
#- Generated on 2023-10-29T01:51:53Z by tools/build/makeL10N.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

my %xlation = "block-default", "défaut", "block-else", "sinon", "block-elsif", "ousi", "block-for", "pour", "block-given", "étantdonné", "block-if", "si", "block-loop", "boucle", "block-orwith", "ouavec", "block-repeat", "répète", "block-unless", "saufsi", "block-until", "jusqua", "block-when", "quand", "block-whenever", "lorsque", "block-while", "tantque", "block-with", "avec", "block-without", "sans", "constraint-where", "où", "core-all", "tout", "core-any", "nimporte", "core-append", "ajoute", "core-ast", "asa", "core-await", "attends", "core-bag", "sac", "core-bail-out", "escampe", "core-bless", "bénit", "core-callsame", "appelmeme", "core-callwith", "appelavec", "core-can-ok", "peut-passable", "core-ceiling", "plafond", "core-chars", "cars", "core-chdir", "chrep", "core-classify", "classifie", "core-close", "ferm", "core-combinations", "combinaisons", "core-defined", "défini", "core-die", "meurs", "core-dies-ok", "meurt-passable", "core-dir", "rep", "core-does-ok", "fait-passable", "core-done", "fait", "core-elems", "élems", "core-emit", "émit", "core-end", "fin", "core-exit", "sortie", "core-fail", "échoue", "core-first", "premier", "core-flat", "applati", "core-flip", "retourne", "core-floor", "plancher", "core-flunk", "rate", "core-full-barrier", "barrière-totale", "core-hash", "dict", "core-head", "tête", "core-is", "est", "core-is-approx", "est-environ", "core-is-deeply", "est-profondément", "core-isa-ok", "estun-passable", "core-isnt", "nestpas", "core-item", "article", "core-join", "joins", "core-key", "clef", "core-keys", "clefs", "core-kv", "cv", "core-last", "dernier", "core-lastcall", "dernierappel", "core-lc", "mi", "core-like", "comme", "core-lines", "lignes", "core-link", "lien", "core-list", "liste", "core-lives-ok", "vit-passable", "core-lsb", "bms", "core-make", "fais", "core-mkdir", "mkrep", "core-move", "bouge", "core-msb", "bps", "core-new", "nouveau", "core-next", "suivant", "core-nextcallee", "suivantappelé", "core-nextsame", "suivantmeme", "core-nextwith", "suivantavec", "core-nok", "nonpassable", "core-none", "aucun", "core-not", "pas", "core-ok", "passable", "core-one", "un", "core-open", "ouvre", "core-pair", "paire", "core-pairs", "paires", "core-pass", "passe", "core-pick", "choisis", "core-pop", "éclos", "core-prepend", "préfixe", "core-print", "imprime", "core-printf", "imprimef", "core-proceed", "poursuis", "core-prompt", "demande", "core-push", "pousse", "core-put", "mets", "core-redo", "refais", "core-reduce", "réduis", "core-repeated", "répété", "core-return", "renvoie", "core-return-rw", "renvoie-le", "core-reverse", "renverse", "core-rmdir", "sprep", "core-roll", "tire", "core-rotate", "tourne", "core-round", "rond", "core-roundrobin", "tourniquet", "core-run", "lance", "core-samecase", "memecasse", "core-samemark", "samediac", "core-samewith", "memeavec", "core-say", "dis", "core-set", "ens", "core-shell", "coque", "core-shift", "décale", "core-sign", "signe", "core-signal", "signale", "core-skip", "omets", "core-skip-rest", "omets-reste", "core-sleep", "dors", "core-sleep-timer", "dors-minuteur", "core-sleep-until", "dors-jusquà", "core-slip", "enfile", "core-slurp", "engloutis", "core-snip", "découpe", "core-snitch", "dénonce", "core-so", "donc", "core-sort", "trie", "core-splice", "noue", "core-split", "coupe", "core-sprintf", "cimprmf", "core-spurt", "gicle", "core-sqrt", "racc", "core-subbuf", "soustampon", "core-subbuf-rw", "soustampon-le", "core-subtest", "soustest", "core-succeed", "réussis", "core-sum", "somme", "core-symlink", "liensymb", "core-tail", "queue", "core-take", "prends", "core-take-rw", "prends-le", "core-throws-like", "jete-comme", "core-todo", "àfaire", "core-trim", "rogne", "core-trim-leading", "rogne-gauche", "core-trim-trailing", "rogne-droite", "core-truncate", "tronque", "core-warn", "préviens", "core-wordcase", "motcasse", "core-words", "mots", "infix-after", "après", "infix-and", "et", "infix-andthen", "etalors", "infix-before", "avant", "infix-but", "mais", "infix-does", "fait", "infix-gcd", "pgcd", "infix-ge", "pge", "infix-gt", "pg", "infix-lcm", "ppcm", "infix-le", "ppe", "infix-leg", "ppepg", "infix-lt", "pp", "infix-ne", "pe", "infix-notandthen", "pasetalors", "infix-or", "ou", "infix-orelse", "oualors", "modifier-for", "pour", "modifier-given", "étantdonné", "modifier-if", "si", "modifier-unless", "saufsi", "modifier-until", "jusquà", "modifier-when", "quand", "modifier-while", "tansque", "modifier-with", "avec", "modifier-without", "sans", "multi-only", "seulement", "package-grammar", "grammaire", "package-native", "natif", "package-package", "paquet", "package-role", "rôle", "phaser-BEGIN", "DÉBUT", "phaser-CHECK", "VERIF", "phaser-CLOSE", "FERM", "phaser-CONTROL", "CONTROLE", "phaser-END", "FIN", "phaser-ENTER", "ENTRÉE", "phaser-FIRST", "DABORD", "phaser-KEEP", "GARDE", "phaser-LAST", "FINAL", "phaser-LEAVE", "SORS", "phaser-NEXT", "SUIVANT", "phaser-QUIT", "QUITTE", "phaser-UNDO", "DÉFAIRE", "prefix-not", "pas", "prefix-so", "donc", "routine-method", "méthode", "routine-rule", "règle", "routine-submethod", "sousméthode", "routine-token", "jeton", "scope-augment", "augmente", "scope-constant", "constante", "scope-has", "a", "scope-HAS", "A", "scope-my", "ma", "scope-our", "notre", "scope-supersede", "supplante", "scope-unit", "unité", "stmt-prefix-also", "aussi", "stmt-prefix-do", "fait", "stmt-prefix-eager", "impatient", "stmt-prefix-gather", "collecte", "stmt-prefix-lazy", "nonchalamment", "stmt-prefix-quietly", "silencieusement", "stmt-prefix-race", "course", "stmt-prefix-react", "réagis", "stmt-prefix-sink", "plonge", "stmt-prefix-start", "commence", "stmt-prefix-supply", "fournis", "stmt-prefix-try", "essaie", "term-now", "maintenant", "term-self", "sois", "term-time", "temps", "traitmod-does", "fait", "traitmod-handles", "gère", "traitmod-hides", "dissimule", "traitmod-is", "est", "traitmod-of", "de", "traitmod-returns", "renvoie", "typer-enum", "énum", "typer-subset", "sousens", "use-import", "importe", "use-need", "abesoin", "use-no", "non", "use-require", "requiert", "use-use", "utilise";
role RakuAST::Deparse::L10N::FR {
    method xsyn (str $prefix, str $key) {
                %xlation{"$prefix-$key"} // $key
    }
}

#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of localization --------------------------------------

# vim: expandtab shiftwidth=4
