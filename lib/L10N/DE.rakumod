# This file contains the German Slang of the Raku Programming Language

#- start of generated part of localization ------------------------------------
#- Generated on 2023-12-07T12:05:29+01:00 by tools/build/makeL10N.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

role L10N::DE {
    use experimental :rakuast;
    token block-default { standard}
    token block-else { sonst}
    token block-elsif { sonstwenn}
    token block-for { füralle}
    token block-given { gegeben}
    token block-if { wenn}
    token block-loop { schleife}
    token block-orwith { odermit}
    token block-repeat { wiederhole}
    token block-unless { wennnicht}
    token block-until { bis}
    token block-when { wann}
    token block-whenever { wannimmer}
    token block-while { solange}
    token block-with { mit}
    token block-without { ohne}
    token constraint-where { wobei}
    token infix-pcontp { "(enthält)"}
    token infix-pelemp { "(element)"}
    token infix-cff { "^ff"}
    token infix-cffc { "^ff^"}
    token infix-cfff { "^fff"}
    token infix-cfffc { "^fff^"}
    token infix-after { nach}
    token infix-and { und}
    token infix-andthen { unddann}
    token infix-before { vorher}
    token infix-but { aber}
    token infix-cmp { vergleich}
    token infix-coll { coll}
    token infix-div { teile}
    token infix-does { macht}
    token infix-eq { gleich}
    token infix-ff { ff}
    token infix-ffc { "ff^"}
    token infix-fff { fff}
    token infix-fffc { "fff^"}
    token infix-gcd { gcd}
    token infix-ge { größergleich}
    token infix-gt { größer}
    token infix-lcm { lcm}
    token infix-le { kleinergleich}
    token infix-leg { kgg}
    token infix-lt { kleiner}
    token infix-max { max}
    token infix-min { min}
    token infix-minmax { minmax}
    token infix-mod { modulo}
    token infix-ne { ungleich}
    token infix-notandthen { nichtunddann}
    token infix-o { o}
    token infix-or { oder}
    token infix-orelse { oderanders}
    token infix-unicmp { unicmp}
    token infix-x { x}
    token infix-X { X}
    token infix-xx { xx}
    token infix-Z { R}
    token meta-R { U}
    token meta-X { X}
    token meta-Z { R}
    token modifier-for { füralle}
    token modifier-given { gegeben}
    token modifier-if { wenn}
    token modifier-unless { wennnicht}
    token modifier-until { bis}
    token modifier-when { wann}
    token modifier-while { solange}
    token modifier-with { mit}
    token modifier-without { ohne}
    token multi-multi { multi}
    token multi-only { nur}
    token multi-proto { proto}
    token package-class { klasse}
    token package-grammar { grammatik}
    token package-module { modul}
    token package-package { paket}
    token package-role { rolle}
    token phaser-BEGIN { BEGIN}
    token phaser-CATCH { FANGFEHLER}
    token phaser-CHECK { CHECK}
    token phaser-CLOSE { SCHLIEßE}
    token phaser-CONTROL { FANGNACHRICHT}
    token phaser-DOC { DOC}
    token phaser-END { ENDE}
    token phaser-ENTER { EINTRITT}
    token phaser-FIRST { ERSTE}
    token phaser-INIT { INIT}
    token phaser-KEEP { AKZEPTIERE}
    token phaser-LAST { LETZTE}
    token phaser-LEAVE { VERLASSE}
    token phaser-NEXT { NÄCHSTE}
    token phaser-POST { NACHHER}
    token phaser-PRE { VORHER}
    token phaser-QUIT { AUSGESTIEGEN}
    token phaser-UNDO { AKZEPTIERENICHT}
    token prefix-not { nicht}
    token prefix-so { also}
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
    token routine-method { methode}
    token routine-regex { regex}
    token routine-rule { regel}
    token routine-sub { sub}
    token routine-submethod { submethode}
    token routine-token { wertmarke}
    token scope-anon { anonym}
    token scope-augment { verbesser}
    token scope-constant { konstante}
    token scope-has { hat}
    token scope-HAS { HAT}
    token scope-my { mein}
    token scope-our { unsere}
    token scope-state { stand}
    token scope-unit { einheit}
    token stmt-prefix-also { auch}
    token stmt-prefix-do { mache}
    token stmt-prefix-eager { eifrig}
    token stmt-prefix-gather { sammel}
    token stmt-prefix-hyper { hyper}
    token stmt-prefix-lazy { faul}
    token stmt-prefix-quietly { leise}
    token stmt-prefix-race { race}
    token stmt-prefix-react { reagier}
    token stmt-prefix-sink { sink}
    token stmt-prefix-start { start}
    token stmt-prefix-supply { liefer}
    token stmt-prefix-try { versuche}
    token term-nano { nano}
    token term-now { jetzt}
    token term-pi { pi}
    token term-rand { zufällig}
    token term-self { selbst}
    token term-tau { tau}
    token term-time { zeit}
    token traitmod-does { macht}
    token traitmod-handles { versteht}
    token traitmod-hides { versteckt}
    token traitmod-is { ist}
    token traitmod-of { wie}
    token traitmod-returns { returns}
    token traitmod-trusts { vertraut}
    token typer-enum { enum}
    token typer-subset { subset}
    token use-import { importiere}
    token use-need { braucht}
    token use-no { kein}
    token use-require { require}
    token use-use { verwende}
    method core2ast {
        my %mapping = "alle", "all", "antipare", "antipairs", "beliebig", "any", "anhängen", "append", "warte", "await", "tasche", "bag", "steig-aus", "bail-out", "segne", "bless", "kategorisier", "categorize", "decke", "ceiling", "zeichen", "chars", "hack-linieende", "chomp", "hack", "chop", "wie-zeiche", "chr", "wie-zeichen", "chrs", "klassifizier", "classify", "schließe", "close", "kamm", "comb", "kombinationen", "combinations", "enthält", "contains", "kreuz", "cross", "dekodiere", "decode", "tief-kartiere", "deepmap", "definiert", "defined", "sterb", "die", "fertig", "done", "duck-kartiere", "duckmap", "elemente", "elems", "sende", "emit", "kodiere", "encode", "ende", "end", "endet-mit", "ends-with", "eval-stirbt-ok", "eval-dies-ok", "eval-lebt-ok", "eval-lives-ok", "verlasse", "exit", "scheitere", "fail", "scheitert-wie", "fails-like", "erste", "first", "flach", "flat", "drehum", "flip", "boden", "floor", "fehlgeschlagen", "flunk", "erhalte", "get", "erhaltez", "getc", "kern", "gist", "filter", "grep", "hasch", "hash", "kopf", "head", "einzug", "indent", "indizes", "indices", "ist", "is", "ist-ungefähr", "is-approx", "ist-im-ganzen", "is-deeply", "ist-prim", "is-prime", "istein-ok", "isa-ok", "istnicht", "isnt", "artikel", "item", "verbinde", "join", "schlüssel", "key", "schlüssels", "keys", "sw", "kv", "letzte", "last", "letzte-anruf", "lastcall", "wie", "like", "linien", "lines", "verknüpfung", "link", "liste", "list", "lebt-ok", "lives-ok", "niedrigstwertige-bit", "lsb", "mache", "make", "kartiere", "map", "paar", "match", "bewege", "move", "höchstwertiges-bit", "msb", "neu", "new", "nächste", "next", "nicht-ok", "nok", "kein", "none", "nicht", "not", "notiere", "note", "ein", "one", "als-zahl", "ord", "als-zahlen", "ords", "paar", "pair", "paare", "pairs", "bestanden", "pass", "permutationen", "permutations", "pflücke", "pick", "plane", "plan", "stelle-voran", "prepend", "druck", "print", "druckf", "printf", "fortfahre", "proceed", "schiebe-oben", "push", "sag-es", "put", "zufällig", "rand", "nochmals", "redo", "reduziere", "reduce", "wiederholt", "repeated", "kehr-um", "reverse", "um-index", "rindex", "werfe", "roll", "drehe", "rotate", "runde-ab", "round", "jeder-mal", "roundrobin", "laufe", "run", "sag", "say", "sammlung", "set", "schiebe-unter", "shift", "überspringe", "skip", "überspringe-alle", "skip-rest", "schlaf", "sleep", "wecker", "sleep-timer", "schlafe-bis", "sleep-until", "schlürfe", "slurp", "verpfeife", "snitch", "also", "so", "sortier", "sort", "spleiße", "splice", "teilt", "split", "sdruckf", "sprintf", "spritz", "spurt", "quadratwurzel", "sqrt", "quetschen", "squish", "zufällig-so", "srand", "beginnt-mit", "starts-with", "ersetz", "subst", "gelinge", "succeed", "summiere", "sum", "symbolische-verknüpfung", "symlink", "schwanz", "tail", "nim", "take", "nmm-rw", "take-rw", "werft-wie", "throws-like", "zumachen", "todo", "übersetz", "trans", "trim-führend", "trim-leading", "trim-hinterher", "trim-trailing", "kürzen", "truncate", "einzigartig", "unique", "nicht-wie", "unlike", "hebe-auf", "unlink", "onderin", "unshift", "verwende-ok", "use-ok", "wie-zahlen", "val", "wert", "value", "werte", "values", "warne", "warn", "wörter", "words", "reißverschluss", "zip";
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
        my %mapping = "kopie", "copy", "standard", "default", "VERALTET", "DEPRECATED", "äquivalent", "equiv", "implementierungsdetail", "implementation-detail", "lockerer", "looser", "rein", "pure", "roh", "raw", "lesen-schreiben", "rw", "enger", "tighter";
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
        my %mapping = "entferne", "delete", "existiert", "exists", "s", "k", "sw", "kv", "w", "v";
        %mapping{$key} // $key
    }
    method adverb-q2str (str $key) {
        $key
    }
    method adverb-rx2str (str $key) {
        my %mapping = "fortsetzen", "continue", "er", "ex", "erschöpfend", "exhaustive", "te", "nd", "te", "nth", "üb", "ov", "überlappung", "overlap", "ratsche", "ratchet", "te", "rd", "te", "st", "bis", "to", "mal", "x";
        %mapping{$key} // $key
    }
    method named2str (str $key) {
        my %mapping = "absolut", "absolute", "actionen", "actions", "anhängen", "append", "schließe", "close", "vollständig", "completely", "fortsetzen", "continue", "anzahl", "count", "datum", "date", "tag", "day", "streiche", "delete", "elementen", "elems", "vonende", "end", "jeder", "every", "er", "ex", "exclusiv", "exclusive", "erschöpfend", "exhaustive", "läuftab", "expires", "stunde", "hour", "s", "k", "schlüssel", "key", "sw", "kv", "hören", "listen", "monat", "month", "te", "nd", "te", "nth", "aus", "off", "üb", "ov", "überlappung", "overlap", "teilweise", "partial", "teile", "parts", "te", "rd", "ersatz", "replacement", "sekunde", "second", "sekunden", "seconds", "größe", "size", "quetschen", "squash", "ste", "st", "tiefsatz", "subscript", "hochgestellt", "superscript", "te", "th", "wie-oft", "times", "zeitzone", "timezone", "w", "v", "wert", "value", "wo", "where", "mal", "x", "jahr", "year";
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
          $LANG.slang_grammar('MAIN').^mixin(L10N::DE)
        );
    }

    BEGIN Map.new
}

#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of localization --------------------------------------

# vim: expandtab shiftwidth=4
