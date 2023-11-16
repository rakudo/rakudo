# This file contains the Portuguese Slang of the Raku Programming Language

#- start of generated part of localization ------------------------------------
#- Generated on 2023-11-16T17:03:40+01:00 by tools/build/makeL10N.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

role L10N::PT {
    use experimental :rakuast;
    token block-default { predefinita}
    token block-else { "se-não"}
    token block-elsif { "se-não-se"}
    token block-for { por}
    token block-given { dado}
    token block-if { se}
    token block-loop { ciclo}
    token block-orwith { "ou-com"}
    token block-repeat { ripita}
    token block-unless { "a-não-ser"}
    token block-until { até}
    token block-when { quando}
    token block-whenever { "assim-que"}
    token block-while { enquanto}
    token block-with { com}
    token block-without { sem}
    token constraint-where { quando}
    token infix-pcontp { "(cont)"}
    token infix-pelemp { "(elem)"}
    token infix-cff { "^ff"}
    token infix-cffc { "^ff^"}
    token infix-cfff { "^fff"}
    token infix-cfffc { "^fff^"}
    token infix-after { depois}
    token infix-and { e}
    token infix-andthen { "e-então"}
    token infix-before { antes}
    token infix-but { mas}
    token infix-cmp { cmp}
    token infix-coll { coll}
    token infix-div { div}
    token infix-does { faz}
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
    token infix-notandthen { "não-então"}
    token infix-o { o}
    token infix-or { ou}
    token infix-orelse { "ou-senão"}
    token infix-unicmp { unicmp}
    token infix-x { x}
    token infix-X { X}
    token infix-xx { xx}
    token infix-Z { Z}
    token meta-R { R}
    token meta-X { X}
    token meta-Z { Z}
    token modifier-for { por}
    token modifier-given { dado}
    token modifier-if { se}
    token modifier-unless { "a-não-ser"}
    token modifier-until { "até-que"}
    token modifier-when { quando}
    token modifier-while { enquanto}
    token modifier-with { com}
    token modifier-without { sem}
    token multi-multi { multi}
    token multi-only { só}
    token multi-proto { proto}
    token package-class { classe}
    token package-grammar { gramática}
    token package-module { módulo}
    token package-package { pacote}
    token package-role { papel}
    token phaser-BEGIN { COMEÇO}
    token phaser-CATCH { PEGA}
    token phaser-CHECK { CHECAGEM}
    token phaser-CLOSE { FECHO}
    token phaser-CONTROL { CONTROLE}
    token phaser-DOC { DOC}
    token phaser-END { FIM}
    token phaser-ENTER { ENTRADA}
    token phaser-FIRST { PRIMEIRA}
    token phaser-INIT { INIT}
    token phaser-KEEP { TEM}
    token phaser-LAST { ÚLTIMO}
    token phaser-LEAVE { SAIDA}
    token phaser-NEXT { PRÓXIMO}
    token phaser-POST { POST}
    token phaser-PRE { PRE}
    token phaser-QUIT { QUIT}
    token phaser-UNDO { DESFAZ}
    token prefix-not { não}
    token prefix-so { so}
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
    token routine-method { método}
    token routine-regex { regex}
    token routine-rule { regra}
    token routine-sub { sub}
    token routine-submethod { submethod}
    token routine-token { token}
    token scope-anon { anon}
    token scope-augment { augment}
    token scope-constant { constante}
    token scope-has { tem}
    token scope-HAS { TEM}
    token scope-my { meu}
    token scope-our { nosso}
    token scope-state { estado}
    token scope-unit { unidade}
    token stmt-prefix-also { também}
    token stmt-prefix-do { faz}
    token stmt-prefix-eager { impaciente}
    token stmt-prefix-gather { recolhe}
    token stmt-prefix-hyper { hyper}
    token stmt-prefix-lazy { preguiçoso}
    token stmt-prefix-quietly { tranquilamente}
    token stmt-prefix-race { race}
    token stmt-prefix-react { reaja}
    token stmt-prefix-sink { cale}
    token stmt-prefix-start { comece}
    token stmt-prefix-supply { supply}
    token stmt-prefix-try { tente}
    token term-nano { nano}
    token term-now { agora}
    token term-pi { pi}
    token term-rand { rand}
    token term-self { "si-mesmo"}
    token term-tau { tau}
    token term-time { tempo}
    token traitmod-does { faz}
    token traitmod-handles { cuida}
    token traitmod-hides { esconde}
    token traitmod-is { é}
    token traitmod-of { do}
    token traitmod-returns { retorna}
    token traitmod-trusts { trusts}
    token typer-enum { enum}
    token typer-subset { subset}
    token use-import { importa}
    token use-need { precisa}
    token use-no { não}
    token use-require { requer}
    token use-use { usa}
    method core2ast {
        my %mapping = "todos", "all", "algum", "any", "insere-no-final", "append", "adiciona-e-retorna-atomicamente", "atomic-add-fetch", "define-valor-atomicamente", "atomic-assign", "decrementa-e-retorna-atomicamente", "atomic-dec-fetch", "retorna-atomicamente", "atomic-fetch", "retorna-e-adiciona-atomicamente", "atomic-fetch-add", "retorna-e-decrementa-atomicamente", "atomic-fetch-dec", "retorna-e-incrementa-atomicamente", "atomic-fetch-inc", "retorna-e-subtrai-atomicamente", "atomic-fetch-sub", "incrementa-e-retorna-atomicamente", "atomic-inc-fetch", "subtrai-e-retorna-atomicamente", "atomic-sub-fetch", "espera", "await", "bolsa", "bag", "abençoa", "bless", "classifique", "classify", "combinações", "combinations", "cruz", "cross", "definido", "defined", "morra", "die", "feito", "done", "emita", "emit", "fim", "end", "termina-com", "ends-with", "saia", "exit", "falhe", "fail", "primo", "first", "plano", "flat", "chão", "floor", "pegue", "get", "essencia", "gist", "cabeça", "head", "indente", "indent", "indice", "index", "é", "is", "não-é", "isnt", "junta", "join", "chave", "key", "chaves", "keys", "cv", "kv", "último", "last", "última-chamada", "lastcall", "linhas", "lines", "lista", "list", "faça", "make", "mova", "move", "próximo", "next", "próxima-chamada", "nextcallee", "próximo-mesmo", "nextsame", "próximo-com", "nextwith", "nenhum", "none", "não", "not", "nota", "note", "um", "one", "aberto", "open", "par", "pair", "pares", "pairs", "permutações", "permutations", "escreva", "print", "escreva-formatado", "printf", "siga", "proceed", "refaça", "redo", "reduza", "reduce", "repetido", "repeated", "retorne", "return", "retorne-rw", "return-rw", "diga", "say", "assine", "sign", "sinal", "signal", "pula", "skip", "durma", "sleep", "durma-até", "sleep-until", "ordene", "sort", "encolha", "squish", "some", "sum", "cauda", "tail", "pegue", "take", "pegue-rw", "take-rw", "valor", "value", "valores", "values", "avise", "warn", "palavras", "words";
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
        self.ast // RakuAST::Name.from-identifier(self.Str)
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
    method system2str (str $key) {
        $key
    }
}

# The EXPORT sub that actually does the slanging
my sub EXPORT($dontslang?) {
    unless $dontslang {
        my $LANG := $*LANG;
        $LANG.define_slang('MAIN',
          $LANG.slang_grammar('MAIN').^mixin(L10N::PT)
        );
    }

    BEGIN Map.new
}

#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of localization --------------------------------------

# vim: expandtab shiftwidth=4
