unit module GrammarNamedLikeCore;
grammar Grammar {
    token TOP { \w+ }
    our class Actions {
        method greet() { 'hi' }
    }
}
