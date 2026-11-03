use v6.d;
unit module LangRevSixD;
our sub boundary() { EVAL q["x" ~~ / <|x> /; 'compiled'] }
