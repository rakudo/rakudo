use v6.e.PREVIEW;
unit module LangRevSixE;
our sub revision() { EVAL q[$?LANGUAGE-REVISION] }
our sub boundary() { EVAL q["x" ~~ / <|x> /] }
