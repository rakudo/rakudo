use Test;

plan 4;

# https://github.com/rakudo/rakudo/issues/5091
# Enum values in the signature of a MAIN must be recognized on the command
# line even when the enum is not visible in the scope that RUN-MAIN is
# called from, which is the case when MAIN is imported from a module that
# declares the enum inside its own package.

my $positional;
my $named;
my $mixed-enum;
my $mixed-note;
my $wolves;
my $butterflies;
my &positional-main;
my &named-main;
my &mixed-main;

my class Container {
    enum Interesting <Wolves Butterflies>;
    $wolves      = Wolves;
    $butterflies = Butterflies;
    &positional-main = anon sub MAIN(Interesting $chosen) {
        $positional = $chosen
    }
    &named-main = anon sub MAIN(Interesting :$chosen!) {
        $named = $chosen
    }
    &mixed-main = anon sub MAIN(Interesting $chosen, Str $note) {
        $mixed-enum = $chosen;
        $mixed-note = $note;
    }
}

@*ARGS = "Wolves",;
RUN-MAIN(&positional-main, Nil);
is-deeply $positional, $wolves,
  'positional enum argument dispatches when the enum is not in scope';

@*ARGS = "--chosen=Butterflies",;
RUN-MAIN(&named-main, Nil);
is-deeply $named, $butterflies,
  'named enum argument dispatches when the enum is not in scope';

@*ARGS = "Wolves", "hello";
RUN-MAIN(&mixed-main, Nil);
is-deeply $mixed-enum, $wolves,
  'enum argument dispatches alongside a Str argument';
is-deeply $mixed-note, "hello",
  'word that is not an enum key still arrives as a Str';

# vim: expandtab shiftwidth=4
