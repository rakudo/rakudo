subset CompUnit::PrecompilationId of Str:D
    where { 2 < .chars < 64 && $_ ~~ /^<[A..Za..z0..9._-]>+$/ };

role CompUnit::PrecompilationDependency {
    method id(--> CompUnit::PrecompilationId) { ... }
    method src(--> Str) { ... }
    method spec(--> CompUnit::DependencySpecification) { ... }
    method Str() {
        "$.id $.src $.spec"
    }
    method serialize(--> Str) { ... }
    method deserialize(Str, --> CompUnit::PrecompilationDependency) { ... }
}

role CompUnit::PrecompilationUnit {
    method id(--> CompUnit::PrecompilationId) { ... }
    method path(--> IO::Path) { ... }
    method modified(--> Instant) { ... }
    method dependencies(--> Array[CompUnit::PrecompilationDependency]) { ... }
    method bytecode(--> Buf) { ... }
    method checksum(--> Str) { ... }
    method bytecode-handle(--> IO::Handle) { ... }
}

class CompUnit::PrecompilationDependency::File does CompUnit::PrecompilationDependency {
    has CompUnit::PrecompilationId $.id;
    has Str $.src;
    has Str $.checksum is rw;
    has Str $!serialized-spec;
    has CompUnit::DependencySpecification $.spec;

    method source-name() {
        "$.src ($.spec.short-name())"
    }

    method deserialize(Str $str) {
        my ($id, $src, $checksum, $spec) = $str.split("\0", 4);
        nqp::p6bindattrinvres(
            self.new(:$id, :$src, :$checksum),
            CompUnit::PrecompilationDependency::File,
            '$!serialized-spec',
            $spec,
        );
    }

    method spec(--> CompUnit::DependencySpecification) {
        $!spec //= $!serialized-spec
            ?? do {
#?if jvm
                my @spec = $!serialized-spec.split("\0", 3);
                my @spec-pairs;
                for @spec>>.match(/(<-[:]>+)':'(.+)/) {
                    @spec-pairs.push: .[0].Str => (.[1] ~~ / ^ \d+ $ / ?? .[1].Int !! .[1].Str);
                }
                CompUnit::DependencySpecification.new: |%(|@spec-pairs);
#?endif
#?if moar
                use MONKEY-SEE-NO-EVAL;
                EVAL $!serialized-spec;
#?endif
            }
            !! Nil;
    }

    method serialize(--> Str) {
#?if jvm
        my $specs;
        for $.spec.^attributes {
            $specs ~= .name.substr(2) ~ ":" ~ $.spec."$(.name.substr(2))"() ~ "\0";
        }
        "$.id\0$.src\0$.checksum\0$specs"
#?endif
#?if !jvm
        "$.id\0$.src\0$.checksum\0{$.spec.perl}"
#?endif
    }

    method Str() {
        "$.id $.src $.checksum $.spec"
    }
}

# vim: ft=perl6 expandtab sw=4
