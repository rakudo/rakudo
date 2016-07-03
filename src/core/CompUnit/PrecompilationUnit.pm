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
    has CompUnit::DependencySpecification $.spec;

    method source-name() {
        "$.src ($.spec.short-name())"
    }

    method deserialize(Str $str) {
#?if jvm
        my ($id, $src, $checksum, *@spec) = $str.split("\0", 7);
        my @spec-pairs;
        for @spec>>.match(/(<-[:]>+)':'(.+)/) {
            @spec-pairs.push: .[0].Str => (.[1] ~~ / ^ \d+ $ / ?? .[1].Int !! .[1].Str);
        }
        note @spec-pairs if %*ENV<SPECIFIC_DEBUG>;
        my CompUnit::DependencySpecification $spec .= new: |%(|@spec-pairs);
#?endif
#?if moarvm
        use MONKEY-SEE-NO-EVAL;
        my ($id, $src, $checksum, $spec) = $str.split("\0", 4);
        $spec = EVAL $spec;
#?endif
        self.new(:$id, :$src, :$checksum, :$spec);
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
