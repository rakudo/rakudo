class CompUnit::PrecompilationId {
    has $.id;

    my $cache-lock = Lock.new;
    my %cache;

    method new(Str:D $id) {
        $cache-lock.protect: {
            %cache{$id} //= 2 < $id.chars < 64 && $id ~~ /^<[A..Za..z0..9._-]>+$/
                ?? self.bless(:$id)
                !! X::AdHoc.new( payload => "Invalid precompilation id: $id" ).throw
        }
    }

    method new-from-string(Str:D $id) {
        $cache-lock.protect: {
            %cache{$id} //= self.bless(:id(nqp::sha1($id)))
        }
    }

    method new-without-check(Str:D $id) {
        $cache-lock.protect: {
            %cache{$id} //= self.bless(:id($id))
        }
    }

    method Str()      { $!id }
    method IO()       { $!id.IO }
    method substr(|c) { $!id.substr(|c) }
}

role CompUnit::PrecompilationDependency {
    method id(--> CompUnit::PrecompilationId:D) { ... }
    method src(--> Str:D) { ... }
    method spec(--> CompUnit::DependencySpecification:D) { ... }
    method checksum(--> Str:D) { ... }
    method Str() {
        "$.id $.src $.spec"
    }
    method serialize(--> Str:D) { ... }
    method deserialize(Str, --> CompUnit::PrecompilationDependency:D) { ... }
}

role CompUnit::PrecompilationUnit {
    method id(--> CompUnit::PrecompilationId:D) { ... }
    method path(--> IO::Path:D) { ... }
    method modified(--> Instant:D) { ... }
    method dependencies(--> Array[CompUnit::PrecompilationDependency]) { ... }
    method bytecode(--> Buf:D) { ... }
    method checksum(--> Str:D) { ... }
    method source-checksum(--> Str:D) { ... }
    method bytecode-handle(--> IO::Handle:D) { ... }
    method close(--> Nil) { ... }
    method is-up-to-date(CompUnit::PrecompilationDependency $dependency, Bool :$check-source --> Bool) {
        my $RMD = $*RAKUDO_MODULE_DEBUG;
        if $check-source { # a repo changed, so maybe it's a change in our source file
            my $source-checksum = $.source-checksum;

            my $srcIO = CompUnit::RepositoryRegistry.file-for-spec($dependency.src) // $dependency.src.IO;
            return False unless $srcIO and $srcIO.e;

            my $current-source-checksum := nqp::sha1($srcIO.slurp(:enc<iso-8859-1>).Str);
            $RMD(
                "$.path\nspec: $dependency.spec()\nsource: $srcIO\n"
                ~ "source-checksum: $source-checksum\ncurrent-source-checksum: $current-source-checksum"
            ) if $RMD;
            return False if $source-checksum ne $current-source-checksum;
        }

        $RMD("dependency checksum $dependency.checksum() unit: $.checksum()") if $RMD;

        $.checksum eq $dependency.checksum
    }
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

    method deserialize(str $str) {
        my $parts := nqp::split("\0", $str);
        nqp::p6bindattrinvres(
            self.new(
                :id(CompUnit::PrecompilationId.new-without-check(nqp::atpos($parts, 0))),
                :src(nqp::atpos($parts, 1)),
                :checksum(nqp::atpos($parts, 2))
            ),
            CompUnit::PrecompilationDependency::File,
            '$!serialized-spec',
            nqp::atpos($parts, 3),
        );
    }

    method spec(--> CompUnit::DependencySpecification:D) {
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
#?if !jvm
                use MONKEY-SEE-NO-EVAL;
                EVAL $!serialized-spec;
#?endif
            }
            !! Nil;
    }

    method serialize(--> Str:D) {
#?if jvm
        my $specs;
        for $.spec.^attributes {
            $specs ~= .name.substr(2) ~ ":" ~ $.spec."$(.name.substr(2))"() ~ "\0";
        }
        "$.id\0$.src\0$.checksum\0$specs"
#?endif
#?if !jvm
        "$.id\0$.src\0$.checksum\0{$!serialized-spec ?? $!serialized-spec !! $!spec.perl}"
#?endif
    }

    method Str() {
        "$.id $.src $.checksum {$!serialized-spec ?? $!serialized-spec !! $!spec.perl}"
    }
}

# vim: ft=perl6 expandtab sw=4
