class CompUnit::PrecompilationId {
    has $.id;

    method new(str $id --> CompUnit::PrecompilationId:D) {
        nqp::atpos(nqp::radix_I(16,$id,0,0,Int),2) == 40
          ?? nqp::p6bindattrinvres(nqp::create(self),
               CompUnit::PrecompilationId,'$!id',$id)
          !! die "Invalid precompilation id: '$id'"
    }

    method new-from-string(str $id --> CompUnit::PrecompilationId:D) {
        nqp::p6bindattrinvres(nqp::create(self),
          CompUnit::PrecompilationId,'$!id',nqp::sha1($id))
    }

    method new-without-check(str $id --> CompUnit::PrecompilationId:D) {
        nqp::p6bindattrinvres(nqp::create(self),
          CompUnit::PrecompilationId,'$!id',$id)
    }

    multi method WHICH(CompUnit::PrecompilationId:D: --> ValueObjAt:D) {
        nqp::box_s(
          nqp::concat('CompUnit::PrecompilationId|',$!id),
          ValueObjAt
        )
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

    method is-up-to-date(
      CompUnit::PrecompilationDependency $dependency,
      Bool :$check-source
    --> Bool:D) {
        my $RMD := $*RAKUDO_MODULE_DEBUG;

        # a repo changed, so maybe it's a change in our source file
        if $check-source {
            my $srcIO :=
              CompUnit::RepositoryRegistry.file-for-spec($dependency.src)
              // $dependency.src.IO;
            return False unless $srcIO.e;

            my $current-source-checksum := $srcIO.CHECKSUM;

            $RMD(
                "$.path\nspec: $dependency.spec()\nsource: $srcIO\n"
                ~ "source-checksum: $.source-checksum\ncurrent-source-checksum: $current-source-checksum"
            ) if $RMD;

            return False if $.source-checksum ne $current-source-checksum;
        }

        $RMD("dependency checksum $dependency.checksum() unit: $.checksum()")
          if $RMD;

        $.checksum eq $dependency.checksum
    }
}

class CompUnit::PrecompilationDependency::File
  does CompUnit::PrecompilationDependency
{
    has CompUnit::PrecompilationId        $.id   is built(:bind);
    has CompUnit::DependencySpecification $.spec is built(:bind);
    has Str $.src             is built(:bind);
    has Str $.checksum        is rw;
    has Str $!serialized-spec is built(:bind);

    method source-name() {
        "$.src ($.spec.short-name())"
    }

    method deserialize(str $str) {
        my $parts := nqp::split("\0",$str);
        self.new(
          :id(CompUnit::PrecompilationId.new-without-check(nqp::atpos($parts,0))),
          :src(nqp::atpos($parts,1)),
          :checksum(nqp::atpos($parts,2))
          :serialized-spec(nqp::atpos($parts,3))
        )
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
        "$!id\0$!src\0$!checksum\0$specs"
#?endif
#?if !jvm
        "$!id\0$!src\0$!checksum\0{
            $!serialized-spec ?? $!serialized-spec !! $!spec.raku
        }"
#?endif
    }

    method Str() {
        "$.id $.src $.checksum {$!serialized-spec ?? $!serialized-spec !! $!spec.raku}"
    }
}

# vim: expandtab shiftwidth=4
