use NQPP6QRegex;
use NQPP5QRegex;

class Raku::Actions is HLL::Actions {
    # The AST nodes come from the Raku setting bootstrap, thus we need to load
    # them from there.
    my $ast_root;
    sub ensure_raku_ast() {
        unless nqp::isconcrete($ast_root) {
            my $loader := nqp::gethllsym('Raku', 'ModuleLoader');
            my $unit := $loader.load_module('Perl6::BOOTSTRAP::v6c', {}, GLOBALish);
            my $export := $unit<EXPORT>.WHO<DEFAULT>.WHO;
            $ast_root := nqp::existskey($export, 'RakuAST')
                    ?? nqp::atkey($export, 'RakuAST').WHO
                    !! nqp::die('Cannot find RakuAST nodes');
        }
    }
    proto method r(*@parts) {*}
    multi method r($t) {
        nqp::ifnull(nqp::atkey($ast_root, $t), nqp::die("No such node RakuAST::{$t}"))
    }
    multi method r($t1, $t2) {
        my $res := nqp::atkey($ast_root, $t1);
        $res := nqp::atkey($res.WHO, $t2) unless nqp::isnull($res);
        nqp::ifnull($res, nqp::die("No such node RakuAST::{$t1}::{$t2}"))
    }

    ##
    ## Compilation unit, language version and other entry point bits
    ##

    # Used to ensure uniqueness of serialization context IDs.
    my class SerializationContextId {
        my $count := 0;
        my $lock  := NQPLock.new;
        method next-id() {
            $lock.protect({ $count++ })
        }
    }

    method lang_setup($/) {
        ensure_raku_ast();

        # Calculate the setting name to use.
        # TODO don't hardcode this
        my $name := 'CORE';
        my $version := nqp::substr(nqp::getcomp('Raku').config<language-version>, 2);
        my $loader := nqp::gethllsym('Raku', 'ModuleLoader');
        my $setting-name := $loader.transform_setting_name("$name.$version");

        # Create a compilation unit.
        my $file := nqp::getlexdyn('$?FILES');
        my $comp-unit-name := nqp::sha1($file ~ (
            nqp::defined(%*COMPILING<%?OPTIONS><outer_ctx>)
                ?? $/.target() ~ SerializationContextId.next-id()
                !! $/.target()));
        $*CU := self.r('CompUnit').new(:$comp-unit-name, :$setting-name);

        # Set up the resolver.
        my $resolver_type := self.r('Resolver', 'Compile');
        my $outer_ctx := %*COMPILING<%?OPTIONS><outer_ctx>;
        if nqp::isconcrete($outer_ctx) {
            $*R := $resolver_type.from-context(:context($outer_ctx));
        }
        else {
            $*R := $resolver_type.from-setting(:$setting-name);
        }
    }

    method comp_unit($/) {
        my $cu := $*CU;
        $cu.replace-statement-list($<statementlist>.ast);
        $cu.resolve-all($*R);
        make $cu;
    }

    method statementlist($/) {
        make self.r('StatementList').new();
    }
}
