use NQPP6QRegex;
use NQPP5QRegex;


class Raku::Actions is HLL::Actions {
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

    method setup_resolver() {
        ensure_raku_ast();
        my $resolver_type := self.r('Resolver', 'Compile');
        my $outer_ctx := %*COMPILING<%?OPTIONS><outer_ctx>;
        if nqp::isconcrete($outer_ctx) {
            $resolver_type.new(:context($outer_ctx))
        }
        else {
            $resolver_type.new()
        }
    }

    method comp_unit($/) {
        make self.r('CompUnit').new($<statementlist>.ast);
    }

    method statementlist($/) {
        make self.r('StatementList').new();
    }
}
