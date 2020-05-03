multi sub run(Str $code, Str $input?, :@args, :@compiler-args, :$fake-run!) {
    my Mu $compiler-args-s := nqp::list_s();
    my Mu $args-s := nqp::list_s();

    for @compiler-args -> $arg {
        nqp::push_s($compiler-args-s, $arg.Str);
    }

    for @args -> $arg {
        nqp::push_s($args-s, $arg.Str);
    }

    my Mu $result := nqp::p6fakerun(nqp::hash(
        'code', $code,
        'input', $input,
        'compiler-args', $compiler-args-s,
        'args', $args-s,
        'env', CLONE-HASH-DECONTAINERIZED(%*ENV)
    ));

    my %out;
    %out<status> = nqp::atkey($result, 'status');
    %out<out> = nqp::atkey($result, 'out');
    %out<err> = nqp::atkey($result, 'err');

    %out;
}
