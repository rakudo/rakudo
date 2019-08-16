use nqp;
use NativeCall;

unit class NativeCall::DLL
    is repr<CPointer>
    does Associative[Pointer, Str(Any)];

class Sym is repr<CPointer> {
    method cast(Sym:D: $target) {
        nativecast $target, self;
    }

    method address(Sym:D: --> Int) {
        nqp::box_i(nqp::unbox_i(self), Int);
    }

    multi method gist(Sym:U: --> '(DLL::Sym)') {}

    multi method gist(Sym:D: --> Str) {
        "DLL::Sym|{nqp::unbox_i(self).base(16)}";
    }
}

my constant DLL is export = NativeCall::DLL;

constant proc = nqp::box_i(0, DLL);

method new { !!! }

multi method load(DLL:U: Str:D $name, Str:D $lib --> DLL) {
    nqp::loadlib($name, $lib);
    nqp::getlib($name, self);
}

multi method load(DLL:U: Str:D $lib --> DLL) {
    self.load($lib, $lib);
}

multi method load(DLL:U: IO::Path:D $path --> DLL) {
    my $abspath := $path.absolute;
    self.load($abspath, $abspath);
}

method get(DLL:U: Str:D $name --> DLL) {
    nqp::getlib($name, self);
}

method drop(DLL:D: --> Nil) {
    nqp::droplib(self);
}

method findsym(DLL:D: Str:D $symbol --> Sym) {
    my int $address = nqp::findsym(self, $symbol);
    $address ?? nqp::box_i($address, Sym) !! Nil;
}

multi method gist(DLL:D: --> Str) {
    my int $i = nqp::unbox_i(self);
    "DLL|{$i ?? $i.base(16) !! '*'}";
}

method AT-KEY($key) {
    self.findsym($key);
}

method EXISTS-KEY($key) {
    self.findsym($key).DEFINITE;
}
