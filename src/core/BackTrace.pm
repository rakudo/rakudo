class BackTraceLine {
    has Str $.file;
    has Int $.line;
    has Mu  $.code;
    has Str $.subname;

    method subtype { $!code.WHAT.perl.lc }

    multi method Str(BackTraceLine:D:) {
        "  in $.subtype $.subname at {$.file}:$.line"
    }

    method is-routine { $!code ~~ Routine }
}

class BackTrace is List {
    # a parrot BT is a ResizablePMCArray, so mappted to a Parcel
    has $!parrot_bt;
    method init(BackTrace:D: Int $offset = 0) {
        for $offset .. $!parrot_bt.elems - 1 {
            next if pir::isnull($!parrot_bt[$_]<sub>);
            my Mu $p6sub =
                pir::perl6_code_object_from_parrot_sub__PP($!parrot_bt[$_]<sub>);
            my $line     = $!parrot_bt[$_]<annotations><line>;
            my $file     = $!parrot_bt[$_]<annotations><file>;
            my $subname  = nqp::p6box_s($!parrot_bt[$_]<sub>);
            $subname = '<anon>' if $subname.substr(0, 6) eq '_block';
            selfw.push: BackTraceLine.new(
                :$line,
                :$file,
                :$subname,
                :code($p6sub),
            );
        }
        self
    }

    method concise() {
        self.grep({ .is-routine }).join("\n") ~ "\n"
    }

    multi method Str(BackTrace:D:) {
        self.join("\n") ~ "\n"
    }
}
