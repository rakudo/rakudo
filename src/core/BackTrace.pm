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
    method new(Int $offset = 1) {
        my $new = self.bless(*);
        my $bt = nqp::atkey(pir::getinterp, 'context').backtrace;
        for $offset .. $bt.elems - 1 {
            next if pir::isnull($bt[$_]<sub>);
            my Mu $p6sub =
                pir::perl6_code_object_from_parrot_sub__PP($bt[$_]<sub>);
            my $line     = $bt[$_]<annotations><line>;
            my $file     = $bt[$_]<annotations><file>;
            my $subname  = nqp::p6box_s($bt[$_]<sub>);
            $subname = '<anon>' if $subname.substr(0, 6) eq '_block';
            $new.push: BackTraceLine.new(
                :$line,
                :$file,
                :$subname,
                :code($p6sub),
            );
        }
        $new;
    }

    method concise() {
        self.grep({ .is-routine }).join("\n") ~ "\n"
    }

    multi method Str(BackTrace:D:) {
        self.join("\n") ~ "\n"
    }
}
