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
    proto method new(|$) {*}

    multi method new(Exception $e, Int $offset = 0) {
        self.new(nqp::getattr(pir::perl6_decontainerize__PP($e), Exception, '$!ex').backtrace, $offset);
    }

    # note that parrot backtraces are RPAs, marshalled to us as Parcel
    multi method new(Parcel $bt, Int $offset = 0) {
        my $new = self.bless(*);
        for $offset .. $bt.elems - 1 {
            next if pir::isnull($bt[$_]<sub>);
            my $code = try {
                pir::perl6_code_object_from_parrot_sub__PP($bt[$_]<sub>);
            };
            my $line     = $bt[$_]<annotations><line>;
            my $file     = $bt[$_]<annotations><file>;
            next unless $line && $file;
            # now *that's* an evil hack
            last if $file eq 'src/stage2/gen/NQPHLL.pm';
            my $subname  = nqp::p6box_s($bt[$_]<sub>);
            $subname = '<anon>' if $subname.substr(0, 6) eq '_block';
            $new.push: BackTraceLine.new(
                :$line,
                :$file,
                :$subname,
                :$code,
            );
        }
        # another evil hack:
        # it seems that the backtrace always starts at line 1
        # of the script, but we're not interested in that
        $new.pop if $new[*-1].line == 1;
        $new;
    }

    method concise() {
        self.grep({ .is-routine }).join("\n") ~ "\n"
    }

    multi method Str(BackTrace:D:) {
        self.join("\n") ~ "\n"
    }
}
