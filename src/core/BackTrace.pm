my $*VERBOSE_BACKTRACE = False;
class BackTraceLine {
    has Str $.file;
    has Int $.line;
    has Str $.subname;
    has     $.subtype;

    multi method Str(BackTraceLine:D:) {
        my $s = $.subname // '<anon>';
        my $what = lc $.subtype.perl;
        "  in $what $s at {$.file}:$.line"
    }
}

class BackTrace is List {
    method new(Int $offset = 1) {
        my $new = self.bless(*);
        my $bt = nqp::atkey(pir::getinterp, 'context').backtrace;
        for $offset .. $bt.elems - 1 {
            next if pir::isnull($bt[$_]<sub>);
            my Mu $p6sub =
                pir::perl6_code_object_from_parrot_sub__PP($bt[$_]<sub>);
            next if !$*VERBOSE_BACKTRACE && !$p6sub.defined;
            next if !$*VERBOSE_BACKTRACE && !$p6sub ~~ Routine;
            my $line     = $bt[$_]<annotations><line>;
            my $file     = $bt[$_]<annotations><file>;
            my $subname  = nqp::p6box_s($bt[$_]<sub>);
            $subname = '<anon>' if $subname.substr(0, 6) eq '_block';
            $new.push: BackTraceLine.new(
                :$line,
                :$file,
                :$subname,
                :subtype($p6sub.WHAT),
            );
        }
        $new;
    }

    multi method Str(BackTrace:D:) {
        self.join("\n") ~ "\n"
    }
}
