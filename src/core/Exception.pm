my role X::Comp { ... }
my class X::ControlFlow { ... }

my class Exception {
    has $!ex;
    has $!bt;

    method backtrace(Exception:D:) {
        if $!bt { $!bt }
        elsif nqp::isconcrete($!ex) {
            nqp::bindattr(self, Exception, '$!bt', Backtrace.new($!ex));
        }
        else { '' }
    }

    # Only valid if .backtrace has not been called yet
    method vault-backtrace(Exception:D:) {
	nqp::isconcrete($!ex) && $!bt ?? Backtrace.new($!ex) !! ''
    }
    method reset-backtrace(Exception:D:) {
        nqp::bindattr(self, Exception, '$!ex', Nil)
    }

    multi method Str(Exception:D:) {
        my $str;
        if nqp::isconcrete($!ex) {
            my str $message = nqp::getmessage($!ex);
            $str = nqp::isnull_s($message) ?? '' !! nqp::p6box_s($message);
        }
        $str ||= (try self.?message);
        $str = ~$str if defined $str;
        $str // "Something went wrong in {self.WHAT.gist}";
    }

    multi method gist(Exception:D:) {
        my $str;
        if nqp::isconcrete($!ex) {
            my str $message = nqp::getmessage($!ex);
            $str = nqp::isnull_s($message)
                ?? "Died with {self.^name}"
                !! nqp::p6box_s($message);
            $str ~= "\n";
            try $str ~= self.backtrace
              || Backtrace.new()
              || '  (no backtrace available)';
        }
        else {
            $str = (try self.?message) // "Unthrown {self.^name} with no message";
        }
        $str;
    }

    method throw(Exception:D: $bt?) {
        nqp::bindattr(self, Exception, '$!ex', nqp::newexception())
            unless nqp::isconcrete($!ex) and $bt;
        nqp::bindattr(self, Exception, '$!bt', $bt); # Even if !$bt
        nqp::setpayload($!ex, nqp::decont(self));
        my $msg := try self.?message;
        if defined($msg) {
            $msg := try ~$msg;
        }
        $msg := $msg // "{self.^name} exception produced no message";
        nqp::setmessage($!ex, nqp::unbox_s($msg));
        nqp::throw($!ex)
    }
    method rethrow(Exception:D:) {
        nqp::setpayload($!ex, nqp::decont(self));
        nqp::rethrow($!ex)
    }

    method resumable(Exception:D:) {
        nqp::p6bool(nqp::istrue(nqp::atkey($!ex, 'resume')));
    }

    method resume(Exception:D: --> True) {
        nqp::resume($!ex);
    }

    method die(Exception:D:) { self.throw }
    method fail(Exception:D:) {
        try self.throw;
        my $fail := Failure.new($!);
        my Mu $return := nqp::getlexrel(nqp::ctxcallerskipthunks(nqp::ctx()), 'RETURN');
        $return($fail) unless nqp::isnull($return);
        $fail.exception.throw
    }

    method is-compile-time { False }
}

my class X::SecurityPolicy is Exception {}

my class X::SecurityPolicy::Eval is X::SecurityPolicy {
    has $.payload = "EVAL is a very dangerous function!!!";

    my role SlurpySentry { }

    method message() {
        do {
            # Remove spaces for die(*@msg)/fail(*@msg) forms
            given $.payload {
                when SlurpySentry {
                    $_.list.join;
                }
                default {
                    .Str;
                }
            }
        } ~ " (use MONKEY-SEE-NO-EVAL to override,\nbut only if you're VERY sure your data contains no injection attacks)";
    }
    method Numeric() { $.payload.Numeric }
    method from-slurpy (|cap) {
        self.new(:payload(cap does SlurpySentry))
    }
}

my class X::AdHoc is Exception {
    has $.payload = "Unexplained error";

    my role SlurpySentry { }

    method message() {
        # Remove spaces for die(*@msg)/fail(*@msg) forms
        given $.payload {
            when SlurpySentry {
                $_.list.join;
            }
            default {
                .Str;
            }
        }
    }
    method Numeric() { $.payload.Numeric }
    method from-slurpy (|cap) {
        self.new(:payload(cap does SlurpySentry))
    }
}

my class X::NQP::NotFound is Exception {
    has $.op;
    method message() {
        "Could not find nqp::$.op, did you forget 'use nqp;' ?"
    }
}
my class X::Dynamic::NotFound is Exception {
    has $.name;
    method message() {
        "Dynamic variable $.name not found";
    }
}
my class X::Method::NotFound is Exception {
    has $.invocant;
    has $.method;
    has $.typename;
    has Bool $.private = False;
    method message() {
        my $message = $.private
          ?? "No such private method '$.method' for invocant of type '$.typename'"
          !! "No such method '$.method' for invocant of type '$.typename'";
        if $.method eq 'length' {
            $message ~= "\nDid you mean 'elems', 'chars', 'graphs' or 'codes'?";
        }
        elsif $.method eq 'bytes' {
            $message ~= "\nDid you mean '.encode(\$encoding).bytes'?";
        }
        $message;
    }
}

my class X::Method::InvalidQualifier is Exception {
    has $.method;
    has $.invocant;
    has $.qualifier-type;
    method message() {
          "Cannot dispatch to method $.method on {$.qualifier-type.^name} "
        ~ "because it is not inherited or done by {$.invocant.^name}";
    }
}

my class X::Role::Parametric::NoSuchCandidate is Exception {
    has Mu $.role;
    method message {
        "No appropriate parametric role variant available for '"
        ~ $.role.^name
        ~ "'";
    }
}

my class X::Pragma::NoArgs is Exception {
    has $.name;
    method message { "The '$.name' pragma does not take any arguments." }
}
my class X::Pragma::CannotPrecomp is Exception {
    has $.what = 'This compilation unit';
    method message { "$.what may not be pre-compiled" }
}
my class X::Pragma::CannotWhat is Exception {
    has $.what;
    has $.name;
    method message { "'$.what $.name' is not an option." }
}
my class X::Pragma::MustOneOf is Exception {
    has $.name;
    has $.alternatives;
    method message { "'$.name' pragma expects one parameter out of $.alternatives." }
}
my class X::Pragma::UnknownArg is Exception {
    has $.name;
    has $.arg;
    method message { "Unknown argument '{$.arg.perl}' specified with the '$.name' pragma." }
}
my class X::Pragma::OnlyOne is Exception {
    has $.name;
    method message { "The '$.name' pragma only takes one argument." }
}

my role X::Control is Exception {
}
my class CX::Next does X::Control {
    method message() { "<next control exception>" }
}
my class CX::Redo does X::Control {
    method message() { "<redo control exception>" }
}
my class CX::Last does X::Control {
    method message() { "<last control exception>" }
}
my class CX::Take does X::Control {
    method message() { "<take control exception>" }
}
my class CX::Warn does X::Control {
    has $.message;
}
my class CX::Succeed does X::Control {
    method message() { "<succeed control exception>" }
}
my class CX::Proceed does X::Control {
    method message() { "<proceed control exception>" }
}

sub EXCEPTION(|) {
    my Mu $vm_ex   := nqp::shift(nqp::p6argvmarray());
    my Mu $payload := nqp::getpayload($vm_ex);
    if nqp::p6bool(nqp::istype($payload, Exception)) {
        nqp::bindattr($payload, Exception, '$!ex', $vm_ex);
        $payload;
    } else {
        my int $type = nqp::getextype($vm_ex);
        my $ex;
        if $type == nqp::const::CONTROL_NEXT {
            $ex := CX::Next.new();
        }
        elsif $type == nqp::const::CONTROL_REDO {
            $ex := CX::Redo.new();
        }
        elsif $type == nqp::const::CONTROL_LAST {
            $ex := CX::Last.new();
        }
        elsif $type == nqp::const::CONTROL_TAKE {
            $ex := CX::Take.new();
        }
        elsif $type == nqp::const::CONTROL_WARN {
            my str $message = nqp::getmessage($vm_ex);
            $message = 'Warning' if nqp::isnull_s($message) || $message eq '';
            $ex := CX::Warn.new(:$message);
        }
        elsif $type == nqp::const::CONTROL_SUCCEED {
            $ex := CX::Succeed.new();
        }
        elsif $type == nqp::const::CONTROL_PROCEED {
            $ex := CX::Proceed.new();
        }
        elsif !nqp::isnull_s(nqp::getmessage($vm_ex)) &&
                nqp::p6box_s(nqp::getmessage($vm_ex)) ~~ /"Method '" (.*?) "' not found for invocant of class '" (.+)\'$/ {
            $ex := X::Method::NotFound.new(
                method   => ~$0,
                typename => ~$1,
            );
        }
        else {
            $ex := nqp::create(X::AdHoc);
            nqp::bindattr($ex, X::AdHoc, '$!payload', nqp::p6box_s(nqp::getmessage($vm_ex)));
        }
        nqp::bindattr($ex, Exception, '$!ex', $vm_ex);
        $ex;
    }
}

my class X::Comp::AdHoc { ... }
sub COMP_EXCEPTION(|) {
    my Mu $vm_ex   := nqp::shift(nqp::p6argvmarray());
    my Mu $payload := nqp::getpayload($vm_ex);
    if nqp::p6bool(nqp::istype($payload, Exception)) {
        nqp::bindattr($payload, Exception, '$!ex', $vm_ex);
        $payload;
    } else {
        my $ex := nqp::create(X::Comp::AdHoc);
        nqp::bindattr($ex, Exception, '$!ex', $vm_ex);
        nqp::bindattr($ex, X::AdHoc, '$!payload', nqp::p6box_s(nqp::getmessage($vm_ex)));
        $ex;
    }
}


do {

    sub print_exception(|) {
        my Mu $ex := nqp::atpos(nqp::p6argvmarray(), 0);
        try {
            my $e := EXCEPTION($ex);
            my $v := $e.vault-backtrace;
            my Mu $err := nqp::getstderr();

            $e.backtrace;  # This is where most backtraces actually happen
            if $e.is-compile-time || $e.backtrace && $e.backtrace.is-runtime {
                nqp::printfh($err, $e.gist);
                nqp::printfh($err, "\n");
                if $v {
                   nqp::printfh($err, "Actually thrown at:\n");
                   nqp::printfh($err, $v.Str);
                   nqp::printfh($err, "\n");
                }
            }
            else {
                nqp::printfh($err, "===SORRY!===\n");
                nqp::printfh($err, $e.Str);
                nqp::printfh($err, "\n");
            }
            Rakudo::Internals.THE_END();
            CONTROL { when CX::Warn { .resume } }
        }
        if $! {
            nqp::rethrow(nqp::getattr(nqp::decont($!), Exception, '$!ex'));
            $ex
        }
    }

    sub print_control(|) {
        my Mu $ex := nqp::atpos(nqp::p6argvmarray(), 0);
        my int $type = nqp::getextype($ex);
        my $backtrace = Backtrace.new(nqp::backtrace($ex), 0);
        if ($type == nqp::const::CONTROL_WARN) {
            my Mu $err := nqp::getstderr();
            my $msg = nqp::p6box_s(nqp::getmessage($ex));
            nqp::printfh($err, $msg.chars ?? "$msg" !! "Warning");
            nqp::printfh($err, $backtrace.first-none-setting-line);
            nqp::resume($ex)
        }
        if $type == nqp::const::CONTROL_LAST {
            X::ControlFlow.new(illegal => 'last', enclosing => 'loop construct', :$backtrace).throw;
        }
        elsif $type == nqp::const::CONTROL_NEXT {
            X::ControlFlow.new(illegal => 'next', enclosing => 'loop construct', :$backtrace).throw;
        }
        elsif $type == nqp::const::CONTROL_REDO {
            X::ControlFlow.new(illegal => 'redo', enclosing => 'loop construct', :$backtrace).throw;
        }
        elsif $type == nqp::const::CONTROL_PROCEED {
            X::ControlFlow.new(illegal => 'proceed', enclosing => 'when clause', :$backtrace).throw;
        }
        elsif $type == nqp::const::CONTROL_SUCCEED {
            # XXX: should work like leave() ?
            X::ControlFlow.new(illegal => 'succeed', enclosing => 'when clause', :$backtrace).throw;
        }
        elsif $type == nqp::const::CONTROL_TAKE {
            X::ControlFlow.new(illegal => 'take', enclosing => 'gather', :$backtrace).throw;
        }
        elsif $type == nqp::const::CONTROL_EMIT {
            X::ControlFlow.new(illegal => 'emit', enclosing => 'supply or react', :$backtrace).throw;
        }
        elsif $type == nqp::const::CONTROL_DONE {
            X::ControlFlow.new(illegal => 'done', enclosing => 'supply or react', :$backtrace).throw;
        }
        else {
            X::ControlFlow.new(illegal => 'control exception', enclosing => 'handler', :$backtrace).throw;
        }
    }

    my Mu $comp := nqp::getcomp('perl6');
    $comp.^add_method('handle-exception',
        method (|) {
            my Mu $ex := nqp::atpos(nqp::p6argvmarray(), 1);
            print_exception($ex);
            nqp::exit(1);
            0;
        }
    );
    $comp.^add_method('handle-control',
        method (|) {
            my Mu $ex := nqp::atpos(nqp::p6argvmarray(), 1);
            print_control($ex);
            nqp::rethrow($ex);
        }
    );

}

my role X::OS is Exception {
    has $.os-error;
    method message() { $.os-error }
}

my role X::IO does X::OS { };

my class X::IO::Unknown does X::IO {
    has $.trying;
    method message { "Unknown IO error trying '$.trying'" }
}
my class X::IO::Rename does X::IO {
    has $.from;
    has $.to;
    method message() {
        "Failed to rename '$.from' to '$.to': $.os-error"
    }
}

my class X::IO::Copy does X::IO {
    has $.from;
    has $.to;
    method message() {
        "Failed to copy '$.from' to '$.to': $.os-error"
    }
}

my class X::IO::Move does X::IO {
    has $.from;
    has $.to;
    method message() {
        "Failed to move '$.from' to '$.to': $.os-error"
    }
}

my class X::IO::DoesNotExist does X::IO {
    has $.path;
    has $.trying;
    method message() {
        "Failed to find '$.path' while trying to do '.$.trying'"
    }
}

my class X::IO::NotAFile does X::IO {
    has $.path;
    has $.trying;
    method message() {
        "'$.path' is not a regular file while trying to do '.$.trying'"
    }
}

my class X::IO::Directory does X::IO {
    has $.path;
    has $.trying;
    has $.use;
    method message () {
        my $x = "'$.path' is a directory, cannot do '.$.trying' on a directory";
        if $.use { $x ~= ", try '{$.use}()' instead" }
        $x;
    }
}

my class X::IO::Symlink does X::IO {
    has $.target;
    has $.name;
    method message() {
        "Failed to create symlink called '$.name' on target '$.target': $.os-error"
    }
}

my class X::IO::Link does X::IO {
    has $.target;
    has $.name;
    method message() {
        "Failed to create link called '$.name' on target '$.target': $.os-error"
    }
}

my class X::IO::Mkdir does X::IO {
    has $.path;
    has $.mode;
    method message() {
        "Failed to create directory '$.path' with mode '0o{$.mode.fmt("%03o")}': $.os-error"
    }
}

my class X::IO::Chdir does X::IO {
    has $.path;
    method message() {
        "Failed to change the working directory to '$.path': $.os-error"
    }
}

my class X::IO::Dir does X::IO {
    has $.path;
    method message() {
        "Failed to get the directory contents of '$.path': $.os-error"
    }
}

my class X::IO::Cwd does X::IO {
    method message() {
        "Failed to get the working directory: $.os-error"
    }
}

my class X::IO::Rmdir does X::IO {
    has $.path;
    method message() {
        "Failed to remove the directory '$.path': $.os-error"
    }
}

my class X::IO::Unlink does X::IO {
    has $.path;
    method message() {
        "Failed to remove the file '$.path': $.os-error"
    }
}

my class X::IO::Chmod does X::IO {
    has $.path;
    has $.mode;
    method message() {
        "Failed to set the mode of '$.path' to '0o{$.mode.fmt("%03o")}': $.os-error"
    }
}

my role X::Comp is Exception {
    has $.filename;
    has $.pos;
    has $.line;
    has $.column;
    has @.modules;
    has $.is-compile-time = False;
    has $.pre;
    has $.post;
    has @.highexpect;
    multi method gist(::?CLASS:D: :$sorry = True, :$expect = True) {
        if $.is-compile-time {
            my ($red,$clear,$green,$yellow,$eject) =
              Rakudo::Internals.error-rcgye;
            my $r = $sorry ?? self.sorry_heading() !! "";
            $r ~= "$.message\nat $.filename():$.line";
            $r ~= "\n------> $green$.pre$yellow$eject$red$.post$clear" if defined $.pre;
            if $expect && @.highexpect {
                $r ~= "\n    expecting any of:";
                for @.highexpect {
                    $r ~= "\n        $_";
                }
            }
            for @.modules.reverse[1..*] {
                my $line = nqp::p6box_i($_<line>);
                $r ~= $_<module>.defined
                        ?? "\n  from module $_<module> ($_<filename> line $line)"
                        !! "\n  from $_<filename> line $line";
            }
            $r;
        }
        else {
            self.Exception::gist;
        }
    }
    method sorry_heading() {
        my ($red, $clear) = Rakudo::Internals.error-rcgye;
        "$red==={$clear}SORRY!$red===$clear Error while compiling $.filename\n"
    }
    method SET_FILE_LINE($file, $line) {
        $!filename = $file;
        $!line     = $line;
        $!is-compile-time = True;
    }
}

my class X::Comp::Group is Exception {
    has $.panic;
    has @.sorrows;
    has @.worries;

    method is-compile-time() { True }

    multi method gist(::?CLASS:D:) {
        my $r = "";
        if $.panic || @.sorrows {
            my ($red, $clear) = Rakudo::Internals.error-rcgye;
            $r ~= "$red==={$clear}SORRY!$red===$clear\n";
            for @.sorrows {
                $r ~= .gist(:!sorry, :!expect) ~ "\n";
            }
            if $.panic {
                $r ~= $.panic.gist(:!sorry) ~ "\n";
            }
        }
        if @.worries {
            $r ~= $.panic || @.sorrows
                ?? "Other potential difficulties:\n"
                !! "Potential difficulties:\n";
            for @.worries {
                $r ~= .gist(:!sorry, :!expect).indent(4) ~ "\n";
            }
        }
        $r
    }

    method message() {
        my @m;
        for @.sorrows {
            @m.append(.message);
        }
        if $.panic {
            @m.append($.panic.message);
        }
        for @.worries {
            @m.append(.message);
        }
        @m.join("\n")
    }
}

my role X::MOP is Exception { }

my class X::Comp::BeginTime does X::Comp {
    has $.use-case;
    has $.exception;

    method message() {
        $!exception ~~ X::MOP
            ?? $!exception.message
            !! "An exception occurred while $!use-case"
    }

    multi method gist(::?CLASS:D: :$sorry = True) {
        my $r = $sorry ?? self.sorry_heading() !! "";
        $r ~= "$.message\nat $.filename():$.line";
        for @.modules.reverse[1..*] {
            my $line = nqp::p6box_i($_<line>);
            $r ~= $_<module>.defined
                    ?? "\n  from module $_<module> ($_<filename> line $line)"
                    !! "\n  from $_<filename> line $line";
        }
        unless $!exception ~~ X::MOP {
            $r ~= "\nException details:\n" ~ $!exception.gist.indent(2);
        }
        $r;
    }
}

# XXX a hack for getting line numbers from exceptions from the metamodel
my class X::Comp::AdHoc is X::AdHoc does X::Comp {
    method is-compile-time() { True }
}

my role X::Syntax does X::Comp { }
my role X::Pod                 { }

my class X::NYI is Exception {
    has $.feature;
    method message() { "$.feature not yet implemented. Sorry. " }
}
my class X::Comp::NYI is X::NYI does X::Comp { };
my class X::NYI::Available is X::NYI {
    has @.available = die("Must give :available<modules> for installation. ");
    method available-str {
        my @a = @.available;
        my $a = @a.pop;
        @a ?? (@a.join(', ') || (), $a).join(" or ") !! $a;
    }
    method message() {
        "Please install { self.available-str } for $.feature support. "
    }
}
my class X::NYI::BigInt is Exception {
    has $.op;
    has $.big;
    has $.side = 'right';
    method message() {
        "Big integer $!big not yet supported on {$!side}hand side of '$!op' operator"
    }
}
my class X::Experimental does X::Comp {
    has $.feature;
    has $.use = $!feature;
    method message() { "Use of $.feature is experimental; please 'use experimental :$.use'" }
}

my class X::Worry is Exception { }
my class X::Worry::P5 is X::Worry { }
my class X::Worry::P5::Reference is X::Worry::P5 {
    method message {
q/To pass an array, hash or sub to a function in Perl 6, just pass it as is.
For other uses of Perl 5's ref operator consider binding with ::= instead.
Parenthesize as \\(...) if you intended a capture of a single variable./
    }
}
my class X::Worry::P5::BackReference is X::Worry::P5 {
    method message {
q/To refer to a positional match capture, just use $0 (numbering starts at 0).
Parenthesize as \\(...) if you intended a capture of a single numeric value./
    }
}
my class X::Worry::P5::LeadingZero is X::Worry::P5 {
    has $.value;
    method message {
qq/Leading 0 does not indicate octal in Perl 6.
Please use 0o$!value if you mean that./
    }
}

my class X::Trait::Unknown is Exception {
    has $.type;       # is, will, of etc.
    has $.subtype;    # wrong subtype being tried
    has $.declaring;  # variable, sub, parameter, etc.
    method message () {
        "Can't use unknown trait '$.type $.subtype' in a$.declaring declaration."
    }
}
my class X::Comp::Trait::Unknown is X::Trait::Unknown does X::Comp { };

my class X::Trait::NotOnNative is Exception {
    has $.type;       # is, will, of etc.
    has $.subtype;    # wrong subtype being tried
    has $.native;     # type of native (optional)
    method message () {
        "Can't use trait '$.type $.subtype' on a native"
          ~ ( $.native ?? " $.native." !! "." );
    }
}
my class X::Comp::Trait::NotOnNative is X::Trait::NotOnNative does X::Comp { };

my class X::Trait::Scope is Exception {
    has $.type;       # is, will, of etc.
    has $.subtype;    # export
    has $.declaring;  # type name of the object
    has $.scope;      # not supported (but used) scope
    has $.supported;  # hint about what is allowed instead
    method message () {
        "Can't apply trait '$.type $.subtype' on a $.scope scoped $.declaring."
        ~ ( $.supported ?? " Only {$.supported.join(' and ')} scoped {$.declaring}s are supported." !! '' );
    }
}
my class X::Comp::Trait::Scope is X::Trait::Scope does X::Comp { };

my class X::OutOfRange is Exception {
    has $.what = 'Argument';
    has $.got = '<unknown>';
    has $.range = '<unknown>';
    has $.comment;
    method message() {
        my $result = $.comment.defined
           ?? "$.what out of range. Is: $.got, should be in $.range.gist(); $.comment"
           !! "$.what out of range. Is: $.got, should be in $.range.gist()";
        $result;
    }
}

my class X::Buf::AsStr is Exception {
    has $.method;
    method message() {
        "Cannot use a Buf as a string, but you called the $.method method on it";
    }
}
my class X::Buf::Pack is Exception {
    has $.directive;
    method message() {
        "Unrecognized directive '$.directive'";
    }
}

my class X::Buf::Pack::NonASCII is Exception {
    has $.char;
    method message() {
        "non-ASCII character '$.char' while processing an 'A' template in pack";
    }
}

my class X::Signature::Placeholder does X::Comp {
    has $.placeholder;
    method message() {
        "Placeholder variable '$.placeholder' cannot override existing signature";
    }
}

my class X::Placeholder::Block does X::Comp {
    has $.placeholder;
    method message() {
        "Placeholder variable $.placeholder may not be used here because the surrounding block takes no signature";
    }
}

my class X::Placeholder::NonPlaceholder does X::Comp {
    has $.variable_name;
    has $.placeholder;
    has $.decl;
    method message() {
        my $decl = $!decl ?? ' ' ~ $!decl !! '';
        "$!variable_name has already been used as a non-placeholder in the surrounding$decl block,\n" ~
        "  so you will confuse the reader if you suddenly declare $!placeholder here"
    }
}

my class X::Placeholder::Mainline is X::Placeholder::Block {
    method message() {
        "Cannot use placeholder parameter $.placeholder in the mainline"
    }
}

my class X::Placeholder::Attribute is X::Placeholder::Block {
    method message() {
        "Cannot use placeholder parameter $.placeholder in an attribute initializer"
    }
}

my class X::Undeclared does X::Comp {
    has $.what = 'Variable';
    has $.symbol;
    has @.suggestions;
    method message() {
        my $message := "$.what '$.symbol' is not declared";
        if +@.suggestions == 1 {
            $message := "$message. Did you mean '@.suggestions[0]'?";
        } elsif +@.suggestions > 1 {
            $message := "$message. Did you mean any of these?\n    { @.suggestions.join("\n    ") }\n";
        }
        $message;
    }
}

my class X::Attribute::Undeclared is X::Undeclared {
    has $.package-kind;
    has $.package-name;

    method message() {
        "Attribute $.symbol not declared in $.package-kind $.package-name";
    }
}

my class X::Attribute::Regex is X::Undeclared {
    method message() {
        "Attribute $.symbol not available inside of a regex, since regexes are methods on Cursor.\n" ~
            "Consider storing the attribute in a lexical, and using that in the regex.";
    }
}

my class X::Undeclared::Symbols does X::Comp {
    has %.post_types;
    has %.unk_types;
    has %.unk_routines;
    has %.routine_suggestion;
    has %.type_suggestion;
    multi method gist(X::Undeclared::Symbols:D: :$sorry = True) {
        ($sorry ?? self.sorry_heading() !! "") ~ self.message
    }
    method message(X::Undeclared::Symbols:D:) {
        sub l(@l) {
            my @lu = @l.map({ nqp::hllize($_) }).unique.sort;
            'used at line' ~ (@lu == 1 ?? ' ' !! 's ') ~ @lu.join(', ')
        }
        sub s(@s) {
            "Did you mean '{ @s.join("', '") }'?";
        }
        my $r = "";
        if %.post_types {
            $r ~= "Illegally post-declared type" ~ (%.post_types.elems == 1 ?? "" !! "s") ~ ":\n";
            for %.post_types.sort(*.key) {
                $r ~= "    $_.key() &l($_.value)\n";
            }
        }
        if %.unk_types {
            $r ~= "Undeclared name" ~ (%.unk_types.elems == 1 ?? "" !! "s") ~ ":\n";
            for %.unk_types.sort(*.key) {
                $r ~= "    $_.key() &l($_.value)";
                if +%.type_suggestion{$_.key()} {
                    $r ~= ". " ~ s(%.type_suggestion{$_.key()});
                }
                $r ~= "\n";
            }
        }
        if %.unk_routines {
            my $obs = {
                y => "tr",
                qr => "rx",
                local => "temp (or dynamic var)",
                new => "method call syntax",
                foreach => "for",
            }
            $r ~= "Undeclared routine" ~ (%.unk_routines.elems == 1 ?? "" !! "s") ~ ":\n";
            for %.unk_routines.sort(*.key) {
                $r ~= "    $_.key() &l($_.value)";
                $r ~= " (in Perl 6 please use " ~ $obs{$_.key()} ~ " instead)" if $obs{$_.key()};
                if +%.routine_suggestion{$_.key()}.list {
                    $r ~= ". " ~ s(%.routine_suggestion{$_.key()}.list);
                }
                $r ~= "\n";
            }
        }
        $r
    }
}

my class X::Redeclaration does X::Comp {
    has $.symbol;
    has $.postfix = '';
    has $.what    = 'symbol';
    method message() {
        "Redeclaration of $.what $.symbol$.postfix";
    }
}

my class X::Redeclaration::Outer does X::Comp {
    has $.symbol;
    method message() {
        "Lexical symbol '$.symbol' is already bound to an outer symbol;\n" ~
        "the implicit outer binding must be rewritten as OUTER::<$.symbol>\n" ~
        "before you can unambiguously declare a new '$.symbol' in this scope";
    }
}

my class X::Dynamic::Postdeclaration does X::Comp {
    has $.symbol;
    method message() {
        "Illegal post-declaration of dynamic variable '$.symbol';\n" ~
        "earlier access must be written as CALLERS::<$.symbol>\n" ~
        "if that's what you meant"
    }
}

my class X::Dynamic::Package does X::Comp {
    has $.symbol;
    method message() {
        "Dynamic variables cannot have package-like names, like $!symbol"
    }
}

my class X::Import::Redeclaration does X::Comp {
    has @.symbols;
    has $.source-package-name;
    method message() {
        @.symbols == 1
            ?? "Cannot import symbol @.symbols[0] from $.source-package-name, because it already exists in this lexical scope"
            !! ("Cannot import the following symbols from $.source-package-name, because they already exist in this lexical scope: ", @.symbols.join(', '));
    }
}

my class X::Import::OnlystarProto does X::Comp {
    has @.symbols;
    has $.source-package-name;
    method message() {
        @.symbols == 1
            ?? "Cannot import symbol @.symbols[0] from $.source-package-name, only onlystar-protos can be merged"
            !! ("Cannot import the following symbols from $.source-package-name, only onlystar-protos can be merged: ", @.symbols.join(', '));
    }
}

my class X::PoisonedAlias does X::Comp {
    has $.alias;
    has $.package-type = 'package';
    has $.package-name;
    method message() {
        "Cannot use poisoned alias $!alias, because it was declared by several {$!package-type}s." ~
        ($!package-name ?? "\nPlease access it via explicit package name like: {$!package-name}::{$!alias}" !! '')
    }
}

my class X::Phaser::Multiple does X::Comp {
    has $.block;
    method message() { "Only one $.block block is allowed" }
}

my class X::Obsolete does X::Comp {
    has $.old;
    has $.replacement; # can't call it $.new, collides with constructor
    has $.when = 'in Perl 6';
    method message() { "Unsupported use of $.old; $.when please use $.replacement" }
}

my class X::Parameter::Default does X::Comp {
    has $.how;
    has $.parameter;
    method message() {
        $.parameter
            ?? "Cannot put default on $.how parameter $.parameter"
            !! "Cannot put default on anonymous $.how parameter";
    }
}

my class X::Parameter::Default::TypeCheck does X::Comp {
    has $.got is default(Nil);
    has $.expected is default(Nil);
    method message() {
        "Default value '$.got.gist()' will never bind to a parameter of type $.expected.^name()"
    }
}

my class X::Parameter::AfterDefault does X::Syntax {
    has $.type;
    has $.modifier;
    has $.default;
    method message() {
        "The $.type \"$.modifier\" came after the default value\n"
        ~ "(did you mean \"...$.modifier $.default\"?)"
    }
}

my class X::Parameter::Placeholder does X::Comp {
    has $.parameter;
    has $.right;
    method message() {
        "In signature parameter, placeholder variables like $.parameter are illegal\n"
        ~ "you probably meant a named parameter: '$.right'";
    }
}

my class X::Parameter::Twigil does X::Comp {
    has $.parameter;
    has $.twigil;
    method message() {
        "In signature parameter $.parameter, it is illegal to use the $.twigil twigil";
    }
}

my class X::Parameter::MultipleTypeConstraints does X::Comp {
    has $.parameter;
    method message() {
        ($.parameter ?? "Parameter $.parameter" !! 'A parameter')
        ~ " may only have one prefix type constraint";
    }
}

my class X::Parameter::BadType does X::Comp {
    has Mu $.type;
    method message() {
        my $what = ~$!type.HOW.WHAT.^name.match(/ .* '::' <(.*)> HOW/) // 'Namespace';
        "$what $!type.^name() is insufficiently type-like to qualify a parameter"
    }
}

my class X::Parameter::WrongOrder does X::Comp {
    has $.misplaced;
    has $.parameter;
    has $.after;
    method message() {
        "Cannot put $.misplaced parameter $.parameter after $.after parameters";
    }
}

my class X::Parameter::InvalidType does X::Comp {
    has $.typename;
    has @.suggestions;
    method message() {
        my $msg := "Invalid typename '$.typename' in parameter declaration.";
        if +@.suggestions > 0 {
            $msg := $msg ~ " Did you mean '" ~ @.suggestions.join("', '") ~ "'?";
        }
        $msg;
    }
}

my class X::Parameter::RW is Exception {
    has $.got;
    has $.symbol;
    method message() {
        "Parameter '$.symbol' expected a writable container, but got $.got.^name() value"
    }
}

my class X::Parameter::TypedSlurpy does X::Comp {
    has $.kind;
    method message() {
        "Slurpy $.kind parameters with type constraints are not supported"
    }
}

my class X::Signature::NameClash does X::Comp {
    has $.name;
    method message() {
        "Name $.name used for more than one named parameter";
    }
}

my class X::Method::Private::Permission does X::Comp {
    has $.method;
    has $.source-package;
    has $.calling-package;
    method message() {
        "Cannot call private method '$.method' on package $.source-package because it does not trust $.calling-package";
    }
}

my class X::Method::Private::Unqualified does X::Comp {
    has $.method;
    method message() {
        "Private method call to $.method must be fully qualified with the package containing the method";
    }
}

my class X::Adverb is Exception {
    has $.what;
    has $.source;
    has @.unexpected;
    has @.nogo;
    method message {
        my $text = '';
        if @!unexpected.elems -> $elems {
            $text = $elems > 1
              ?? "$elems unexpected adverbs (@.unexpected[])"
              !! "Unexpected adverb '@!unexpected[0]'"
        }
        if @!nogo {
            $text ~= $text ?? " and u" !! "U";
            $text ~= "nsupported combination of adverbs (@.nogo[])";
        }
        $text ~ " passed to $!what on $!source";
    }
    method unexpected { @!unexpected.sort }
    method nogo       { @!nogo.sort }
}

my class X::Bind is Exception {
    has $.target;
    method message() {
        $.target.defined
            ?? "Cannot bind to $.target"
            !! 'Cannot use bind operator with this left-hand side'
    }
}
my class X::Bind::NativeType does X::Comp {
    has $.name;
    method message() {
        "Cannot bind to natively typed variable '$.name'; use assignment instead"
    }
}
my class X::Bind::Slice is Exception  {
    has $.type;
    method message() {
        "Cannot bind to {$.type.^name} slice";
    }
}
my class X::Bind::ZenSlice is X::Bind::Slice {
    method message() {
        "Cannot bind to {$.type.^name} zen slice";
    }
}

my class X::Subscript::Negative is Exception {
    has $.index;
    has $.type;
    method message() {
        "Calculated index ({$.index}) is negative, but {$.type.^name} allows only 0-based indexing";
    }
}

my class X::Invalid::Value is Exception {
    has $.method;
    has $.name;
    has $.value;
    method message {
        "Invalid value '$.value' for :$.name on method $.method"
    }
}

my class X::Value::Dynamic does X::Comp {
    has $.what;
    method message() { "$.what value must be known at compile time" }
}

my class X::Syntax::Name::Null does X::Syntax {
    method message() { 'Name component may not be null'; }
}

my class X::Syntax::UnlessElse does X::Syntax {
    method message() { '"unless" does not take "else", please rewrite using "if"' }
}

my class X::Syntax::KeywordAsFunction does X::Syntax {
    has $.word;
    has $.needparens;
    method message {
        "Word '$.word' interpreted as '{$.word}()' function call; please use whitespace "
            ~ ($.needparens ?? 'around the parens' !! 'instead of parens')
    }
}

my class X::Syntax::Malformed::Elsif does X::Syntax {
    has $.what = 'else if';
    method message() { qq{In Perl 6, please use "elsif' instead of "$.what"} }
}

my class X::Syntax::Reserved does X::Syntax {
    has $.reserved;
    has $.instead = '';
    method message() { "The $.reserved is reserved$.instead" }
}

my class X::Syntax::P5 does X::Syntax {
    method message() { 'This appears to be Perl 5 code' }
}

my class X::Syntax::NegatedPair does X::Syntax {
    has $.key;
    method message() { "Argument not allowed on negated pair with key '$.key'" }
}

my class X::Syntax::Variable::Numeric does X::Syntax {
    has $.what = 'variable';
    method message() { "Cannot declare a numeric $.what" }
}

my class X::Syntax::Variable::Match does X::Syntax {
    method message() { 'Cannot declare a match variable' }
}

my class X::Syntax::Variable::Initializer does X::Syntax {
    has $.name = '<anon>';
    method message() { "Cannot use variable $!name in declaration to initialize itself" }
}


my class X::Syntax::Variable::Twigil does X::Syntax {
    has $.what = 'variable';
    has $.twigil;
    has $.scope;
    has $.additional;
    method message() { "Cannot use $.twigil twigil on '$.scope' $.what$.additional" }
}

my class X::Syntax::Variable::IndirectDeclaration does X::Syntax {
    method message() { 'Cannot declare a variable by indirect name (use a hash instead?)' }
}

my class X::Syntax::Variable::BadType does X::Comp {
    has Mu $.type;
    method message() {
        my $what = ~$!type.HOW.WHAT.^name.match(/ .* '::' <(.*)> HOW/) // 'Namespace';
        "$what $!type.^name() is insufficiently type-like to qualify a variable"
    }
}

my class X::Syntax::Variable::ConflictingTypes does X::Comp {
    has Mu $.outer;
    has Mu $.inner;
    method message() {
        "$!inner.^name() not allowed here; variable list already declared with type $!outer.^name()"
    }
}

my class X::Syntax::Augment::WithoutMonkeyTyping does X::Syntax {
    method message() { "augment not allowed without 'use MONKEY-TYPING'" };
}

my class X::Syntax::Augment::Illegal does X::Syntax {
    has $.package;
    method message() { "Cannot augment $.package because it is closed" };
}

my class X::Syntax::Augment::Adverb does X::Syntax {
    method message() { "Cannot put adverbs on a typename when augmenting" }
}

my class X::Syntax::Type::Adverb does X::Syntax {
    has $.adverb;
    method message() { "Cannot use adverb $.adverb on a type name (only 'ver' and 'auth' are understood)" }
}

my class X::Syntax::Argument::MOPMacro does X::Syntax {
    has $.macro;
    method message() { "Cannot give arguments to $.macro" };
}

my class X::Role::Initialization is Exception {
    has $.role;
    method message() { "Can only supply an initialization value for a role if it has a single public attribute, but this is not the case for '{$.role.^name}'" }
}

my class X::Syntax::Comment::Embedded does X::Syntax {
    method message() { "Opening bracket required for #` comment" }
}

my class X::Syntax::Pod::BeginWithoutIdentifier does X::Syntax does X::Pod {
    method message() {
        '=begin must be followed by an identifier; (did you mean "=begin pod"?)'
    }
}

my class X::Syntax::Pod::BeginWithoutEnd does X::Syntax does X::Pod {
    has $.type;
    has $.spaces;
    has $.instead;
    method message() {
        if $.instead {
            qq{Expected "=end $.type" to terminate "=begin $.type"; found "=end $.instead" instead.}
        } else {
            "'=begin' not terminated by matching '$.spaces=end $.type'"
        }
    }
}

my class X::Syntax::Confused does X::Syntax {
    has $.reason = 'unknown';
    method message() { $.reason eq 'unknown' ?? 'Confused' !! $.reason }
}

my class X::Syntax::Malformed does X::Syntax {
    has $.what;
    method message() { "Malformed $.what" }
}
my class X::Syntax::Missing does X::Syntax {
    has $.what;
    method message() { "Missing $.what" }
}
my class X::Syntax::BlockGobbled does X::Syntax {
    has $.what;
    method message() {
        my $looks_like_type = $.what ~~ /'::' | <[A..Z]><[a..z]>+/;
        $.what ~~ /^'is '/
            ?? "Trait '$.what' needs whitespace before block"
            !! "{ $.what ?? "Function '$.what'" !! 'Expression' } needs parens to avoid gobbling block" ~
                    ($looks_like_type ?? " (or perhaps it's a class that's not declared or available in this scope?)" !! "");
    };
}

my class X::Syntax::ConditionalOperator::PrecedenceTooLoose does X::Syntax {
    has $.operator;
    method message() { "Precedence of $.operator is too loose to use inside ?? !!; please parenthesize" }
}

my class X::Syntax::ConditionalOperator::SecondPartGobbled does X::Syntax {
    method message() { "Your !! was gobbled by the expression in the middle; please parenthesize" }
}

my class X::Syntax::ConditionalOperator::SecondPartInvalid does X::Syntax {
    has $.second-part;
    method message() { "Please use !! rather than $.second-part" }
}

my class X::Syntax::Perl5Var does X::Syntax {
    has $.name;
    my %m =
      '$*'  => '^^ and $$',
      '$"'  => '.join() method',
      '$$'  => '$*PID',
      '$('  => '$*GID',
      '$)'  => '$*EGID',
      '$<'  => '$*UID',
      '$>'  => '$*EUID',
      '$;'  => 'real multidimensional hashes',
      '$&'  => '$<>',
      '$`'  => '$/.prematch',
      '$\'' => '$/.postmatch',
      '$,'  => '$*OUT.output_field_separator()',
      '$.'  => "the filehandle's .ins method",
      '$/'  => "the filehandle's .nl-in attribute",
      '$\\' => "the filehandle's .nl-out attribute",
      '$|'  => ':autoflush on open',
      '$?'  => '$! for handling child errors also',
      '$@'  => '$!',
      '$#'  => '.fmt',
      '$['  => 'user-defined array indices',
      '$]'  => '$*PERL.version or $*PERL.compiler.version',

      '$^C' => 'COMPILING namespace',
      '$^D' => '$*DEBUGGING',
      '$^E' => '$!.extended_os_error',
      '$^F' => '$*SYSTEM_FD_MAX',
      '$^H' => '$?FOO variables',
      '$^I' => '$*INPLACE',
      '$^M' => 'a global form such as $*M',
      '$^N' => '$/[*-1]',
      '$^O' => '$?DISTRO.name or $*DISTRO.name',
      '$^R' => 'an explicit result variable',
      '$^S' => 'context function',
      '$^T' => '$*INITTIME',
      '$^V' => '$*PERL.version or $*PERL.compiler.version',
      '$^W' => '$*WARNING',
      '$^X' => '$*EXECUTABLE-NAME',

      '$:'  => 'Form module',
      '$-'  => 'Form module',
      '$+'  => 'Form module',
      '$='  => 'Form module',
      '$%'  => 'Form module',
      '$^'  => 'Form module',
      '$~'  => 'Form module',
      '$^A' => 'Form module',
      '$^L' => 'Form module',

      '@-'  => '.from method',
      '@+'  => '.to method',

      '%-'  => '.from method',
      '%+'  => '.to method',
      '%^H' => '$?FOO variables',
    ;
    method message() {
        my $v = $.name ~~ m/ <[ $ @ % & ]> [ \^ <[ A..Z ]> | \W ] /;
        $v
          ?? %m{~$v}
            ?? "Unsupported use of $v variable; in Perl 6 please use {%m{~$v}}"
            !! "Unsupported use of $v variable"
          !! 'Weird unrecognized variable name: ' ~ $.name;
    }
}

my class X::Syntax::Self::WithoutObject does X::Syntax {
    method message() { "'self' used where no object is available" }
}
my class X::Syntax::VirtualCall does X::Syntax {
    has $.call;
    method message() { "Virtual method call $.call may not be used on partially constructed object (maybe you mean {$.call.subst('.','!')} for direct attribute access here?)" }
}
my class X::Syntax::NoSelf does X::Syntax {
    has $.variable;
    method message() { "Variable $.variable used where no 'self' is available" }
}

my class X::Syntax::Number::RadixOutOfRange does X::Syntax {
    has $.radix;
    method message() { "Radix $.radix out of range (allowed: 2..36)" }
}

my class X::Syntax::Number::IllegalDecimal does X::Syntax {
    method message() { "Decimal point must be followed by digit" }
}

my class X::Syntax::Number::LiteralType does X::Syntax {
    has $.varname;
    has $.vartype;
    has $.value;
    has $.valuetype;
    has $.suggestiontype;

    method message() {
        my $vartype := $!vartype.WHAT.^name;
        my $value := $!value.perl;
        my $val = "Cannot assign a literal of type {$.valuetype} ($value) to a variable of type $vartype. You can declare the variable to be of type $.suggestiontype, or try to coerce the value with { $value ~ '.' ~ $vartype } or $vartype\($value\)";
        try $val ~= ", or just write the value as " ~ $!value."$vartype"().perl;
        $val;
    }
}

my class X::Syntax::NonAssociative does X::Syntax {
    has $.left;
    has $.right;
    method message() {
        "Only identical operators may be list associative; since '$.left' and '$.right' differ, they are non-associative and you need to clarify with parentheses";
    }
}

my class X::Syntax::CannotMeta does X::Syntax {
    has $.meta;
    has $.operator;
    has $.reason;
    has $.dba;
    method message() {
        "Cannot $.meta $.operator because $.dba operators are $.reason";
    }
}

my class X::Syntax::Adverb does X::Syntax {
    has $.what;

    method message() { "You can't adverb " ~ $.what }
}

my class X::Syntax::Regex::Adverb does X::Syntax {
    has $.adverb;
    has $.construct;
    method message() { "Adverb $.adverb not allowed on $.construct" }
}

my class X::Syntax::Regex::UnrecognizedMetachar does X::Syntax {
    has $.metachar;
    method message() { "Unrecognized regex metacharacter $.metachar (must be quoted to match literally)" }
}

my class X::Syntax::Regex::UnrecognizedModifier does X::Syntax {
    has $.modifier;
    method message() { "Unrecognized regex modifier :$.modifier" }
}

my class X::Syntax::Regex::NullRegex does X::Syntax {
    method message() { 'Null regex not allowed' }
}

my class X::Syntax::Regex::MalformedRange does X::Syntax {
    method message() { 'Malformed Range' }
}

my class X::Syntax::Regex::Unspace does X::Syntax {
    has $.char;
    method message { "No unspace allowed in regex; if you meant to match the literal character, " ~
        "please enclose in single quotes ('" ~ $.char ~ "') or use a backslashed form like \\x" ~
        sprintf('%02x', $.char.ord)
    }
}

my class X::Syntax::Regex::Unterminated does X::Syntax {
    method message { 'Regex not terminated.' }
}

my class X::Syntax::Regex::SpacesInBareRange does X::Syntax {
    method message { 'Spaces not allowed in bare range.' }
}

my class X::Syntax::Regex::SolitaryQuantifier does X::Syntax {
    method message { 'Quantifier quantifies nothing' }
}

my class X::Syntax::Regex::NonQuantifiable does X::Syntax {
    method message { 'Can only quantify a construct that produces a match' }
}

my class X::Syntax::Regex::SolitaryBacktrackControl does X::Syntax {
    method message { "Backtrack control ':' does not seem to have a preceding atom to control" }
}

my class X::Syntax::Term::MissingInitializer does X::Syntax {
    method message { 'Term definition requires an initializer' }
}

my class X::Syntax::Variable::MissingInitializer does X::Syntax {
    has $.type;
    has $.implicit;
    method message {
        $.implicit ??
            "Variable definition of type $.type (implicit $.implicit) requires an initializer" !!
            "Variable definition of type $.type requires an initializer"
    }
}

my class X::Syntax::AddCategorical::TooFewParts does X::Syntax {
    has $.category;
    has $.needs;
    method message() { "Not enough symbols provided for categorical of type $.category; needs $.needs" }
}

my class X::Syntax::AddCategorical::TooManyParts does X::Syntax {
    has $.category;
    has $.needs;
    method message() { "Too many symbols provided for categorical of type $.category; needs only $.needs" }
}

my class X::Syntax::Signature::InvocantMarker does X::Syntax {
    method message() {
        "Can only use : as invocant marker in a signature after the first parameter"
    }
}

my class X::Syntax::Signature::InvocantNotAllowed does X::Syntax {
    method message() {
        "Can only use the : invocant marker in the signature for a method"
    }
}

my class X::Syntax::Extension::Category does X::Syntax {
    has $.category;
    method message() {
        "Cannot add tokens of category '$.category'";
    }
}

my class X::Syntax::Extension::Null does X::Syntax {
    method message() {
        "Null operator is not allowed";
    }
}

my class X::Syntax::Extension::TooComplex does X::Syntax {
    has $.name;
    method message() {
        "Colon pair value '$.name' too complex to use in name";
    }
}

my class X::Syntax::Extension::SpecialForm does X::Syntax {
    has $.category;
    has $.opname;
    has $.hint;
    method message() {
        "Cannot override $.category operator '$.opname', as it is a special form " ~
            "handled directly by the compiler" ~ ($!hint ?? "\n$!hint" !! "")
    }
}

my class X::Syntax::InfixInTermPosition does X::Syntax {
    has $.infix;
    method message() {
        "Preceding context expects a term, but found infix $.infix instead";
    }
}

my class X::Syntax::DuplicatedPrefix does X::Syntax {
    has $.prefixes;
    method message() {
        my $prefix = substr($.prefixes,0,1);
        "Expected a term, but found either infix $.prefixes or redundant prefix $prefix\n"
        ~ "  (to suppress this message, please use a space like $prefix $prefix)";
    }
}

my class X::Attribute::Package does X::Comp {
    has $.package-kind;
    has $.name;
    method message() { "A $.package-kind cannot have attributes, but you tried to declare '$.name'" }
}
my class X::Attribute::NoPackage does X::Comp {
    has $.name;
    method message() { "You cannot declare attribute '$.name' here; maybe you'd like a class or a role?" }
}
my class X::Attribute::Required does X::MOP {
    has $.name;
    method message() { "The attribute '$.name' is required, but you did not provide a value for it." }
}
my class X::Declaration::Scope does X::Comp {
    has $.scope;
    has $.declaration;
    method message() { "Cannot use '$.scope' with $.declaration declaration" }
}

my class X::Declaration::Scope::Multi is X::Declaration::Scope {
    method message() {
        "Cannot use '$.scope' with individual multi candidates. Please declare an {$.scope}-scoped proto instead";
    }
}

my class X::Declaration::OurScopeInRole does X::Comp {
    has $.declaration;
    method message() {
        "Cannot declare our-scoped $.declaration inside of a role\n" ~
        "(the scope inside of a role is generic, so there is no unambiguous\n" ~
        "package to install the symbol in)"
    }
}

my class X::Anon::Multi does X::Comp {
    has $.multiness;
    has $.routine-type = 'routine';
    method message() { "An anonymous $.routine-type may not take a $.multiness declarator" }
}
my class X::Anon::Augment does X::Comp {
    has $.package-kind;
    method message() { "Cannot augment anonymous $.package-kind" }
}
my class X::Augment::NoSuchType does X::Comp {
    has $.package-kind;
    has $.package;
    method message() { "You tried to augment $.package-kind $.package, but it does not exist" }
}

my class X::Routine::Unwrap is Exception {
    method message() { "Cannot unwrap routine: invalid wrap handle" }
}

my class X::Constructor::Positional is Exception {
    has $.type;
    method message() { "Default constructor for '" ~ $.type.^name ~ "' only takes named arguments" }
}

my class X::Hash::Store::OddNumber is Exception {
    method message() { "Odd number of elements found where hash initializer expected" }
}

my class X::Pairup::OddNumber is Exception {
    method message() { "Odd number of elements found for .pairup()" }
}

my class X::Match::Bool is Exception {
    has $.type;
    method message() { "Cannot use Bool as Matcher with '" ~ $.type ~ "'.  Did you mean to use \$_ inside a block?" }
}

my class X::Package::UseLib does X::Comp {
    has $.what;
    method message { "Cannot 'use lib' inside a $.what" }
}
my class X::Package::Stubbed does X::Comp {
    has @.packages;
    # TODO: suppress display of line number
    method message() {
        "The following packages were stubbed but not defined:\n    "
        ~ @.packages.join("\n    ");
    }
}

my class X::Phaser::PrePost is Exception {
    has $.phaser = 'PRE';
    has $.condition;
    method message {
        my $what = $.phaser eq 'PRE' ?? 'Precondition' !! 'Postcondition';
        $.condition.defined
            ?? "$what '$.condition.trim()' failed"
            !! "$what failed";
    }
}

my class X::Str::Numeric is Exception {
    has $.source;
    has $.pos;
    has $.reason;
    method source-indicator {
        my ($red,$clear,$green,$,$eject) = Rakudo::Internals.error-rcgye;
        my sub escape($str) { $str.perl.substr(1).chop }
        join '', "in '",
                $green,
                escape(substr($.source,0, $.pos)),
                $eject,
                $red,
                escape(substr($.source,$.pos)),
                $clear,
                "' (indicated by ",
                $eject,
                $clear,
                ")",
                ;
    }
    method message() {
        "Cannot convert string to number: $.reason $.source-indicator";
    }
}

my class X::Str::Match::x is Exception {
    has $.got is default(Nil);
    method message() {
        "in Str.match, got invalid value of type {$.got.^name} for :x, must be Int or Range"
    }
}

my class X::Str::Trans::IllegalKey is Exception {
    has $.key;
    method message {
        "in Str.trans, got illegal substitution key of type {$.key.^name} (should be a Regex or Str)"
    }
}
my class X::Str::Trans::InvalidArg is Exception {
    has $.got is default(Nil);
    method message() {
        "Only Pair objects are allowed as arguments to Str.trans, got {$.got.^name}";
    }
}

my class X::Range::InvalidArg is Exception {
    has $.got is default(Nil);
    method message() {
        "{$.got.^name} objects are not valid endpoints for Ranges";
    }
}

my class X::Sequence::Deduction is Exception {
    has $.from;
    method message() {
        $!from ?? "Unable to deduce arithmetic or geometric sequence from $!from (or did you really mean '..'?)"
               !! 'Unable to deduce sequence for some unfathomable reason'
    }
}

my class X::Cannot::Lazy is Exception {
    has $.action;
    has $.what;
    method message() {
        $.what
          ?? "Cannot $.action a lazy list onto a $.what"
          !! "Cannot $.action a lazy list";
    }
}
my class X::Cannot::Empty is Exception {
    has $.action;
    has $.what;
    method message() {
        "Cannot $.action from an empty $.what";
    }
}
my class X::Cannot::New is Exception {
    has $.class;
    method message() {
        "Cannot make a {$.class.^name} object using .new";
    }
}

my class X::Backslash::UnrecognizedSequence does X::Syntax {
    has $.sequence;
    method message() { "Unrecognized backslash sequence: '\\$.sequence'" }
}

my class X::Backslash::NonVariableDollar does X::Syntax {
    method message() { "Non-variable \$ must be backslashed" }
}

my class X::ControlFlow is Exception {
    has $.illegal;   # something like 'next'
    has $.enclosing; # ....  outside a loop
    has $.backtrace; # where the bogus control flow op was

    method backtrace() {
        $!backtrace || nextsame();
    }

    method message() { "$.illegal without $.enclosing" }
}
my class X::ControlFlow::Return is X::ControlFlow {
    method illegal()   { 'return'  }
    method enclosing() { 'Routine' }
    method message()   { 'Attempt to return outside of any Routine' }
}

my class X::Composition::NotComposable does X::Comp {
    has $.target-name;
    has $.composer;
    method message() {
        $.composer.^name ~ " is not composable, so $.target-name cannot compose it";
    }
}

my class X::TypeCheck is Exception {
    has $.operation;
    has $.got is default(Nil);
    has $.expected is default(Nil);
    method gotn() {
        my $perl = (try $!got.perl) // "?";
        $perl = "$perl.substr(0,21)..." if $perl.chars > 24;
        (try $!got.^name eq $!expected.^name
          ?? $perl
          !! "$!got.^name() ($perl)"
        ) // "?"
    }
    method expectedn() {
        (try $!got.^name eq $!expected.^name
          ?? $!expected.perl
          !! $!expected.^name
        ) // "?"
    }
    method priors() {
        my $prior = do if nqp::isconcrete($!got) && $!got ~~ Failure {
            "Earlier failure:\n " ~ $!got.mess ~ "\nFinal error:\n ";
        }
        else { '' }
        $prior;
    }
    method message() {
        self.priors() ~
        "Type check failed in $.operation; expected $.expectedn but got $.gotn";

    }
}

my class X::TypeCheck::Binding is X::TypeCheck {
    has $.symbol;
    method operation { 'binding' }
    method message() {
        if $.symbol {
            self.priors() ~
            "Type check failed in $.operation $.symbol; expected $.expectedn but got $.gotn";
        } else {
            self.priors() ~
            "Type check failed in $.operation; expected $.expectedn but got $.gotn";
        }
    }
}
my class X::TypeCheck::Return is X::TypeCheck {
    method operation { 'returning' }
    method message() {
        self.priors() ~
        "Type check failed for return value; expected $.expectedn but got $.gotn";
    }
}
my class X::TypeCheck::Assignment is X::TypeCheck {
    has $.symbol;
    method operation { 'assignment' }
    method message {
        self.priors() ~ do
            $.symbol.defined && $.symbol ne '$'
            ?? "Type check failed in assignment to $.symbol; expected $.expectedn but got $.gotn"
            !! "Type check failed in assignment; expected $.expectedn but got $.gotn";
    }
}
my class X::TypeCheck::Argument is X::TypeCheck {
    has $.protoguilt;
    has @.arguments;
    has $.objname;
    has $.signature;
    method message {
            my $multi = $!signature ~~ /\n/ // '';
            "Calling {$!objname}({ join(', ', @!arguments) }) will never work with " ~ (
                $!protoguilt ?? 'proto signature ' !!
                $multi       ?? 'any of these multi signatures:' !!
                                'declared signature '
            ) ~ $!signature;
    }
}

my class X::TypeCheck::Splice is X::TypeCheck does X::Comp {
    has $.action;
    method message {
        self.priors() ~
        "Type check failed in {$.action}; expected $.expectedn but got $.gotn";
    }

}

my class X::Assignment::RO is Exception {
    has $.typename = "value";
    method message {
        "Cannot modify an immutable {$.typename}";
    }
}

my class X::Assignment::RO::Comp does X::Comp {
    has $.variable;
    method message {
        "Cannot assign to readonly variable {$.variable}"
    }
}

my class X::Immutable is Exception {
    has $.typename;
    has $.method;
    method message {
        "Cannot call '$.method' on an immutable '$.typename'";
    }
}

my class X::NoDispatcher is Exception {
    has $.redispatcher;
    method message() {
        "$.redispatcher is not in the dynamic scope of a dispatcher";
    }
}

my class X::Localizer::NoContainer is Exception {
    has $.localizer;
    method message() {
        "Can only use '$.localizer' on a container";
    }
}

my class X::Mixin::NotComposable is Exception {
    has $.target;
    has $.rolish;
    method message() {
        "Cannot mix in non-composable type {$.rolish.^name} into object of type {$.target.^name}";
    }
}

my class X::Inheritance::Unsupported does X::Comp {
    # note that this exception is thrown before the child type object
    # has been composed, so it's useless to carry it around. Use the
    # name instead.
    has $.child-typename;
    has $.parent;
    method message {
        $.parent.^name ~ ' does not support inheritance, so '
        ~ $.child-typename ~ ' cannot inherit from it';
    }
}

my class X::Inheritance::UnknownParent is Exception {
    has $.child;
    has $.parent;
    has @.suggestions is rw;

    method message {
        my $message := "'" ~ $.child ~ "' cannot inherit from '" ~ $.parent ~ "' because it is unknown.";
        if +@.suggestions > 1 {
            $message := $message ~ "\nDid you mean one of these?\n    '" ~ @.suggestions.join("'\n    '") ~ "'\n";
        } elsif +@.suggestions == 1 {
            $message := $message ~ "\nDid you mean '" ~ @.suggestions[0] ~ "'?\n";
        }
        $message;
    }
}

my class X::Inheritance::SelfInherit is Exception {
    has $.name;

    method message {
        "'$.name' cannot inherit from itself."
    }
}

my class X::Export::NameClash does X::Comp {
    has $.symbol;
    method message() {
        "A symbol '$.symbol' has already been exported";
    }
}

my class X::HyperOp::NonDWIM is Exception {
    has &.operator;
    has $.left-elems;
    has $.right-elems;
    has $.recursing;
    method message() {
        "Lists on either side of non-dwimmy hyperop of &.operator.name() are not of the same length"
        ~ " while recursing" x +$.recursing
        ~ "\nleft: $.left-elems elements, right: $.right-elems elements";
    }
}

my class X::HyperOp::Infinite is Exception {
    has &.operator;
    has $.side;
    method message() {
        $.side eq "both"
            ?? "Lists on both sides of hyperop of &.operator.name() are known to be infinite"
            !! "List on $.side side of hyperop of &.operator.name() is known to be infinite"
    }
}

my class X::Set::Coerce is Exception {
    has $.thing;
    method message {
        "Cannot coerce object of type {$.thing.^name} to Set. To create a one-element set, pass it to the 'set' function";
    }
}


my role X::Temporal is Exception { }
my class X::Temporal::InvalidFormat does X::Temporal {
    has $.invalid-str;
    has $.target = 'Date';
    has $.format;
    method message() {
        "Invalid $.target string '$.invalid-str'; use $.format instead";
    }
}
my class X::DateTime::TimezoneClash does X::Temporal {
    method message() {
        'DateTime.new(Str): :timezone argument not allowed with a timestamp offset';
    }
}
my class X::DateTime::InvalidDeltaUnit does X::Temporal {
    has $.unit;
    method message() {
        "Cannnot use unit $.unit with Date.delta";
    }
}

my class X::Eval::NoSuchLang is Exception {
    has $.lang;
    method message() {
        "No compiler available for language '$.lang'";
    }
}

my class X::Import::MissingSymbols is Exception {
    has $.from;
    has @.missing;
    method message() {
        "Trying to import from '$.from', but the following symbols are missing: "
            ~ @.missing.join(', ');
    }
}

my class X::Import::NoSuchTag is Exception {
    has $.source-package;
    has $.tag;
    method message() {
        "Error while importing from '$.source-package': no such tag '$.tag'"
    }
}

my class X::Import::Positional is Exception {
    has $.source-package;
    method message() {
        "Error while importing from '$.source-package':\n"
        ~ "no EXPORT sub, but you provided positional argument in the 'use' statement"
    }
}

my class X::Numeric::Real is Exception {
    has $.target;
    has $.reason;
    has $.source;

    method message() {
        "Can not convert $.source to {$.target.^name}: $.reason";
    }
}

my class X::Numeric::DivideByZero is Exception {
    has $.using;
    has $.numerator;
    method message() {
        "Attempt to divide{$.numerator ?? " $.numerator" !! ''} by zero"
          ~ ( $.using ?? " using $.using" !! '' );
    }
}

my class X::Numeric::Overflow is Exception {
    method message() { "Numeric overflow" }
}

my class X::Numeric::Underflow is Exception {
    method message() { "Numeric underflow" }
}

my class X::Numeric::Confused is Exception {
    has $.num;
    has $.base;
    method message() {
        "This call only converts base-$.base strings to numbers; value {$.num.perl} is of type {$.num.WHAT.^name}, so cannot be converted!\n"
            ~ "(If you really wanted to convert {$.num.perl} to a base-$.base string, use {$.num.perl}.base($.base) instead.)";
    }
}

my class X::PseudoPackage::InDeclaration does X::Comp {
    has $.pseudo-package;
    has $.action;
    method message() {
        "Cannot use pseudo package $.pseudo-package in $.action";
    }
}

my class X::NoSuchSymbol is Exception {
    has $.symbol;
    method message { "No such symbol '$.symbol'" }
}

my class X::Item is Exception {
    has $.aggregate;
    has $.index;
    method message { "Cannot index {$.aggregate.^name} with $.index" }
}

my class X::Multi::Ambiguous is Exception {
    has $.dispatcher;
    has @.ambiguous;
    has $.capture;
    method message {
        join "\n",
            "Ambiguous call to '$.dispatcher.name()'; these signatures all match:",
            @.ambiguous.map(*.signature.perl)
    }
}

my class X::Multi::NoMatch is Exception {
    has $.dispatcher;
    has $.capture;
    method message {
        my @cand = $.dispatcher.dispatchees.map(*.signature.gist);
        my $where = so first / where /, @cand;
        my @bits;
        my @priors;
        if $.capture {
            for $.capture.list {
                try @bits.push($where ?? .perl !! .WHAT.perl );
                @bits.push($_.^name) if $!;
                when Failure {
                    @priors.push(" " ~ .mess);
                }
            }
            for $.capture.hash {
                if .value ~~ Failure {
                    @priors.push(" " ~ .value.mess);
                }
                if .value ~~ Bool {
                    @bits.push(':' ~ ('!' x !.value) ~ .key);
                }
                else {
                    try @bits.push(":$(.key)($($where ?? .value.?perl !! .value.WHAT.?perl ))");
                    @bits.push($_.value.^name) if $!;
                }
            }
        }
        else {
            @bits.push('...');
        }
        if @cand[0] ~~ /': '/ {
            my $invocant = @bits.shift;
            my $first = @bits ?? @bits.shift !! '';
            @bits.unshift($invocant ~ ': ' ~ $first);
        }
        my $cap = '(' ~ @bits.join(", ") ~ ')';
        @priors = flat "Earlier failures:\n", @priors, "\nFinal error:\n " if @priors;
        @priors.join ~
        join "\n    ",
            "Cannot call $.dispatcher.name()$cap; none of these signatures match:",
            @cand;
    }
}

my class X::Caller::NotDynamic is Exception {
    has $.symbol;
    method message() {
        "Cannot access '$.symbol' through CALLER, because it is not declared as dynamic";
    }
}

my class X::Inheritance::NotComposed does X::MOP {
    # normally, we try very hard to capture the types
    # and not just their names. But in this case, both types
    # involved aren't composed yet, so they basically aren't
    # usable at all.
    has $.child-name;
    has $.parent-name;
    method message() {
        "'$.child-name' cannot inherit from '$.parent-name' because '$.parent-name' isn't composed yet"
            ~ ' (maybe it is stubbed)';
    }
}

my class X::PhaserExceptions is Exception {
    has @.exceptions;
    method message() {
        "Multiple exceptions were thrown by LEAVE/POST phasers"
    }
    multi method gist(X::PhaserExceptions:D:) {
        join "\n", flat
            "Multiple exceptions were thrown by LEAVE/POST phasers\n",
            @!exceptions>>.gist>>.indent(4)
    }
}

nqp::bindcurhllsym('P6EX', nqp::hash(
  'X::TypeCheck::Binding', 
  sub (Mu $got, Mu $expected, $symbol?) {
      X::TypeCheck::Binding.new(:$got, :$expected, :$symbol).throw;
  },
  'X::TypeCheck::Assignment',
  sub (Mu $symbol, Mu $got, Mu $expected) {
      X::TypeCheck::Assignment.new(:$symbol, :$got, :$expected).throw;
  },
  'X::TypeCheck::Return',
  sub (Mu $got, Mu $expected) {
      X::TypeCheck::Return.new(:$got, :$expected).throw;
  },
  'X::Assignment::RO',
  sub ($typename = "value") {
      X::Assignment::RO.new(:$typename).throw;
  },
  'X::ControlFlow::Return',
  sub () {
      X::ControlFlow::Return.new().throw;
  },
  'X::NoDispatcher',
  sub ($redispatcher) {
      X::NoDispatcher.new(:$redispatcher).throw;
  },
  'X::Multi::Ambiguous',
  sub ($dispatcher, @ambiguous, $capture) {
      X::Multi::Ambiguous.new(:$dispatcher, :@ambiguous, :$capture).throw
  },
  'X::Multi::NoMatch',
  sub ($dispatcher, $capture) {
      X::Multi::NoMatch.new(:$dispatcher, :$capture).throw
  },
  'X::Role::Initialization',
  sub ($role) {
      X::Role::Initialization.new(:$role).throw
  },
  'X::Role::Parametric::NoSuchCandidate',
  sub (Mu $role) {
      X::Role::Parametric::NoSuchCandidate.new(:$role).throw;
  },
  'X::Inheritance::NotComposed',
  sub ($child-name, $parent-name) {
      X::Inheritance::NotComposed.new(:$child-name, :$parent-name).throw;
  },
  'X::Parameter::RW',
  sub (Mu $got, $symbol) {
      X::Parameter::RW.new(:$got, :$symbol).throw;
  },
  'X::PhaserExceptions',
  sub (@exceptions) {
      X::PhaserExceptions.new(exceptions =>
        @exceptions.map(-> Mu \e { EXCEPTION(e) })).throw;
  },
));

my class X::HyperWhatever::Multiple is Exception {
    method message() {
        "Multiple HyperWhatevers and Whatevers may not be used together"
    }
}

my class X::EXPORTHOW::InvalidDirective does X::Comp {
    has $.directive;
    method message() {
        "Unknown EXPORTHOW directive '$.directive' encountered during import"
    }
}

my class X::EXPORTHOW::NothingToSupersede does X::Comp {
    has $.declarator;
    method message() {
        "There is no package declarator '$.declarator' to supersede"
    }
}

my class X::EXPORTHOW::Conflict does X::Comp {
    has $.declarator;
    has $.directive;
    method message() {
        "'EXPORTHOW::{$.directive}::{$.declarator}' conflicts with an existing meta-object imported into this lexical scope"
    }
}

my class X::UnitScope::Invalid does X::Syntax {
    has $.what;
    has $.where;
    method message() {
        "A unit-scoped $.what definition is not allowed $.where;\n"
        ~ "Please use the block form."
    }
}

my class X::UnitScope::TooLate does X::Syntax {
    has $.what;
    method message() {
        "Too late for unit-scoped $.what definition;\n"
        ~ "Please use the block form."
    }
}

my class X::StubCode is Exception {
    has $.message = 'Stub code executed';
}

my class X::TooLateForREPR is X::Comp  {
    has $.type;
    method message() {
        "Cannot change REPR of $!type.^name() now (must be set at initial declaration)";
    }
}

my class X::NotParametric is Exception {
    has $.type;
    method message() {
        "$!type.^name() cannot be parameterized";
    }
}

my class X::InvalidType does X::Comp {
    has $.typename;
    has @.suggestions;
    method message() {
        my $msg := "Invalid typename '$.typename'";
        if +@.suggestions > 0 {
            $msg := $msg ~ ". Did you mean '" ~ @.suggestions.join("', '") ~ "'?";
        }
        $msg;
    }
}

my class X::InvalidTypeSmiley does X::Comp {
    has $.name;
    method message() {
        "Invalid type smiley '$.name' used in type name";
    }
}

my class X::Seq::Consumed is Exception {
    method message() {
        "This Seq has already been iterated, and its values consumed\n" ~
        "(you might solve this by adding .cache on usages of the Seq, or\n" ~
        "by assigning the Seq into an array)"
    }
}

my class X::Seq::NotIndexable is Exception {
    method message() {
        "Cannot index a Seq; coerce it to a list or assign it to an array first"
    }
}

my class X::WheneverOutOfScope is Exception {
    method message() {
        "Cannot have a 'whenever' block outside the scope of a 'supply' block"
    }
}

my class X::IllegalOnFixedDimensionArray is Exception {
    has $.operation;
    method message() {
        "Cannot $.operation a fixed-dimension array"
    }
}

my class X::NotEnoughDimensions is Exception {
    has $.operation;
    has $.got-dimensions;
    has $.needed-dimensions;
    method message() {
        "Cannot $.operation a $.needed-dimensions dimension array with only $.got-dimensions dimensions"
    }
}

my class X::TooManyDimensions is Exception {
    has $.operation;
    has $.got-dimensions;
    has $.needed-dimensions;
    method message() {
        "Cannot $.operation a $.needed-dimensions dimension array with $.got-dimensions dimensions"
    }
}

my class X::Assignment::ArrayShapeMismatch is Exception {
    has $.target-shape;
    has $.source-shape;
    method message() {
        "Cannot assign an array of shape $.source-shape to an array of shape $.target-shape"
    }
}

my class X::Assignment::ToShaped is Exception {
    has $.shape;
    method message() {
        "Assignment to array with shape $.shape must provide structured data"
    }
}

my class X::Language::Unsupported is Exception {
    has $.version;
    method message() {
        "No compiler available for Perl $.version"
    }
}

my class X::Proc::Unsuccessful is Exception {
    has $.proc;
    method message() {
        "The spawned process exited unsuccessfully (exit code: $.proc.exitcode())"
    }
}

class CompUnit::DependencySpecification { ... }
my class X::CompUnit::UnsatisfiedDependency is Exception {
    has CompUnit::DependencySpecification $.specification;

    my sub is-core($name) {
        my @parts = $name.split("::");
        my $ns := ::CORE.WHO;
        for @parts {
            return False unless $ns{$_}:exists;
            $ns := $ns{$_}.WHO;
        };
        True
    };

    method message() {
        my $name = $.specification.short-name;
        is-core($name)
            ?? "{$name} is a builtin type. You can use it without loading a module."
            !! "Could not find $.specification in:\n" ~ $*REPO.repo-chain.map(*.Str).join("\n").indent(4)
    }
}

# vim: ft=perl6 expandtab sw=4
