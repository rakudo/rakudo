my role X::Comp { ... }
my class X::ControlFlow { ... }
my role X::Control { ... }

my class Exception {
    has $!ex;
    has $!bt;

    method backtrace(Exception:D:) {
        $!bt
          ?? $!bt
          !! nqp::isconcrete($!ex)
            ?? ($!bt := Backtrace.new($!ex))
            !! Nil
    }

    # Only valid if .backtrace has not been called yet
    method vault-backtrace(Exception:D:) {
        nqp::isconcrete($!ex) && $!bt ?? Backtrace.new($!ex) !! Nil
    }
    method reset-backtrace(Exception:D: --> Nil) {
        $!ex := Nil
    }

    multi method Str(Exception:D:) {
        my $str;
        if nqp::isconcrete($!ex) {
            my str $message = nqp::getmessage($!ex);
            $str = nqp::isnull_s($message) ?? '' !! nqp::p6box_s($message);
        }
        $str ||= (try self.message);
        $str = ~$str if defined $str;
        $str // "Something went wrong in {self.WHAT.gist}";
    }

    multi method gist(Exception:D:) {
        my $str;
        if nqp::isconcrete($!ex) {
            my str $message = nqp::getmessage($!ex);
            $str = nqp::isnull_s($message)
                ?? (try self.message) // "Died with {self.^name}"
                !! nqp::p6box_s($message);
            $str ~= "\n";
            try $str ~= self.backtrace
              || Backtrace.new()
              || '  (no backtrace available)';
        }
        else {
            $str = (try self.message) // "Unthrown {self.^name} with no message";
        }
        $str;
    }

    method throw(Exception:D: $bt?) {
        unless nqp::isconcrete($!ex) and $bt {
            my $orig-ex := $!ex;
            $!ex := nqp::newexception();
            self!maybe-set-control() unless nqp::isconcrete($orig-ex);
        }
        $!bt := $bt; # Even if !$bt
        nqp::setpayload($!ex, self);
        nqp::throw($!ex)
    }
    method rethrow(Exception:D:) {
        unless nqp::isconcrete($!ex) {
            $!ex := nqp::newexception();
            try nqp::setmessage($!ex, self.message);
            self!maybe-set-control();
        }
        nqp::setpayload($!ex, self);
        nqp::rethrow($!ex)
    }

    method !maybe-set-control(--> Nil) {
        if nqp::istype(self, X::Control) {
            nqp::setextype($!ex, nqp::const::CONTROL_ANY);
        }
    }

    method resume(Exception:D: --> True) {
        nqp::resume($!ex);
    }

    method die(Exception:D:) { self.throw }
    method fail(Exception:D:) {
        try self.throw;
        my $fail := Failure.new($!);
        nqp::throwpayloadlexcaller(nqp::const::CONTROL_RETURN, $fail);
        CATCH { $fail.exception.throw }
    }

    method is-compile-time(--> False) { }
    method message() { ... }
}

my class X::SecurityPolicy is Exception {}

my class X::SecurityPolicy::Eval is X::SecurityPolicy {
    has $.payload = "EVAL is a very dangerous function!!!";

    my role SlurpySentry { }

    method message() {
        (($.payload ~~ SlurpySentry
          ?? $.payload.list.join # Remove spaces die(*@msg)/fail(*@msg) forms
          !! $.payload.Str
         ) ~ " (use the MONKEY-SEE-NO-EVAL pragma to override this error but only if you're VERY sure your data contains no injection attacks)."
        ).naive-word-wrapper
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
    has Mu $.invocant;
    has $.method;
    has $.typename;
    has Bool $.private;
    has $.addendum;
    has @!suggestions;
    has @!tips;
    has $!message;

    # This attribute is an implementation detail. Not to be documented.
    has $.in-class-call;

    method of-type() {
        nqp::eqaddr(nqp::decont($!invocant),IterationEnd)
          ?? "IterationEnd"
          !! "of type '$.typename'"
    }

    method message() { $!message // self!create-message }
    method suggestions() {
        self!create-message unless $!message;
        @!suggestions
    }
    method tips() {
        self!create-message unless $!message;
        @!tips
    }

    method !create-message() {
        my @message = $.private
          ?? "No such private method '!$.method' for invocant $.of-type"
          !! "No such method '$.method' for invocant $.of-type";

        @message.push: $.addendum if $.addendum;

        my $indirect-method = $.method.starts-with("!")
                                ?? $.method.substr(1)
                                !! "";

        my %suggestions;
        my int $max_length = do given $.method.chars {
            when 0..3 { 1 }
            when 4..8 { 2 }
            when 9..* { 3 }
        }

        if $.method eq 'length' {
            given $!invocant {
                when List { %suggestions{$_} = 0 for <elems> }
                when Cool { %suggestions{$_} = 0 for <chars codes>; }
                default   { %suggestions{$_} = 0 for <elems chars codes>; }
            }

        }
        elsif $.method eq 'bytes' {
            %suggestions<encode($encoding).bytes> = 0;
        }

        my sub code-name(Mu $meth) {
            # KnowHOW `methods` method returns a hash. Respectively, iteration over .^methods gives us Pairs.
            return $meth.key if $meth ~~ Pair;
            my $code-obj := nqp::decont($meth);
            (try nqp::can($code-obj,'name') ?? $code-obj.name !! nqp::getcodename($code-obj)) // '?'
        }

        my $public_suggested = 0;
        sub find_public_suggestion($before, $after --> Nil) {
            if $before.fc eq $after.fc {
                $public_suggested = 1;
                %suggestions{$after} = 0;  # assume identity
            }
            else {
                my $dist := StrDistance.new(
                  before => $before.fc,
                  after  => $after.fc
                );
                if $dist <= $max_length {
                    $public_suggested = 1;
                    %suggestions{$after} = $dist;
                }
            }
        }

        if nqp::can($!invocant.HOW, 'methods') {
            # $!invocant can be a KnowHOW which method `methods` returns a hash, not a list.
            my $invocant_methods :=
              Set.new: $!invocant.^methods(:local).map: { code-name($_) };

            for $!invocant.^methods(:all) -> $method_candidate {
                my $method_name := code-name($method_candidate);
                # GH#1758 do not suggest a submethod from a parent
                next
                  if $method_candidate.^name eq 'Submethod'  # a submethod
                  && !$invocant_methods{$method_name};       # unknown method

                find_public_suggestion($.method, $method_name);
            }

            # handle special unintrospectable cases
            for <HOW WHAT WHO> -> $method_name {
                find_public_suggestion($.method, $method_name);
            }
        }

        my $private_suggested = 0;
        if $.in-class-call && nqp::can($!invocant.HOW, 'private_method_table') {
            for $!invocant.^private_method_table.keys -> $method_name {
                my $dist = StrDistance.new(
                  before => $.method.fc,
                  after  => $method_name.fc
                );
                if $dist <= $max_length {
                    $private_suggested = 1;
                    %suggestions{"!$method_name"} = $dist
                        unless $indirect-method eq $method_name;
                }
            }
        }

        if $indirect-method && !$.private && $private_suggested {
            @!tips.push: "Method name starts with '!', did you mean 'self!\"$indirect-method\"()'?";
        }

        @!suggestions = %suggestions.sort(*.value).map(*.key).head(4);

        if @!suggestions == 1 {
            @!tips.push: "Did you mean '@!suggestions[0]'?";
        }
        elsif @!suggestions {
            @!tips.push: "Did you mean any of these: { @!suggestions.map( { "'$_'" } ).join(", ") }?";
        }

        if !$indirect-method
           &&($private_suggested ^^ $public_suggested)
           && ($private_suggested ^^ $.private)
        {
            @!tips.push: "Perhaps a " ~ ($private_suggested ?? "private" !! "public") ~ " method call must be used."
        }

        if @!tips > 1 {
            @!tips = @!tips.map: "\n" ~ ("- " ~ *).naive-word-wrapper(:indent("    "));
            @message.push: ($.addendum ?? "Other possible" !! "Possible") ~ " causes are:";
        }
        elsif @!tips {
            @message.push: @!tips.shift;
        }

        @message[0] ~= "." if @message > 1;

        $!message = @message.join(" ").naive-word-wrapper ~ @!tips.join
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
    has $.hint;
    method message {
        "No appropriate parametric role variant available for '"
        ~ $.role.^name ~ "'"
        ~ ($.hint ?? ":\n" ~ (~$.hint).indent(4) !! "")
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
    method message { "Unknown argument '{$.arg.raku}' specified with the '$.name' pragma." }
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
my class CX::Return does X::Control {
    method message() { "<return control exception>" }
}
my class CX::Emit does X::Control {
    method message() { "<emit control exception>" }
}
my class CX::Done does X::Control {
    method message() { "<done control exception>" }
}

sub EXCEPTION(|) is implementation-detail {
    my Mu $vm_ex   := nqp::shift(nqp::p6argvmarray());
    my Mu $payload := nqp::getpayload($vm_ex);
    if nqp::istype($payload, Exception) {
        nqp::bindattr($payload, Exception, '$!ex', $vm_ex);
        $payload;
    } else {
        my int $type = nqp::getextype($vm_ex);
        my $ex;
        if $type +& nqp::const::CONTROL_NEXT {
            $ex := CX::Next.new();
        }
        elsif $type +& nqp::const::CONTROL_REDO {
            $ex := CX::Redo.new();
        }
        elsif $type +& nqp::const::CONTROL_LAST {
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
        elsif $type == nqp::const::CONTROL_RETURN {
            $ex := CX::Return.new();
        }
        elsif $type == nqp::const::CONTROL_EMIT {
            $ex := CX::Emit.new();
        }
        elsif $type == nqp::const::CONTROL_DONE {
            $ex := CX::Done.new();
        }
#?if !moar
        # for MoarVM this check is done in src/Perl6/Metamodel/BOOTSTRAP.nqp, cmp 222d16b0b9
        elsif !nqp::isnull_s(nqp::getmessage($vm_ex)) &&
                nqp::p6box_s(nqp::getmessage($vm_ex)) ~~ /"Method '" (.*?) "' not found for invocant of class '" (.+)\'$/ {
            $ex := X::Method::NotFound.new(
                method   => ~$0,
                typename => ~$1,
            );
        }
#?endif
        else {
            $ex := nqp::create(X::AdHoc);
            nqp::bindattr($ex, X::AdHoc, '$!payload', nqp::p6box_s(nqp::getmessage($vm_ex) // 'unknown exception'));
        }
        nqp::bindattr($ex, Exception, '$!ex', $vm_ex);
        $ex;
    }
}

my class X::Comp::AdHoc { ... }
sub COMP_EXCEPTION(|) is implementation-detail {
    my Mu $vm_ex   := nqp::shift(nqp::p6argvmarray());
    my Mu $payload := nqp::getpayload($vm_ex);
    if nqp::istype($payload, Exception) {
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
        my $e := EXCEPTION($ex);

        if %*ENV<PERL6_EXCEPTIONS_HANDLER> -> $handler {
            my $class := ::("Exceptions::$handler");
            unless nqp::istype($class,Failure) {
                temp %*ENV<PERL6_EXCEPTIONS_HANDLER> = ""; # prevent looping
                unless $class.process($e) {
                    nqp::getcurhllsym('&THE_END')();
                    return
                }
            }
        }
        if %*ENV<RAKU_EXCEPTIONS_HANDLER> -> $handler {
            my $class := ::("Exceptions::$handler");
            unless nqp::istype($class,Failure) {
                temp %*ENV<RAKU_EXCEPTIONS_HANDLER> = ""; # prevent looping
                unless $class.process($e) {
                    nqp::getcurhllsym('&THE_END')();
                    return
                }
            }
        }

        try {
            my $v := $e.vault-backtrace;
            my Mu $err := $*ERR;

            $e.backtrace;  # This is where most backtraces actually happen
            if $e.is-compile-time || $e.backtrace && $e.backtrace.is-runtime {
                $err.say($e.gist);
                if $v and !$e.gist.ends-with($v.Str) {
                    $err.say("Actually thrown at:");
                    $err.say($v.Str);
                }
            }
            elsif Rakudo::Internals.VERBATIM-EXCEPTION(0) {
                $err.print($e.Str);
            }
            else {
                $err.say("===SORRY!===");
                $err.say($e.Str);
            }
            nqp::getcurhllsym('&THE_END')();
            CONTROL { when CX::Warn { .resume } }
        }
        if $! {
            nqp::rethrow(nqp::getattr(nqp::decont($!), Exception, '$!ex'));
            $ex
        }
    }

    sub print_control(|) {
        my Mu $ex := nqp::atpos(nqp::p6argvmarray(),0);
        my int $type = nqp::getextype($ex);
        my $backtrace = Backtrace.new(nqp::backtrace($ex));

        nqp::if(
          nqp::iseq_i($type,nqp::const::CONTROL_WARN),
          nqp::stmts(
            (my Mu $err := $*ERR),
            (my str $msg = nqp::getmessage($ex)),
            $err.say(nqp::if(nqp::chars($msg),$msg,"Warning")),
            $err.print($backtrace.first-none-setting-line),
            nqp::resume($ex)
          )
        );

        my $label = $type +& nqp::const::CONTROL_LABELED ?? "labeled " !! "";
        if $type +& nqp::const::CONTROL_LAST {
            X::ControlFlow.new(illegal => "{$label}last", enclosing => 'loop construct', :$backtrace).throw;
        }
        elsif $type +& nqp::const::CONTROL_NEXT {
            X::ControlFlow.new(illegal => "{$label}next", enclosing => 'loop construct', :$backtrace).throw;
        }
        elsif $type +& nqp::const::CONTROL_REDO {
            X::ControlFlow.new(illegal => "{$label}redo", enclosing => 'loop construct', :$backtrace).throw;
        }
        elsif $type +& nqp::const::CONTROL_PROCEED {
            X::ControlFlow.new(illegal => 'proceed', enclosing => 'when clause', :$backtrace).throw;
        }
        elsif $type +& nqp::const::CONTROL_SUCCEED {
            # XXX: should work like leave() ?
            X::ControlFlow.new(illegal => 'succeed', enclosing => 'when clause', :$backtrace).throw;
        }
        elsif $type +& nqp::const::CONTROL_TAKE {
            X::ControlFlow.new(illegal => 'take', enclosing => 'gather', :$backtrace).throw;
        }
        elsif $type +& nqp::const::CONTROL_EMIT {
            X::ControlFlow.new(illegal => 'emit', enclosing => 'supply or react', :$backtrace).throw;
        }
        elsif $type +& nqp::const::CONTROL_DONE {
            X::ControlFlow.new(illegal => 'done', enclosing => 'supply or react', :$backtrace).throw;
        }
        else {
            X::ControlFlow.new(illegal => 'control exception', enclosing => 'handler', :$backtrace).throw;
        }
    }

    my Mu $comp := nqp::getcomp('Raku');
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

my class X::IO::Lock does X::IO {
    has $.lock-type;
    method message() { "Could not obtain $.lock-type lock: $.os-error" }
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

my class X::IO::Null does X::IO {
    method message() {
        "Cannot use null character (U+0000) as part of the path"
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

my class X::IO::Flush does X::IO {
    method message() {
        "Cannot flush handle: $.os-error"
    }
}

my class X::IO::NotAChild does X::IO {
    has $.path;
    has $.child;
    method message() {
      "Path {$.child.raku} is not a child of path {$.path.raku}"
    }
}

my class X::IO::Resolve does X::IO {
    has $.path;
    method message() { "Failed to completely resolve {$.path.raku}" }
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

my class X::IO::BinaryAndEncoding does X::IO {
    method message { "Cannot open a handle in binary mode (:bin) and also specify an encoding" }
}

my class X::IO::BinaryMode does X::IO {
    has $.trying;
    method message { "Cannot do '$.trying' on a handle in binary mode" }
}

my class X::IO::Closed does X::IO {
    has $.trying;
    method message { "Cannot do '$.trying' on a closed handle" }
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
            $r ~= $.filename eq '<unknown file>'
              ?? $.line == 1
                ?? $.message
                !! "$.message\nat line $.line"
              !! "$.message\nat $.filename():$.line";
            $r ~= "\n------> $green$.pre$yellow$eject$red$.post$clear" if defined $.pre;
            if $expect && @.highexpect {
                $r ~= "\n    expecting any of:";
                for flat @.highexpect».list {
                    $r ~= "\n        $_";
                }
            }
            for @.modules.reverse.skip {
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
        "$red==={$clear}SORRY!$red===$clear Error while compiling{
          $.filename eq '<unknown file>'
            ?? ':'
            !! " $.filename"
        }\n"
    }
    method SET_FILE_LINE($file, $line) is implementation-detail {
        $!filename = $file;
        $!line     = $line;
        $!is-compile-time = True;
    }
}

my class X::Comp::Group is Exception {
    has $.panic;
    has @.sorrows;
    has @.worries;

    method is-compile-time(--> True) { }

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
    has str $.use-case;
    has $.exception;

    method message() {
        $!exception ~~ X::MOP
            ?? $!exception.message
            !! "An exception occurred while $!use-case"
    }

    multi method gist(::?CLASS:D: :$sorry = True) {
        my $r = $sorry ?? self.sorry_heading() !! "";
        $r ~= "$.message\nat $.filename():$.line";
        for @.modules.reverse.skip {
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

my class X::Coerce is Exception {
    has Mu $.target-type is built(:bind);
    has Mu $.from-type is built(:bind);
    method message() {
        "from '" ~ $!from-type.^name ~ "' into '" ~ $!target-type.^name ~ "'"
    }
}

my class X::Coerce::Impossible is X::Coerce {
    has $.hint is required;
    method message() {
        "Impossible coercion " ~ callsame() ~ ": " ~ $!hint
    }
}

# XXX a hack for getting line numbers from exceptions from the metamodel
my class X::Comp::AdHoc is X::AdHoc does X::Comp {
    method is-compile-time(--> True) { }
}

my class X::Comp::FailGoal does X::Comp {
    has $.dba;
    has $.goal;
    has $.line-real;

    method is-compile-time(--> True) { }

    method message { "Unable to parse expression in $.dba; couldn't find final $.goal"
                     ~ " (corresponding starter was at line $.line-real)" }
}

my role X::Syntax does X::Comp { }
my role X::Pod                 { }

my class X::NYI is Exception {
    has $.feature;
    has $.did-you-mean;
    has $.workaround;
    method message() {
        my $msg = "{ $.feature andthen "$_ not" orelse "Not" } yet implemented. Sorry.";
        $msg ~= "\nDid you mean: {$.did-you-mean.gist}?" if $.did-you-mean;
        $msg ~= "\nWorkaround: $.workaround" if $.workaround;
        $msg
    }
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
q/To pass an array, hash or sub to a function in Raku, just pass it as is.
For other uses of Perl's ref operator consider binding with ::= instead.
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
        'Leading 0 has no meaning. If you meant to create an octal number'
        ~ ", use '0o' prefix" ~ (
#?if jvm
            $!value ~~ /<[89]>/
#?endif
#?if !jvm
            $!value.comb.grep(*.unival > 7)
#?endif
                ?? ", but note that $!value is not a valid octal number"
                !! "; like, '0o$!value'"
        ) ~ '. If you meant to create a string, please add quotation marks.'
    }
}
my class X::Worry::Precedence::Range is X::Worry {
    has $.action;
    method message {
"To $!action a range, parenthesize the whole range.
(Or parenthesize the whole endpoint expression, if you meant that.)"
    }
}

my class X::Trait::Invalid is Exception {
    has $.type;       # is, will, of etc.
    has $.subtype;    # wrong subtype being tried
    has $.declaring;  # variable, sub, parameter, etc.
    has $.name;       # '$foo', '@bar', etc.
    method message () {
        "Cannot use '$.type $.subtype' on $.declaring '$.name'."
    }
}

my class X::Trait::Unknown is Exception {
    has $.type;       # is, will, of etc.
    has $.subtype;    # wrong subtype being tried
    has $.declaring;  # variable, sub, parameter, etc.
    method message () {
        "Can't use unknown trait '{
            try { $.type } // "unknown type"
        }' -> '{
            try { $.subtype } // "unknown subtype"
        }' in a$.declaring declaration."
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

my class X::Exhausted is Exception {
    has $.what;
    has $.reason;
    method message {
        $.reason
          ?? "Could not create another $.what because of: $.reason"
          !! "Could not create another $.what"
    }
}

my class X::OutOfRange is Exception {
    has $.what = 'Argument';
    has $.got = '<unknown>';
    has $.range = '<unknown>';
    has $.comment;
    method message() {
        my $result = $.comment.defined
           ?? "$.what out of range. Is: $.got.gist(), should be in $.range.gist(); $.comment"
           !! "$.what out of range. Is: $.got.gist(), should be in $.range.gist()";
        $result;
    }
}

my class X::Buf::AsStr is Exception {
    has $.object;
    has $.method;
    method message() {
        my $message = $.method.starts-with('Str')
          ?? "Stringification of a {$.object.^name} is not done with '$.method'"
          !! "A {$.object.^name} is not a Str, so using '$.method' will not work";
        ($message ~ ". The 'decode' method should be used to convert a {$.object.^name} to a Str."
        ).naive-word-wrapper
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
        "Placeholder variable '$.placeholder' may not be used here because the surrounding block does not take a signature.".naive-word-wrapper;
    }
}

my class X::Placeholder::NonPlaceholder does X::Comp {
    has $.variable_name;
    has $.placeholder;
    has $.decl;
    method message() {
        my $decl = $!decl ?? $!decl !! 'block';
        "'$!variable_name' has already been used as a non-placeholder in the surrounding $decl, so you will confuse the reader if you suddenly declare $!placeholder here.".naive-word-wrapper
    }
}

my class X::Placeholder::Mainline is X::Placeholder::Block {
    method message() {
        "Cannot use placeholder parameter $.placeholder outside of a sub or block"
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
            $message := "$message. Did you mean any of these: { @.suggestions.map( { "'$_'" } ).join(", ") }?";
        }
        $message.naive-word-wrapper
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
        "Attribute '$.symbol' not available inside of a regex, since regexes are methods on the Cursor class. Consider storing the attribute in a lexical, and using that in the regex.".naive-word-wrapper
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
                use => '"v" prefix for pragma (e.g., "use v6;", "use v6.c;")',
                need => '"v" prefix and "use" for pragma (e.g., "use v6;", "use v6.c;")',
            }
            $r ~= "Undeclared routine" ~ (%.unk_routines.elems == 1 ?? "" !! "s") ~ ":\n";
            for %.unk_routines.sort(*.key) {
                $r ~= "    $_.key() &l($_.value)";
                $r ~= " (in Raku please use " ~ $obs{$_.key()} ~ " instead)" if $obs{$_.key()};
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
        ("Redeclaration of $.what '$.symbol'"
          ~ (" $.postfix" if $.postfix)
          ~ ($.what eq 'routine'
              ?? ". Did you mean to declare a multi-sub?"
              !! ".")
        ).naive-word-wrapper
    }
}

my class X::Redeclaration::Outer does X::Comp {
    has $.symbol;
    method message() {
        "Lexical symbol '$.symbol' is already bound to an outer symbol.  The implicit outer binding must be rewritten as 'OUTER::<$.symbol>' before you can unambiguously declare a new '$.symbol' in this scope.".naive-word-wrapper
    }
}

my class X::Dynamic::Postdeclaration does X::Comp {
    has $.symbol;
    method message() {
        "Illegal post-declaration of dynamic variable '$.symbol'. Earlier access must be written as 'CALLERS::<$.symbol>' if that's what you meant.".naive-word-wrapper
    }
}

my class X::Dynamic::Package does X::Comp {
    has $.symbol;
    method message() {
        "Dynamic variables cannot have package-like names (with '::'), so '$!symbol' is not allowed.".naive-word-wrapper
    }
}

my class X::Import::Redeclaration does X::Comp {
    has @.symbols;
    has $.source-package-name;
    method message() {
        (@.symbols == 1
           ?? "Cannot import symbol '@.symbols[0]' from '$.source-package-name', because it already exists in this lexical scope."
           !! "Cannot import the following symbols from '$.source-package-name', because they already exist in this lexical scope: { @.symbols.map( { "'$_'" } ).join(', ')}."
        ).naive-word-wrapper
    }
}

my class X::Import::OnlystarProto does X::Comp {
    has @.symbols;
    has $.source-package-name;
    method message() {
        (@.symbols == 1
           ?? "Cannot import symbol '@.symbols[0]' from '$.source-package-name', because only onlystar-protos ('proto foo(|) {*}') can be merged."
           !! "Cannot import the following symbols from '$.source-package-name', only onlystar-protos ('proto foo(|) {*}') can be merged: { @.symbols.map( { "'$_'" } ).join(', ')}."
        ).naive-word-wrapper
    }
}

my class X::PoisonedAlias does X::Comp {
    has str $.alias;
    has str $.package-type = 'package';
    has str $.package-name;
    method message() {
        ("Cannot directly use poisoned alias '$.alias' because it was declared by several {$.package-type}s." ~
         ($.package-name
           ?? " Please access it via explicit package name like: '{$.package-name}::{$!alias}'"
           !! '')
        ).naive-word-wrapper
    }
}

my class X::Phaser::Multiple does X::Comp {
    has $.block;
    method message() { "Only one $.block block is allowed" }
}

my class X::Obsolete does X::Comp {
    has $.old;
    has $.replacement; # can't call it $.new, collides with constructor
    has $.when = 'in Raku';
    method message() {
        "Unsupported use of $.old. $.when.tc() please use: $.replacement.".naive-word-wrapper
    }
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
    has $.what = 'parameter';
    has $.got is default(Nil);
    has $.expected is default(Nil);
    method message() {
        "Default value '{Rakudo::Internals.MAYBE-STRING: $!got}' will never bind to a parameter of type {$!expected.^name}"
    }
}

my class X::Parameter::AfterDefault does X::Syntax {
    has $.type;
    has $.modifier;
    has $.default;
    method message() {
        "The $.type '$.modifier' came after the default value. Did you mean: ...$.modifier $.default?".naive-word-wrapper
    }
}

my class X::Parameter::Placeholder does X::Comp {
    has $.type;
    has $.parameter;
    has $.right;
    method message() {
        "$.type.tc() placeholder variables like '$.parameter' are not allowed in signatures.  Did you mean: '$.right' ?".naive-word-wrapper
    }
}

my class X::Parameter::Twigil does X::Comp {
    has $.parameter;
    has $.twigil;
    method message() {
        "Parameters with a '$.twigil' twigil, like '$.parameter', are not allowed in signatures.".naive-word-wrapper
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
        "$what '$!type.^name()' is insufficiently type-like to qualify a parameter.  Did you mean 'class'?".naive-word-wrapper
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

my class X::Parameter::InvalidConcreteness is Exception {
    has $.expected;
    has $.got;
    has $.routine;
    has $.param;
    has Bool $.should-be-concrete;
    has Bool $.param-is-invocant;

    method message() {
        $!routine = '<anon>' if not $!routine.defined or $!routine eq '';
        $!param   = '<anon>' if not $!param.defined   or $!param   eq '';
        my $beginning  = $!param-is-invocant  ?? 'Invocant of method' !! "Parameter '$!param' of routine";
        my $must-be    = $!should-be-concrete ?? 'an object instance' !! 'a type object';
        my $not-a      = $!should-be-concrete ?? 'a type object'      !! 'an object instance';
        my $suggestion = $!should-be-concrete ?? '.new'               !! 'multi';

        "$beginning '$!routine' must be $must-be of type '$!expected', not $not-a of type '$!got'.  Did you forget a '$suggestion'?".naive-word-wrapper
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
        $msg.naive-word-wrapper
    }
}

my class X::Parameter::RW is Exception {
    has $.got;
    has $.symbol;
    method message() {
        "Parameter '$.symbol' expects a writable container (variable) as an argument, but got '{Rakudo::Internals.MAYBE-STRING: $.got, method => 'gist'}' ($.got.^name()) as a value without a container.".naive-word-wrapper
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
        "Cannot call private method '$.method' on package '$.source-package' because it does not trust the '$.calling-package' package.".naive-word-wrapper
    }
}

my class X::Method::Private::Unqualified does X::Comp {
    has $.method;
    method message() {
        "Calling private method '$.method' must be fully qualified with the package containing that private method.".naive-word-wrapper
    }
}

my class X::Adverb is Exception {
    has $.what   is rw;
    has $.source is rw;
    has @.unexpected;
    has @.nogo;
    method message {
        my $text = '';
        if @!unexpected.elems -> $elems {
            $text = $elems > 1
              ?? "$elems unexpected adverbs ('@.unexpected.join("', '")')"
              !! "Unexpected adverb '@!unexpected[0]'"
        }
        if @!nogo {
            $text ~= $text ?? " and u" !! "U";
            $text ~= "nsupported combination of adverbs ('@.nogo.join("', '")')";
        }
        ($text ~ " passed to $!what on '$!source'.").naive-word-wrapper
    }
    method unexpected { @!unexpected.sort }
    method nogo       { @!nogo.sort }
}

my class X::Delete is Exception {
    has $.target;
    method message() {
        $.target.defined
            ?? "Cannot delete from $.target"
            !! 'Cannot delete from this left-hand side'
    }
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

my class X::Invalid::ComputedValue is Exception {
    has $.method;
    has $.name;
    has $.value;
    has $.reason;
    method message {
        "$.name {"on $.method " if $.method}computed to $.value,"
            ~ " which cannot be used"
            ~ (" because $.reason" if $.reason);
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
    has $.keyword;
    method message() { qq|"unless" does not take "$!keyword", please rewrite using "if"| }
}

my class X::Syntax::WithoutElse does X::Syntax {
    has $.keyword;
    method message() { qq|"without" does not take "$!keyword", please rewrite using "with"| }
}

my class X::Syntax::KeywordAsFunction does X::Syntax {
    has $.word;
    has $.needparens;
    method message {
        ("The word '$.word' is interpreted as a '{$.word}()' function call.  Please use whitespace "
          ~ ($.needparens ?? 'around the' !! 'instead of')
          ~ " parentheses."
        ).naive-word-wrapper
    }
}

my class X::Syntax::ParentAsHash does X::Syntax {
    has $.type;
    has $.parent;
    has $.what;
    method message() {
        "Parent class specification is probably missing some whitespace. Found '$.type is $.parent\{ ...', which tries to specify a parent with a '$.what'.  You probably meant '$.type is $.parent \{ ...'.".naive-word-wrapper
    }
}

my class X::Syntax::Malformed::Elsif does X::Syntax {
    has $.what = 'else if';
    method message() { qq{In Raku, please use "elsif' instead of "$.what"} }
}

my class X::Syntax::Reserved does X::Syntax {
    has $.reserved;
    has $.instead = '';
    method message() { "The $.reserved is reserved$.instead" }
}

my class X::Syntax::P5 does X::Syntax {
    method message() { 'This appears to be Perl code' }
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
    has $.name;
    has $.twigil;
    has $.scope;
    has $.additional = '';
    method message() {
        "Cannot use a '$.twigil' twigil on a '$.scope $.name' $.what$.additional.".naive-word-wrapper
    }
}

my class X::Syntax::Variable::IndirectDeclaration does X::Syntax {
    method message() { 'Cannot declare a variable by indirect name (use a hash instead?)' }
}

my class X::Syntax::Variable::BadType does X::Comp {
    has Mu $.type;
    method message() {
        my $what = ~$!type.HOW.WHAT.^name.match(/ .* '::' <(.*)> HOW/) // 'Namespace';
        "$what '$!type.^name()' is insufficiently type-like to qualify a variable.  Did you mean 'class'?".naive-word-wrapper
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
    method message() { "Cannot use adverb $.adverb on a type name (only 'ver', 'auth' and 'api' are understood)" }
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

my class X::Syntax::Pod::DeclaratorLeading does X::Syntax  {
    method message() { "Opening bracket required for #| declarator block" }
}

my class X::Syntax::Pod::DeclaratorTrailing does X::Syntax {
    method message() { "Opening bracket required for #= declarator block" }
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
    has $.identifier-name;
#?if moar
    my constant $m = nqp::hash(
#?endif
#?if !moar
    my $m := nqp::hash(
#?endif
      '$"',    '.join() method',
      '$$',    '$*PID',
      '$;',    'real multidimensional hashes',
      '$&',    '$<>',
      '$`',    '$/.prematch',
      '$\'',   '$/.postmatch',
      '$,',    '.join() method',
      '$.',    "the .kv method on e.g. .lines",
      '$/',    "the filehandle's .nl-in attribute",
      '$\\',   "the filehandle's .nl-out attribute",
      '$|',    "the filehandle's .out-buffer attribute",
      '$?',    '$! for handling child errors also',
      '$@',    '$!',
      '$]',    '$*RAKU.version or $*RAKU.compiler.version',

      '$^C',   'COMPILING namespace',
      '$^H',   '$?FOO variables',
      '$^N',   '$/[*-1]',
      '$^O',   'VM.osname',
      '$^R',   'an explicit result variable',
      '$^S',   'context function',
      '$^T',   '$*INIT-INSTANT',
      '$^V',   '$*RAKU.version or $*RAKU.compiler.version',
      '$^X',   '$*EXECUTABLE-NAME',

      '@-',    '.from method',
      '@+',    '.to method',

      '%-',    '.from method',
      '%+',    '.to method',
      '%^H',   '$?FOO variables',
    );
    method message() {
        my $name = $!name;
        my $v    = $name ~~ m/ <[ $ @ % & ]> [ \^ <[ A..Z ]> | \W ] /;
        my $sugg = nqp::atkey($m,~$v);
        if $name eq '$#' {
            # Currently only `$#` var has this identifier business handling.
            # Should generalize the logic if we get more of stuff like this.
            $name ~= $!identifier-name;
            $sugg  = '@' ~ $!identifier-name ~ '.end';
        }
        $v
          ?? $sugg
            ?? "Unsupported use of $name variable; in Raku please use $sugg"
            !! "Unsupported use of $name variable"
          !! 'Weird unrecognized variable name: ' ~ $name;
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
    has $.native;

    method message() {
        my $vartype := $!vartype.WHAT.^name;
        my $conversionmethod := $vartype;
        $vartype := $vartype.lc if $.native;
        my $vt := $!value.^name;
        my $value := $vt eq "IntStr" || $vt eq "NumStr" || $vt eq "RatStr" || $vt eq "ComplexStr"
            ?? $!value.Str
            !! $!value.raku;
        my $val = "Cannot assign a literal of type {$.valuetype} ($value) to { $.native ?? "a native" !! "a" } variable of type $vartype. You can declare the variable to be of type $.suggestiontype, or try to coerce the value with { $value ~ '.' ~ $conversionmethod } or $conversionmethod\($value\)";
        try $val ~= ", or just write the value as " ~ $!value."$vartype"().raku;
        $val;
    }
}

my class X::Syntax::NonAssociative does X::Syntax {
    has $.left;
    has $.right;
    method message() {
        "Operators '$.left' and '$.right' are non-associative and require parentheses";
    }
}

my class X::Syntax::NonListAssociative is X::Syntax::NonAssociative {
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
    method message() {
        'Malformed Range. If attempting to use variables for end points, '
        ~ 'wrap the entire range in curly braces.'
    }
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

my class X::Syntax::Regex::QuantifierValue does X::Syntax {
    has $.inf;
    has $.non-numeric;
    has $.non-numeric-range;
    has $.empty-range;
    method message {
        $!inf
          && 'Minimum quantity to match for quantifier cannot be Inf.'
            ~ ' Did you mean to use + or * quantifiers instead of **?'
        || $!non-numeric-range
          && 'Cannot use Range with non-Numeric or NaN end points as quantifier'
        || $!non-numeric
          && 'Cannot use non-Numeric or NaN value as quantifier'
        || $!empty-range
          && 'Cannot use empty Range as quantifier'
        || 'Invalid quantifier value'
    }
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

my class X::Syntax::Regex::Alias::LongName does X::Syntax {
    method message() { "Can only alias to a short name (without '::')"; }
}

my class X::Syntax::Term::MissingInitializer does X::Syntax {
    method message { 'Term definition requires an initializer' }
}

my class X::Syntax::Variable::MissingInitializer does X::Syntax {
    has $.what;
    has $.type;
    has $.implicit;
    has $.maybe;
    method message {
        my $modality    = $.maybe ?? "may need" !! "needs";
        my $type        = $.implicit ?? "$.type (implicit $.implicit)" !! "$.type";
        my $requirement = $.what eq 'attribute'
                      ?? 'to be marked as required or given an initializer'
                      !! 'to be given an initializer';
        "$.what.tc() definition of type $type $modality $requirement"
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

my class X::Syntax::Coercer::TooComplex does X::Syntax {
    method message() {
        'Coercer is too complex. Only type objects, with optional type'
        ~ " smileys, or empty parentheses, implying 'Any', are supported."
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
        my $infix := $!infix.trim;
        "Preceding context expects a term, but found infix $infix instead."
        ~ (
            $.post && $.post.starts-with('end ')
                ?? "\nDid you forget '=begin $.post.substr(4)' Pod marker?"
                !! "\nDid you make a mistake in Pod syntax?"
            if $infix eq '='
        )
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
    has $.why;
    method message() {
        $.why && nqp::istype($.why,Str)
          ?? "The attribute '$.name' is required because $.why,\nbut you did not provide a value for it."
          !! "The attribute '$.name' is required, but you did not provide a value for it."
    }
}
my class X::Attribute::Scope::Package does X::Comp {
    has $.scope;
    has $.allowed;
    has $.disallowed;
    method message() { "Cannot use {$.scope}-scoped attribute in $.disallowed"
        ~ ($.allowed ?? ", only $.allowed." !! ".") }
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
    has $.found;
    has $.last;
    method message() {
        my $msg =
          "Odd number of elements found where hash initializer expected";
        if $.found == 1 {
            $msg ~= $.last
              ?? ":\nOnly saw: $.last.raku()"
              !! ":\nOnly saw 1 element"
        }
        else {
            $msg ~= ":\nFound $.found (implicit) elements";
            $msg ~= ":\nLast element seen: $.last.raku()" if $.last;
        }
    }
}

my class X::Pairup::OddNumber is Exception {
    method message() { "Odd number of elements found for .pairup()" }
}

my class X::Match::Bool is Exception {
    has $.type;
    method message() { "Cannot use Bool as Matcher with '" ~ $.type ~ "'.  Did you mean to use \$_ inside a block?" }
}

my class X::LibEmpty does X::Comp {
    method message { q/Repository specification can not be an empty string.  Did you mean 'use lib "."' ?/ }
}
my class X::LibNone does X::Comp {
    method message { q/Must specify at least one repository.  Did you mean 'use lib "lib"' ?/ }
}
my class X::Package::UseLib does X::Comp {
    has $.what;
    method message { "Cannot 'use lib' inside a $.what" }
}
my class X::Package::Stubbed does X::Comp {
    has @.packages;
    method message() {
        "The following packages were stubbed but not defined:\n    "
        ~ @.packages.join("\n    ");
    }

    # The unnamed named param is here so this candidate, rather than
    # the one from X::Comp is used. (is it a bug that this is needed?
    # No idea: https://colabti.org/irclogger/irclogger_log/perl6-dev?date=2017-09-14#l405
    multi method gist(::?CLASS:D: :$) {
        $.message;
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

my class X::Str::InvalidCharName is Exception {
    has $.name;
    method message() {
        $!name.chars ?? "Unrecognized character name [{$!name}]"
                     !! "Cannot use empty name as character name"
    }
}

my class X::Str::Numeric is Exception {
    has $.source;
    has $.pos;
    has $.reason;
    method source-indicator {
        my ($red,$clear,$green,$,$eject) = Rakudo::Internals.error-rcgye;
        my sub escape($str) { $str.raku.substr(1).chop }
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

my class X::Str::Subst::Adverb is Exception {
    has $.name;
    has $.got;
    method message() {
        "Cannot use :$.name adverb in Str.subst, got $.got"
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

my class X::Str::Sprintf::Directives::Count is Exception {
    has int $.args-used; # number of directives actually detected in the format string
    has int $.args-have; # number of args supplied
    method message() {
        my $msg = "Your printf-style directives specify ";

        if $.args-used == 1 {
            $msg ~= "1 argument, but ";
        }
        else {
            $msg ~= "$.args-used arguments, but ";
        }

        if $.args-have < 1 {
            $msg ~= "no argument was";
        }
        else {
            if $.args-have == 1 {
                $msg ~= "1 argument was";
            }
            else {
                # too many args
                $msg ~= "$.args-have arguments were";
            }
        }
        $msg ~= " supplied.";

        if $.args-used > $.args-have {
            $msg ~= "\nAre you using an interpolated '\$'?";
        }

        $msg;
    }
}

my class X::Str::Sprintf::Directives::Unsupported is Exception {
    has str $.directive;
    has str $.sequence;
    method message() {
        "Directive $.directive is not valid in sprintf format sequence $.sequence"
    }
}

my class X::Str::Sprintf::Directives::BadType is Exception {
    has str $.type;
    has str $.directive;
    has str $.expected;
    has $.value;
    method message() {
        $.expected
          ??  "Directive %$.directive expected a $.expected value, not a $.type ({Rakudo::Internals.SHORT-STRING: $.value[0]})"
          !! "Directive %$.directive not applicable for value of type $.type ({Rakudo::Internals.SHORT-STRING: $.value[0]})"
    }
}

my role X::Encoding is Exception { }

my class X::Encoding::Unknown does X::Encoding {
    has $.name;
    method message() {
        "Unknown string encoding '$.name'"
    }
}

my class X::Encoding::AlreadyRegistered does X::Encoding {
    has $.name;
    method message() {
        "An encoding with name '$.name' has already been registered"
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
        $!from ?? "Unable to deduce arithmetic or geometric sequence from: $!from\nDid you really mean '..'?"
               !! 'Unable to deduce sequence for some unfathomable reason'
    }
}

my class X::Sequence::Endpoint is Exception {
    has $.from;
    has $.endpoint;
    method message() {
        "Incompatible endpoint for sequence: "
          ~ $!from.raku
          ~ " ... "
          ~ $!endpoint.raku
    }
}

my class X::Cannot::Junction is Exception {
    has $.junction;
    has $.for;
    method message() {
        "Cannot use Junction '$.junction' $.for."
    }
}

my class X::Cannot::Map is Exception {
    has $.what   = "(<unknown type>)";
    has $.using  = "(<an unknown expression>)";
    has $.suggestion;
    method message() {
        my $message = "Cannot map a $.what using $.using";
        $.suggestion ?? "$message\n$.suggestion" !! $message
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
my class X::Cannot::Capture is Exception {
    has $.what;
    method message() {
        "Cannot unpack or Capture `$!what.gist()`.\n"
          ~ "To create a Capture, add parentheses: \\(...)\n"
          ~ 'If unpacking in a signature, perhaps you needlessly used'
          ~ ' parentheses? -> ($x) {} vs. -> $x {}'
          ~ "\nor missed `:` in signature unpacking? -> \&c:(Int) \{}";
    }
}

my class X::Backslash::UnrecognizedSequence does X::Syntax {
    has $.sequence;
    has $.suggestion;
    method message() {
        "Unrecognized backslash sequence: '\\$.sequence'"
        ~ (nqp::defined($!suggestion) ?? ". Did you mean $!suggestion?" !! '')
    }
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
    has Bool $.out-of-dynamic-scope;
    submethod BUILD(Bool() :$!out-of-dynamic-scope) {}

    method illegal()   { 'return'  }
    method enclosing() { 'Routine' }
    method message()   {
        'Attempt to return outside of ' ~ (
            $!out-of-dynamic-scope
              ?? 'immediately-enclosing Routine (i.e. `return` execution is'
               ~ ' outside the dynamic scope of the Routine where `return` was used)'
              !! 'any Routine'
        )
    }
}

my class X::Composition::NotComposable does X::Comp {
    has $.target-name;
    has $.composer;
    method message() {
        $!composer.^name ~ " is not composable, so $!target-name cannot compose it";
    }
}

my class X::ParametricConstant is Exception {
    method message { 'Parameterization of constants is forbidden' }
}

my class X::TypeCheck is Exception {
    has $.operation;
    has $!got      is built(:bind) is default(Nil);
    has $!expected is built(:bind) is default(Nil);
    method got()      { $!got }
    method expected() { $!expected }
    method gotn() {
        my Str:D $raku := Rakudo::Internals.SHORT-STRING: $!got, :method<raku>;
        nqp::eqaddr($!got.WHAT, $!expected.WHAT)
          ?? $raku
          !! nqp::can($!got.HOW, 'name')
            ?? "$!got.^name() ($raku)"
            !! $raku
    }
    method expectedn() {
        nqp::eqaddr($!got.WHAT, $!expected.WHAT)
          ?? Rakudo::Internals.MAYBE-STRING($!expected, :method<raku>)
          !! nqp::can($!expected.HOW, 'name')
            ?? $!expected.^name
            !! '?'
    }
    method priors() {
        nqp::isconcrete($!got) && nqp::istype($!got, Failure)
          ?? "Earlier failure:\n " ~ $!got.mess ~ "\nFinal error:\n "
          !! ''
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
        my $to = $.symbol.defined && $.symbol ne '$'
            ?? " to '$.symbol'"
            !! "";
        my $expected = nqp::eqaddr(self.expected, self.got)
            ?? "expected type $.expectedn cannot be itself"
            !! "expected $.expectedn but got $.gotn";
        self.priors() ~ "Type check failed in $.operation$to; $expected";
    }
}
my class X::TypeCheck::Binding::Parameter is X::TypeCheck::Binding {
    has Parameter $.parameter;
    has Bool $.constraint;
    method expectedn() {
        $.constraint && nqp::istype(self.expected, Code)
            ?? 'anonymous constraint to be met'
            !! callsame()
    }
    method message() {
        my $to = $.symbol.defined && $.symbol ne '$'
            ?? " to parameter '$.symbol'"
            !! " to anonymous parameter";
        my $expected = nqp::eqaddr(self.expected, self.got)
            ?? "expected type $.expectedn cannot be itself"
            !! "expected $.expectedn but got $.gotn";
        my $what-check = $.constraint ?? 'Constraint type' !! 'Type';
        self.priors() ~ "$what-check check failed in $.operation$to; $expected";
    }
}
my class X::TypeCheck::Return is X::TypeCheck {
    method operation { 'returning' }
    method message() {
        my $expected = nqp::eqaddr(self.expected, self.got)
            ?? "expected return type $.expectedn cannot be itself " ~
               "(perhaps $.operation a :D type object?)"
            !! "expected $.expectedn but got $.gotn";
        self.priors() ~
        "Type check failed for return value; $expected";
    }
}
my class X::TypeCheck::Assignment is X::TypeCheck {
    has $.symbol;
    method operation { 'assignment' }
    method message {
        my $to = $.symbol.defined && $.symbol ne '$'
            ?? " to $.symbol" !! "";
        my $is-itself := nqp::eqaddr(self.expected, self.got);
        my $expected = $is-itself
            ?? "expected type $.expectedn cannot be itself"
            !! "expected $.expectedn but got $.gotn";
        my $maybe-Nil := $is-itself
          || nqp::istype(self.expected.HOW, Metamodel::DefiniteHOW)
          && nqp::eqaddr(self.expected.^base_type, self.got)
          ?? ' (perhaps Nil was assigned to a :D which had no default?)' !! '';

        self.priors() ~ "Type check failed in assignment$to; $expected$maybe-Nil"
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
                $!protoguilt ?? 'signature of the proto ' !!
                $multi       ?? 'any of these multi signatures:' !!
                                'declared signature '
            ) ~ $!signature;
    }
}

my class X::TypeCheck::Attribute::Default is X::TypeCheck does X::Comp {
    has str $.name;
    has $.operation;
    method message {
        self.priors() ~
        "Can never $.operation default value $.gotn to attribute '$.name', it expects: $.expectedn"
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
    has $.value = "value";
    method message {
        nqp::isconcrete($!value)
          ?? "Cannot modify an immutable {$!value.^name} ({
                 Rakudo::Internals.SHORT-STRING: $!value
             })"
          !! "Cannot modify an immutable '{$!value.^name}' type object"
    }
    method typename { $.value.^name }
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
        $!parent.^name ~ ' does not support inheritance, so '
        ~ $!child-typename ~ ' cannot inherit from it';
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
        "Cannot use unit $.unit with Date.delta";
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

my class X::Numeric::CannotConvert is Exception {
    has $.target;
    has $.reason;
    has $.source;

    method message() {
        "Cannot convert {$!source // $!source.raku} to {$!target // $!target.raku}: $!reason";
    }

}
my class X::Numeric::Real is X::Numeric::CannotConvert {}

my class X::Numeric::DivideByZero is Exception {
    has $.using;
    has $.details;
    has $.numerator;
    method message() {
        "Attempt to divide{$.numerator ?? " $.numerator" !! ''} by zero"
          ~ ( $.using ?? " using $.using" !! '' )
          ~ ( " $_" with $.details );
    }
}

my class X::Numeric::Overflow is Exception {
    method message() { "Numeric overflow" }
}

my class X::Numeric::Underflow is Exception {
    method message() { "Numeric underflow" }
}

my class X::Numeric::Uninitialized is Exception {
    has Numeric $.type;
    method message() { "Use of uninitialized value of type " ~ $!type.^name ~ " in numeric context" }
}

my class X::Numeric::Confused is Exception {
    has $.num;
    has $.base;
    method message() {
        "This call only converts base-$.base strings to numbers; value "
        ~ "{$.num.raku} is of type {$.num.WHAT.^name}, so cannot be converted!"
        ~ (
            "\n(If you really wanted to convert {$.num.raku} to a base-$.base"
                ~ " string, use {$.num.raku}.base($.base) instead.)"
            if $.num.^can('base')
        );
    }
}

my class X::Enum::NoValue is Exception {
    has Mu $.type is required;
    has $.value is required;
    method message {
        "No value '" ~ $!value ~ "' found in enum " ~ $!type.^name
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

my class X::NoCoreRevision is Exception {
    has $.lang-rev;
    method message { "No CORE for language version 6.$!lang-rev" }
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
        my @bits;
        my @priors;
        if $.capture {
            for $.capture.list {
                try @bits.push(.WHAT.raku);
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
                    try @bits.push(":$(.key)\($(.value.WHAT.raku))");
                    @bits.push(':' ~ .value.^name) if $!;
                }
            }
        }
        else {
            @bits.push('...');
        }
        if @.ambiguous[0].signature.gist.contains: ': ' {
            my $invocant = @bits.shift;
            my $first = @bits ?? @bits.shift !! '';
            @bits.unshift($invocant ~ ': ' ~ $first);
        }
        my $cap = '(' ~ @bits.join(", ") ~ ')';
        @priors = flat "Earlier failures:\n", @priors, "\nFinal error:\n " if @priors;
        @priors.join ~ join "\n",
            "Ambiguous call to '$.dispatcher.name()$cap'; these signatures all match:",
            @.ambiguous.map: {
                my $sig := .signature.raku.substr(1).subst(/ \s* "-->" <-[)]>+ /);
                .?default ?? "  $sig is default" !! "  $sig"
            }
    }
}

my class X::Multi::NoMatch is Exception {
    has $.dispatcher;
    has $.capture;
    method message {
        my @cand = $.dispatcher.dispatchees.map(*.signature.gist);
        my @un-rw-cand;
        if first / 'is rw' /, @cand {
            my $rw-capture = Capture.new(
                :list( $!capture.list.map({ my $ = $_ })                  ),
                :hash( $!capture.hash.map({ .key => my $ = .value }).hash ),
            );
            @un-rw-cand = $.dispatcher.dispatchees».signature.grep({
                $rw-capture ~~ $^cand
            })».gist;
        }

        my $where = so first / where /, @cand;
        my @bits;
        my @priors;
        if $.capture {
            for $.capture.list {
                try @bits.push(
                    $where ?? Rakudo::Internals.SHORT-STRING($_) !! .WHAT.raku ~ ':' ~ (.defined ?? "D" !! "U")
                );
                @bits.push($_.^name) if $!;
                if nqp::istype($_,Failure) {
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
                    try @bits.push(":$(.key)\($($where
                        ?? Rakudo::Internals.SHORT-STRING(.value)
                        !! .value.WHAT.raku
                    ))");
                    @bits.push(':' ~ .value.^name) if $!;
                }
            }
        }
        else {
            @bits.push('...');
        }
        if @cand && @cand[0] ~~ /': '/ {
            my $invocant = @bits.shift;
            my $first = @bits ?? @bits.shift !! '';
            @bits.unshift($invocant ~ ': ' ~ $first);
        }
        my $cap = '(' ~ @bits.join(", ") ~ ')';
        @priors = flat "Earlier failures:\n", @priors, "\nFinal error:\n " if @priors;
        @priors.join ~ "Cannot resolve caller $.dispatcher.name()$cap; " ~ (
            @un-rw-cand
            ?? "the following candidates\nmatch the type but require "
                ~ 'mutable arguments:' ~  join("\n    ", '', @un-rw-cand) ~ (
                        "\n\nThe following do not match for other reasons:"
                        ~  join("\n    ", '', sort keys @cand ∖ @un-rw-cand)
                    unless @cand == @un-rw-cand
                )
            !! ( @cand
                 ??  join "\n    ", 'none of these signatures match:', @cand
                 !! "Routine does not have any candidates. Is only the proto defined?"
               )
        );
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

my class X::Language::IncompatRevisions is Exception {
    has Mu $.type-a is built(:bind) is required;
    has Mu $.type-b is built(:bind) is required;
    method message() {
        "Type object "
        ~ $!type-a.^name ~ " of " ~ $!type-a.^language-version
        ~ " is not compatible with "
        ~ $!type-b.^name ~ " of " ~ $!type-b.^language-version
    }
}

my role X::Nominalizable is Exception {
    has Mu $.nominalizable is built(:bind) is required;
}

my class X::Nominalizable::NoWrappee does X::Nominalizable {
    has %.kinds is required where *.elems; # This would be the named parameters passed to Metamodel::Nominalizable::wrappee method
    method message() {
        my $kinds = %!kinds.keys.join(", or ");
        "Can't find requested wrappee of " ~ $kinds ~ " on " ~ $.nominalizable.^name
    }
}

my class X::Nominalizable::NoKind does X::Nominalizable {
    method message {
        $.nominalizable.HOW.^name ~ " does not declare 'nominalizable_kind' method"
    }
}

#?if !moar
nqp::bindcurhllsym('P6EX', nqp::hash(
#?endif
#?if moar
nqp::bindcurhllsym('P6EX', BEGIN nqp::hash(
#?endif
  'X::TypeCheck::Binding',
  -> Mu $got is raw, Mu $expected is raw, $symbol? is raw {
      X::TypeCheck::Binding.new(:$got, :$expected, :$symbol).throw;
  },
  'X::TypeCheck::Binding::Parameter',
  -> Mu $got is raw, Mu $expected is raw, $symbol is raw, $parameter is raw, $is-constraint? is raw {
      my $constraint = $is-constraint ?? True !! False;
      X::TypeCheck::Binding::Parameter.new(:$got, :$expected, :$symbol, :$parameter, :$constraint).throw;
  },
  'X::TypeCheck::Assignment',
  -> Mu $symbol is raw, Mu $got is raw, Mu $expected is raw {
      X::TypeCheck::Assignment.new(:$symbol, :$got, :$expected).throw;
  },
  'X::TypeCheck::Return',
  -> Mu $got is raw, Mu $expected is raw {
      X::TypeCheck::Return.new(:$got, :$expected).throw;
  },
  'X::Assignment::RO',
  -> $value is raw = "value" {
      X::Assignment::RO.new(:$value).throw;
  },
  'X::ControlFlow::Return',
  -> $out-of-dynamic-scope is raw = False {
      X::ControlFlow::Return.new(:$out-of-dynamic-scope).throw;
  },
  'X::NoDispatcher',
  -> $redispatcher is raw {
      X::NoDispatcher.new(:$redispatcher).throw;
  },
  'X::Method::NotFound',
  -> Mu $invocant is raw, $method is raw, $typename is raw, $private is raw = False, :$in-class-call is raw = False {
      X::Method::NotFound.new(:$invocant, :$method, :$typename, :$private, :$in-class-call).throw
  },
  'X::Multi::Ambiguous',
  -> $dispatcher is raw, @ambiguous is raw, $capture is raw {
      X::Multi::Ambiguous.new(:$dispatcher, :@ambiguous, :$capture).throw
  },
  'X::Multi::NoMatch',
  -> $dispatcher is raw, $capture is raw {
      X::Multi::NoMatch.new(:$dispatcher, :$capture).throw
  },
  'X::Role::Initialization',
  -> $role is raw {
      X::Role::Initialization.new(:$role).throw
  },
  'X::Inheritance::NotComposed',
  -> $child-name is raw, $parent-name is raw {
      X::Inheritance::NotComposed.new(:$child-name, :$parent-name).throw;
  },
  'X::Parameter::RW',
  -> Mu $got is raw, $symbol is raw {
      X::Parameter::RW.new(:$got, :$symbol).throw;
  },
  'X::PhaserExceptions',
  -> @exceptions is raw {
      X::PhaserExceptions.new(exceptions =>
        @exceptions.map(-> Mu \e { EXCEPTION(e) })).throw;
  },
  'X::Trait::Invalid',
  -> $type is raw, $subtype is raw, $declaring is raw, $name is raw {
      X::Trait::Invalid.new(:$type, :$subtype, :$declaring, :$name).throw;
  },
  'X::Parameter::InvalidConcreteness',
  -> $expected is raw, $got is raw, $routine is raw, $param is raw, Bool() $should-be-concrete is raw, Bool() $param-is-invocant is raw {
      X::Parameter::InvalidConcreteness.new(:$expected, :$got, :$routine, :$param, :$should-be-concrete, :$param-is-invocant).throw;
  },
  'X::NYI',
  -> $feature is raw {
      X::NYI.new(:$feature).throw;
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
    has Str:D $.suggestion = 'Please use the block form.';
    method message() {
        "A unit-scoped $.what definition is not allowed $.where;\n$!suggestion"
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

my class X::MustBeParametric is Exception {
    has $.type;
    method message() {
        "$!type.^name() *must* be parameterized";
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
        "Invalid type smiley ':$.name' used, only ':D', ':U' and ':_' are allowed";
    }
}

my class X::MultipleTypeSmiley does X::Comp {
    method message() {
        "Multiple type smileys cannot be used, did you forget a ':' somewhere?";
    }
}

my class X::Seq::Consumed is Exception {
    method message() {
        "The iterator of this Seq is already in use/consumed by another Seq\n" ~
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
        "Cannot have a 'whenever' block outside the scope of a 'supply' or 'react' block"
    }
}

my class X::Comp::WheneverOutOfScope does X::Comp {
    method message() {
        "Cannot have a 'whenever' block outside the scope of a 'supply' or 'react' block"
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

my class X::IllegalDimensionInShape is Exception {
    has $.dim;
    method message() {
        "Illegal dimension in shape: $.dim. All dimensions must be integers bigger than 0"
    }
}

my class X::ArrayShapeMismatch is Exception {
    has $.action = "assign";
    has $.target-shape;
    has $.source-shape;
    method message() {
        "Cannot assign an array of shape $.source-shape to an array of shape $.target-shape"
    }
}
my class X::Assignment::ArrayShapeMismatch is X::ArrayShapeMismatch {
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
        "No compiler available for Raku $.version"
    }
}
my class X::Language::TooLate is Exception {
    method message() {
        "Too late to switch language version. Must be used as the very first statement."
    }
}
my class X::Language::ModRequired is Exception {
    has $.version;
    has $.modifier;
    method message() {
        "Raku $.version requires $.modifier modifier"
    }
}

my class X::Proc::Unsuccessful is Exception {
    has $.proc;
    method message() {
        "The spawned command '{$.proc.command[0]}' exited unsuccessfully (exit code: $.proc.exitcode(), signal: $.proc.signal())"
    }
}

class CompUnit::DependencySpecification { ... }
class CompUnit::Repository::FileSystem { ... }
my class X::CompUnit::UnsatisfiedDependency is Exception {
    has CompUnit::DependencySpecification $.specification;

    my sub is-core($name) {
        my @parts = $name.split("::");
        my $last := @parts.pop;
        my $ns := ::CORE.WHO;
        for @parts {
            return False unless $ns{$_}:exists;
            $ns := $ns{$_}.WHO;
        };
        $ns{$last}:exists
            and not nqp::istype(nqp::how($ns{$last}), Metamodel::PackageHOW)
    }

    method !is-missing-from-meta-file() {
        $*REPO.isa(CompUnit::Repository::FileSystem) and $*REPO.prefix.add("META6.json").e
    }

    method message() {
        my $name = $.specification.short-name;
        is-core($name)
            ?? "{$name} is a builtin type, not an external module"
            !! "Could not find $.specification in:\n"
                ~ $*REPO.repo-chain.map(*.path-spec).join("\n").indent(4)
                ~ ($.specification ~~ / $<name>=.+ '::from' $ /
                    ?? "\n\nIf you meant to use the :from adverb, use"
                        ~ " a single colon for it: $<name>:from<...>\n"
                    !! self!is-missing-from-meta-file
                        ?? "\n\nPlease note that a 'META6.json' file was found in '$*REPO.prefix.relative()',"
                            ~ " of which the 'provides' section was used to determine if a dependency is available"
                            ~ " or not.  Perhaps you need to add '$!specification' in the <provides> section of"
                            ~ " that file?  Or need to specify a directory that does *not* have a 'META6.json' file?"
                        !! ''
                )
    }
}

my class Exceptions::JSON {
    method process($ex) {
        $*ERR.print: Rakudo::Internals::JSON.to-json($ex);
        False  # done processing
    }
}

# Provide means of accessing any X:: exception to the Metamodel.
Metamodel::Configuration.set_X_package(X);

# vim: expandtab shiftwidth=4
