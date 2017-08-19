augment class Grammar {

    # (oughta be down in Match or even nqp really)
    method locprepost() {
        my $orig = self.orig;
        my $marked = try self.MARKED('ws');
        my $pos = $marked && index(" }])>»", substr($orig, self.pos, 1)) < 0 ?? $marked.from !! self.pos;

        my $prestart = $pos - 40;
        $prestart = 0 if $prestart < 0;
        my $pre = substr($orig, $prestart, $pos - $prestart);
        $pre = $pre.subst(/.*\n/, "");
        $pre = '<BOL>' if $pre eq '';

        my $postchars = $pos + 40 > chars($orig) ?? chars($orig) - $pos !! 40;
        my $post = substr($orig, $pos, $postchars);
        $post = $post.subst(/\n.*/, "");
        $post = '<EOL>' if $post eq '';

        [$pre, $post]
    }

    # stolen and trimmed down from World
    method typed_exception(X::Comp $ex, *%opts) {
        # If the highwater is beyond the current position, force the cursor to that location.
        my @expected;
        my $high = self.'!highwater'();

        if $high >= self.pos() {
            self.'!cursor_pos'($high);
            my $highexpect := self.'!highexpect'();
            if nqp::islist($highexpect) {
                my %seen;
                for ^nqp::elems($highexpect) {
                    my $x = nqp::hllizefor(nqp::shift_s($highexpect),'perl6');
                    push @expected, $x unless %seen{$x}++;
                }
                @expected .= sort;
            }
        }

        my @locprepost := self.locprepost();
        # Build and throw exception object.
        %opts<line>            = nqp::getcomp('perl6').lineof(self.orig, self.pos, :cache(1));
        # only set <pos> if it's not already set:
        %opts<pos>             //= self.pos;
        %opts<pre>             = @locprepost[0];
        %opts<post>            = @locprepost[1];
        %opts<highexpect>      = @expected if @expected;
        %opts<is-compile-time> = 1;
        Failure.new($ex.new(|%opts))
    }

    method SETFAIL($failed, :$filename) {
        return Nil unless self.defined;
        self.'!cursor_pos'($failed.pos);
        self.typed_exception(X::Syntax::Confused, filename => ($filename // "<anon>"))
    }
}

# vim: ft=perl6 expandtab sw=4
