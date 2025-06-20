my class Grammar is Match {
    use NQPHLL:from<NQP>;

    # (oughta be down in Match or even nqp really)
    method locprepost() {
        my $orig = self.orig;
        my $marked = self.?MARKED('ws');
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
                    my $x = nqp::hllizefor(nqp::shift($highexpect),'Raku');
                    push @expected, $x unless %seen{$x}++;
                }
                @expected .= sort;
            }
        }

        my @locprepost := self.locprepost();
        # Build and throw exception object.
        %opts<line>            = HLL::Compiler.lineof(self.orig, self.pos, :cache(1));
        # only set <pos> if it's not already set:
        %opts<pos>             //= self.pos;
        %opts<pre>             = @locprepost[0];
        %opts<post>            = @locprepost[1];
        %opts<highexpect>      = @expected if @expected;
        %opts<is-compile-time> = 1;
        $ex.new(|%opts).Failure
    }

    method SETFAIL($failed, :$filename) {
        return Nil unless self.defined;
        self.'!cursor_pos'($failed.pos);
        self.typed_exception(X::Syntax::Confused, filename => ($filename // "<anon>"))
    }

    method parse(\target, :$rule, :$args, Mu :$actions, :$filename) {
        my $*LINEPOSCACHE;
        my $grammar := self.new(:orig(target), |%_);
        $grammar.set_actions($actions)
          unless nqp::eqaddr(nqp::decont($actions),Mu);

        nqp::if(
          (my $cursor := nqp::if(
          $rule,
            nqp::if(
              $args,
              $grammar."$rule"(|$args.Capture),
              $grammar."$rule"()
            ),
            nqp::if(
              $args,
              $grammar.TOP(|$args.Capture),
              $grammar.TOP()
            ),
          )),
          nqp::stmts(
            (my $match := $cursor.Match::MATCH),
            nqp::while(
              $match && nqp::isne_i(
                nqp::getattr_i(($match := $cursor.Match::MATCH),Match,'$!pos'),
                target.chars
            ),
              $match := ($cursor := $cursor.'!cursor_next'()).Match::MATCH
            ),
            $match || $grammar.SETFAIL($match, :$filename),
          ),
          $grammar.SETFAIL($cursor, :$filename),
        )
    }

    method subparse(\target, :$rule = 'TOP', :$args, :$actions) {
        my $grammar := self.new(:orig(target), |%_);
        $grammar.set_actions($actions)
          unless nqp::eqaddr(nqp::decont($actions),Mu);

        $args
          ?? $grammar."$rule"(|$args.Capture).Match::MATCH
          !! $grammar."$rule"().Match::MATCH
    }

    method parsefile(Str(Cool) $filename, :$enc) {
        nqp::elems(nqp::getattr(%_,Map,'$!storage'))
          ?? self.parse($filename.IO.slurp(:$enc), :$filename, |%_)
          !! self.parse($filename.IO.slurp(:$enc), :$filename)
    }
}

# vim: expandtab shiftwidth=4
