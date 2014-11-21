# class for Unclassified IO objects
my class IOU {
    has $!this;
    has $!CWD;
    has $!that;

    submethod BUILD(:$!this,:$!CWD) { }
    method new($this, :$CWD = $*CWD) {
        self!what($this,$CWD) // self.bless(:$this,:$CWD);
    }
    method that(IOU:D: $method) {
        self.$!that //= self!what($!this,$!CWD) // Nil;
    }

    method !what(IOU: $this,$CWD) {
        my $abspath := MAKE-ABSOLUTE-PATH($this,$CWD);
        if FILETEST-E($abspath) {
            if FILETEST-F($abspath) {
                return IO::File.new(:$abspath);
            }
            elsif FILETEST-D($abspath) {
                return IO::Dir.new(:$abspath);
            }
            elsif FILETEST-L($abspath) {
                return IO::Link.new(:$abspath);
            }
        }
        Nil;
    }

    multi method ACCEPTS(IOU:D: \other) {
        $!that.?ACCEPTS(other) // nqp::p6bool(
          nqp::iseq_s(nqp::unbox_s($!this), nqp::unbox_s(~other))
        );
    }

    method IOU(IOU:D:)           { self }
    method Numeric(IO::Local:D:) { self.basename.Numeric }
    method Bridge(IO::Local:D:)  { self.basename.Bridge }
    method Int(IO::Local:D:)     { self.basename.Int }

    multi method Str(IOU:D:)  { $!this }
    multi method gist(IOU:D:) { qq|"$!this".IO| }
    multi method perl(IOU:D:) { "q|$!this|.IO" }

    # handles doesn't work in the settings, so we need to be verbose
    method absolute(IOU:D:)    { $.that.abspath }
    method relative(IOU:D: |c) { $.that.relative(|c) }
    method chop(IOU:D:)        { $.that.chop }
    method volume(IOU:D:)      { $.that.volume }
    method dirname(IOU:D:)     { $.that.dirname }
    method basename(IOU:D:)    { $.that.basename }
    method extension(IOU:D:)   { $.that.extension }
    method e(IOU:D:)           { $.that.e }
    method d(IOU:D:)           { $.that.d }
    method s(IOU:D:)           { $.that.s }
    method l(IOU:D:)           { $.that.l }
    method r(IOU:D:)           { $.that.r }
    method w(IOU:D:)           { $.that.w }
    method rw(IOU:D:)          { $.that.rw }
    method x(IOU:D:)           { $.that.x  }
    method rwx(IOU:D:)         { $.that.rwx }
    method z(IOU:D:)           { $.that.z   }
    method modified(IOU:D:)    { $.that.modified }
    method accessed(IOU:D:)    { $.that.accessed }
    method changed(IOU:D:)     { $.that.changed }
}

#multi method push(Any:U \SELF: *@values) {
#SELF = nqp::istype(SELF,Positional) ?? SELF.new !! Array.new;

# vim: ft=perl6 expandtab sw=4
