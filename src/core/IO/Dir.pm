# A class for directories that we know exist
my class IO::Dir does IO {
    has $!abspath;  # assumes we have a trailing slash *ALWAYS*
    has @!parts;

    my %nul = '..' => 1, '.' => 1, '' => 1;

    submethod BUILD(:$!abspath,:$check) {
        if $check {   # should really be .resolve, but we don't have that yet
            self!parts;

            # handle //unc/ on win
            @!parts.unshift( @!parts.splice(0,3).join('/') )
              if @!parts.at_pos(1) eq ''    # //
              and @!parts.at_pos(0) eq '';  # and no C: like stuff

            # front part cleanup
            @!parts.splice(1,1) while %nul.exists_key(@!parts.at_pos(1)); 

            # back part cleanup
            my Int $checks = @!parts.end;
            while $checks > 1 {
                if @!parts.at_pos($checks) -> $part {
                    $part eq '..'
                      ?? @!parts.splice(--$checks, 2)
                      !! $part eq '.'
                        ?? @!parts.splice($checks--, 1)
                        !! $checks--;
                }
                else {
                    @!parts.splice($checks--, 1);
                }
            }
            @!parts.push("");
            $!abspath = @!parts.join('/');
            fail "$!abspath is not a directory" unless FILETEST-D($!abspath);
        }
    }

    method !parts() {
        @!parts = $!abspath.split('/') unless @!parts;
    }

    method child($child) {
        $child
          ?? self.new(:abspath($!abspath ~ $child ~ '/'),:check)
          !! self;
    }

    method parent($levels = 1) {
        self!parts;
        @!parts <= $levels + 1
          ?? self.new(:abspath( @!parts[0] ~ '/' ))
          !! self.new(:abspath( @!parts[0 .. *-($levels + 2)].join('/') ~ '/'));
    }

    method chdir($path) {
        self.new( MAKE-ABSOLUTE-PATH($path,$!abspath), :check );
    }

    method open(|c) {
        fail (X::IO::Directory.new(:$!abspath, :trying<open>));
    }

    method Str  { $!abspath }
    method gist { "q|$!abspath|.IO" }
    method perl { "q|$!abspath|.IO" }

    method e   { True }
    method d   { True }
    method f   { False }
    method s   { 0 }
    method l   { False }
    method r   { FILETEST-R(  $!abspath) }
    method w   { FILETEST-W(  $!abspath) }
    method rw  { FILETEST-RW( $!abspath) }
    method x   { FILETEST-X(  $!abspath) }
    method rwx { FILETEST-RWX($!abspath) }
    method z   { True }
    method modified { FILETEST-MODIFIED($!abspath) }
    method accessed { FILETEST-ACCESSED($!abspath) }
    method changed  { FILETEST-CHANGED( $!abspath) }
}
