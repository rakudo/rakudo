class Perl6::CommandLine::Result is HLL::CommandLine::Result {
    has %!delim-opts;
    has %!options;
    has @!arguments;

    method options() {
        %!options
    }

    method arguments() {
        @!arguments
    }

    method delim-opts() {
        %!delim-opts;
    }

    method add-argument($x) {
        nqp::push(@!arguments, $x);
    }

    method init() {
        @!arguments := [];
        %!options := nqp::hash();
        %!delim-opts := nqp::hash;
    }

    method add-option($name, $value, :$delimited) {
        # how I miss p6's Hash.push
        my %opts := $delimited ?? %!delim-opts !! %!options;

        if nqp::existskey(%opts, $name) {
            if nqp::islist(%opts{$name}) {
                nqp::push(%opts{$name}, $value);
            } else {
                %opts{$name} := [ %opts{$name}, $value ];
            }
        } else {
            %opts{$name} := $value;
        }
    }
}

class Perl6::CommandLine::Parser is HLL::CommandLine::Parser {
    has %!opt-to-aliases;
    has %!aliases-to-types;
    has %!cli-options;
    has %!stopper;

    method add-stopper($x) {
        %!stopper{$x} := 1;
    }

    method new(@spec) {
        my $obj := self.CREATE;
        $obj.BUILD(specs => @spec);
        $obj
    }

    method BUILD(:@specs) {
        %!opt-to-aliases := nqp::hash;
        %!aliases-to-types := nqp::hash;
        %!stopper := nqp::hash('--', 1);
        for @specs -> $optspec {
            my $eqsign := nqp::index($optspec, '=');
            my $type;
            my @options;
            my $aliasgroup;
            if $eqsign < 0 {
                $type := 'b';
                $aliasgroup := $optspec;
                @options := nqp::split('|', $aliasgroup);
            }
            else {
                $type := nqp::substr($optspec, $eqsign + 1);
                $aliasgroup := nqp::substr($optspec, 0, $eqsign);
                @options := nqp::split('|', $aliasgroup);
            }
            for @options -> $opt {
                if nqp::chars($opt) == 1 {
                    nqp::push(@*boolshopts, $opt) if $type eq 'b';
                    nqp::push(@*stringshopts, $opt) if $type ne 'b';
                }
                else {
                    nqp::push(@*longopts, $opt);
                }
                %*only-once{$opt} := 1 if nqp::substr($type, 0, 1) eq 's';
                %!opt-to-aliases{$opt} := $aliasgroup;
                %!aliases-to-types{$aliasgroup} := $type;
            }
        }
    }

    method parse(@args) {
        my grammar CLIParser {
            token ws {
                \x0
            }

            token TOP {
                :my $*STOPPED := 0;
                <option>* % <.ws>
                [
                    <.ws>
                    <argument>* % <.ws>
                    <.ws>?
                ]?
            }

            token option {
                <!{ $*STOPPED }>
                [
                    [ '--' <.ws>+ { $*STOPPED := 1 } ]
                ||  <delim-opts>
                ||  <longopt>
                ||  <shopts>
                ]
            }

            token delim-opts {
                $<delim> = [ '+' '+'+ ]
                $<opt-key> = \w+
                $<opt-value> = [ <!before $<delim> > . ]+?
                $<delim>
                '/'
                $<opt-key>
            }

            token longopt {
                <!{ $*STOPPED }>
                [ '--' || ':' ]
                [ $<negated> = '/' ]?
                $<optname> = @*longopts
                [
                    <?{ %*aliases-to-types{%*opt-to-aliases{$<optname>}} eq 'b' }>
                ||  <.optvalsep> <!after '-'> <value>
                ]
                { for @*stoppers { if $_ eq '--' ~ $<optname> { $*STOPPED := 1 } } }
            }

            token shopts {
                <!{ $*STOPPED }>
                '-'
                [ $<negated> = '/' ]?
                [ $<boolshopts> = @*boolshopts ]*
                [
                    $<strshopt> = @*stringshopts
                    <.optvalsep>?
                    <value>
                ]?
                { for @*stoppers { if $_ eq '-' ~ $<strshopt> { $*STOPPED := 1 } } }
            }

            token optvalsep {
                <.ws> || '='
            }

            token value {
                <-[\x0]>+
            }

            token argument {
                <-[\x0]>+
            }
        }

        my class CLIActions {
            has %!aliases-to-types;
            has %!opt-to-aliases;

            method type-for-opt($opt) {
                %!aliases-to-types{%!opt-to-aliases{$opt}}
            }

            method warn-deprecated($opt, $is-long) {
                my $type := self.type-for-opt($opt);
                my $new := nqp::substr($type, 1, nqp::chars($type) - 1);
                self.error-out("===SORRY!===" ~
                    "\nOption -" ~ nqp::x('-', $is-long) ~ $opt ~ " is deprecated, please use " ~ $new ~ ".");
            }

            method TOP($/) {
                my $result := Perl6::CommandLine::Result.new;
                $result.init;
                if $<option> {
                    my @a := make $<option>;
                    for @a {
                        my %opt := $_.ast;
                        for %opt {
                            my $key := ~(nqp::iterkey_s($_));
                            my $value := ~(nqp::iterval($_));
                            my $delimited := $key eq 'DELIMITED-OPTION';
                            if %*only-once{$key} && $result.options{$key} {
                                self.error-out("===SORRY!===" ~
                                    "\nOption " ~ $key ~ " can only be supplied once.");
                            }
                            elsif $delimited {
                                $key := $value[0];
                                $value := $value[1];
                            }
                            $result.add-option($key, $value, :$delimited);
                        }
                    }
                }
                if $<argument> {
                    my @a := make $<argument>;
                    for @a {
                        $result.add-argument(~(make $_.ast));
                    }
                }
                my %delim-opts;
                make $result;
            }

            method option($/) {
                my %opts;
                if $<longopt> {
                    my $p := make $<longopt>.ast;
                    for $p {
                        my $key := nqp::iterkey_s($_);
                        my $value := nqp::iterval($_);
                        if nqp::substr(self.type-for-opt($key), 0, 1) eq 'd' {
                            self.warn-deprecated($key, 1);
                        }
                        %opts{$key} := $value;
                    }
                }
                if $<shopts> {
                    my $p := make $<shopts>.ast;
                    for $p {
                        my $key := nqp::iterkey_s($_);
                        my $value := nqp::iterval($_);
                        if nqp::substr(self.type-for-opt($key), 0, 1) eq 'd' {
                            self.warn-deprecated($key, 0);
                        }
                        %opts{$key} := $value;
                    }
                }
                if $<delim-opts> {
                    my %a := make $<delim-opts>.ast;
                    for %a {
                        my $key := nqp::iterkey_s($_);
                        my $value := nqp::iterval($_);
                        %opts<DELIMITED-OPTION> := [$key, $value];
                    }
                }
                make %opts;
            }

            method delim-opts($/) {
                my $key := $<opt-key>.Str;
                my $value := $<opt-value>.Str;
                my %delim-opts;
                $value := nqp::join(" ", nqp::split("\x0", $value));
                %delim-opts{$key} := $value;
                make %delim-opts;
            }

            method shopts($/) {
                my %opts := nqp::hash;

                if nqp::elems($<boolshopts> ) > 1 && $<negated> {
                    self.error-out("===SORRY!===" ~
                        "\nGrouped short options cannot be negated.");
                }


                for $<boolshopts> {
                    %opts{$_} := $<negated> ?? 0 !! 1;
                }

                my $value := '';
                if $<strshopt> && $<value> {
                    $value := make $<value>;
                    %opts{$<strshopt>.Str} := $value;
                }
                make %opts;
            }

            method longopt($/) {
                my $value := $<value> ?? make $<value>.ast !! 1;
                $value := ~($value // 1);
                my $key := $<optname>.Str;

                if $<negated> && self.type-for-opt($key) ne 'b' {
                    self.error-out("===SORRY!===" ~
                        "\nOptions that take a string value cannot be negated.");
                }
                elsif $<negated> {
                    $value := $<negated> eq '/' ?? $value == 0 !! $value == 1;
                }

                make nqp::hash($<optname>, $value);
            }

            method value($/) {
                make $/.Str
            }

            method argument($/) {
                make $/.Str
            }

            method error-out($msg) {
                nqp::say($msg);
                nqp::exit(0);
            }
        }

        my $args := nqp::join("\x0", @args);

        my %*opt-to-aliases := %!opt-to-aliases;
        my %*aliases-to-types := %!aliases-to-types;
        my @*stoppers;
        for %!stopper {
            nqp::push(@*stoppers, nqp::iterkey_s($_));
        }

        my $result;
        my $actions := CLIActions.new(:aliases-to-types(%!aliases-to-types), :opt-to-aliases(%!opt-to-aliases));
        $result := CLIParser.parse($args, :$actions).ast;

        if nqp::istype($result, Perl6::CommandLine::Result) {
            $result;
        }
        else {
            nqp::die("Failed parsing command line.");
        }
    }
}
