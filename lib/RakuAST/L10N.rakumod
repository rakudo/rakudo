# always use highest version of Raku
use v6.*;

# Known groups of translation
my constant %known-groups = <
  adverb-pc adverb-q adverb-rx block constraint core enum infix meta
  modifier multi named package phaser pragma prefix quote-lang routine
  scope stmt-prefix system term traitmod trait-is typer use
>.map({ $_ => 1 });
my constant %sub-groups = <core named>.map({ $_ => 1 });

# Set up core settings
my @core = CHECK $=finish.lines.map: { .words.Slip unless .starts-with("#") }

#- fresh-translation-file ------------------------------------------------------
# Produce a fresh translation file
my sub fresh-translation-file(IO::Path:D $io) is export {
    my str @lines = $=finish.lines.map: {
        if .starts-with("#") {
            $_ if .starts-with("# KEY" | "# vim")
        }
        else {
            $_ ?? "#$_" !! ""
        }
    }

    $io.spurt(qq:to/HEADER/ ~ @lines.join("\n"))
# This file contains the $io.basename() localization of the
# Raku Programming Language.
#
# CONTRIBUTORS: 
#
# See https://github.com/Raku-L10N/L10N/ for more information
HEADER
}

#- read-hash -------------------------------------------------------------------
my proto sub read-hash(|) is export {*}

# Produce the "bare" core translations
my multi sub read-hash() { %(flat @core) }

# Produce all words on non-commented lines of given IO as a Slip
sub io2words(IO::Path:D $io) {
    $io.lines.map: { .words.Slip unless .starts-with("#") }
}

# Read translation hash from given file (with path as string)
my multi sub read-hash(Str:D $path, :$core) { read-hash($path.IO, :$core) }

# Read translation hash from given file (as IO object)
my multi sub read-hash(IO::Path:D $io, :$core) {
    $core
      ?? %(flat @core, io2words($io))
      !! %(io2words($io))
}

#- missing-translations --------------------------------------------------------
# Return a sorted list of keys that do not have a translation for a given IO
my sub missing-translations(IO::Path:D $io) is export {
    (read-hash() (-) read-hash($io)).keys.sort
}

#- write-hash ------------------------------------------------------------------
# Write out the localization of the given hash to the given file (as IO object)
my sub write-hash(IO::Path:D $io, %mapping) is export {

    # Return the group for the given key
    my %groups;
    sub group-hash(Str:D $key) {
        my int $disabled = +$key.starts-with("#");
        my str @parts    = $key.split("-");

        # The group is determined from the given key, taking into account
        # a potential "#" prefix on the key.  Since some keys contain hyphens
        # and the rest of the key can also contain hyphens, we need to do
        # this dance to find a legal key to be used.
        my str $group = @parts.shift;
        $group = $group.substr(1) if $disabled;
        until %known-groups{$group} || !@parts {
            $group = $group ~ "-" ~ @parts.shift;
        }
        die "No group found for $key" unless @parts;

        # Groups with many potential localization keys, are divided into
        # sub-groups including the first letter.
        $group ~= $key.substr($group.chars + $disabled, 2).lc
          if %sub-groups{$group};

        # Fetch or create the hash
        %groups{$group} // (%groups{$group} := {})
    }

    # Set up base information, including translations not yet done (which
    # start with an "#" immediately followed by the key.)
    for $io.lines {
        unless .starts-with("# ") || $_ eq "#" || .is-whitespace {
            my ($key,$translation) = .words;
            group-hash($key){$key} := $translation;
        }
    }

    # Update the groups from the given hash
    for %mapping {
        my $key  := .key;
        my %hash := group-hash($key);
        %hash{"#$key"}:delete;  # remove any untranslated
        %hash{$key} := .value;  # set as translated
    }

    # Start building the file
    my str @lines;
    my $handle := $io.open(:!chomp);
    for $handle.lines {
        .starts-with("#")
          ?? @lines.push($_)
          !! last
    }
    $handle.close;

    # Add the lines with the translations in correct order to reduce the
    # amount of changes on updates.
    for %groups.sort(*.key) {
        my %hash   := %groups{.key};
        my int $max = %hash.keys.map({ .chars - .starts-with("#") }).max;
        my $format := '%-' ~ $max ~ "s  %s\n";

        @lines.push("\n");
        @lines.push(sprintf($format, "# KEY", "TRANSLATION"));

        for %hash.sort(-> $a is copy, $b is copy {
            $a = $a.key;
            $a = $a.substr(1) if $a.starts-with("#");
            $b = $b.key;
            $b = $b.substr(1) if $b.starts-with("#");

            $a.fc cmp $b.fc || $b cmp $a
        }) {
            my $key := .key;
            @lines.push($key.starts-with("#")
              ?? "#" ~ sprintf($format, $key.substr(1), .value)
              !! sprintf($format, $key, .value)
            );
        }
    }

    # Put in the vim marker
    @lines.push("\n");
    @lines.push("# vim: expandtab shiftwidth=4\n");

    $io.spurt(@lines.join);
}

#- make-mapper2ast -------------------------------------------------------------
# Return the AST for translation lookup logic, basically:
#
# method $name {
#     my constant %mapping = @operands;
#     my $ast  := self.ast;
#     my $name := $ast ?? $ast.simple-identifier !! self.Str;
#     if %mapping{$name} -> $original {
#         RakuAST::Name.from-identifier($original)
#     }
#     else {
#         $ast // RakuAST::Name.from-identifier($name)
#     }
# }
#
# if there are any operands, otherwise:
#
# method $name { self.ast // RakuAST::Name.from-identifier(self.Str) }
#
sub make-mapper2ast(str $name, @operands) {
    my $stmts := @operands
      ?? RakuAST::StatementList.new(
           RakuAST::Statement::Expression.new(
             expression => RakuAST::VarDeclaration::Constant.new(
               scope       => "my",
               name        => "\%mapping",
               initializer => RakuAST::Initializer::Assign.new(
                 RakuAST::ApplyListInfix.new(
                   infix    => RakuAST::Infix.new(","),
                   operands => @operands,
                 )
               )
             )
            ),
            RakuAST::Statement::Expression.new(
              expression => RakuAST::VarDeclaration::Simple.new(
                sigil       => "\$",
                desigilname => RakuAST::Name.from-identifier("ast"),
                initializer => RakuAST::Initializer::Bind.new(
                  RakuAST::ApplyPostfix.new(
                    operand => RakuAST::Term::Self.new,
                    postfix => RakuAST::Call::Method.new(
                      name => RakuAST::Name.from-identifier("ast")
                    )
                  )
                )
              )
            ),
            RakuAST::Statement::Expression.new(
              expression => RakuAST::VarDeclaration::Simple.new(
                sigil       => "\$",
                desigilname => RakuAST::Name.from-identifier("name"),
                initializer => RakuAST::Initializer::Bind.new(
                  RakuAST::Ternary.new(
                    condition => RakuAST::Var::Lexical.new("\$ast"),
                    then      => RakuAST::ApplyPostfix.new(
                      operand => RakuAST::Var::Lexical.new("\$ast"),
                      postfix => RakuAST::Call::Method.new(
                        name => RakuAST::Name.from-identifier("simple-identifier")
                      )
                    ),
                    else      => RakuAST::ApplyPostfix.new(
                      operand => RakuAST::Term::Self.new,
                      postfix => RakuAST::Call::Method.new(
                        name => RakuAST::Name.from-identifier("Str")
                      )
                    )
                  )
                )
              )
            ),
            RakuAST::Statement::If.new(
              condition => RakuAST::ApplyPostfix.new(
                operand => RakuAST::Var::Lexical.new("\%mapping"),
                postfix => RakuAST::Postcircumfix::HashIndex.new(
                  index => RakuAST::SemiList.new(
                    RakuAST::Statement::Expression.new(
                      expression => RakuAST::Var::Lexical.new("\$name")
                    )
                  )
                )
              ),
              then      => RakuAST::PointyBlock.new(
                signature => RakuAST::Signature.new(
                  parameters => (
                    RakuAST::Parameter.new(
                      target => RakuAST::ParameterTarget::Var.new(
                        :name<$original>
                      )
                    ),
                  )
                ),
                body      => RakuAST::Blockoid.new(
                  RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                      expression => RakuAST::ApplyPostfix.new(
                        operand => RakuAST::Type::Simple.new(
                          RakuAST::Name.from-identifier-parts("RakuAST","Name")
                        ),
                        postfix => RakuAST::Call::Method.new(
                          name => RakuAST::Name.from-identifier("from-identifier"),
                          args => RakuAST::ArgList.new(
                            RakuAST::Var::Lexical.new("\$original")
                          )
                        )
                      )
                    )
                  )
                )
              ),
              else      => RakuAST::Block.new(
                body => RakuAST::Blockoid.new(
                  RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                      expression => RakuAST::ApplyInfix.new(
                        left  => RakuAST::Var::Lexical.new("\$ast"),
                        infix => RakuAST::Infix.new("//"),
                        right => RakuAST::ApplyPostfix.new(
                          operand => RakuAST::Type::Simple.new(
                            RakuAST::Name.from-identifier-parts("RakuAST","Name")
                          ),
                          postfix => RakuAST::Call::Method.new(
                            name => RakuAST::Name.from-identifier("from-identifier"),
                            args => RakuAST::ArgList.new(
                              RakuAST::Var::Lexical.new("\$name")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
      !! RakuAST::StatementList.new(
           RakuAST::Statement::Expression.new(
             expression => RakuAST::ApplyInfix.new(
               left  => RakuAST::ApplyPostfix.new(
                 operand => RakuAST::Term::Self.new,
                 postfix => RakuAST::Call::Method.new(
                   name => RakuAST::Name.from-identifier("ast")
                 )
               ),
               infix => RakuAST::Infix.new("//"),
               right => RakuAST::ApplyPostfix.new(
                 operand => RakuAST::Type::Simple.new(
                   RakuAST::Name.from-identifier-parts("RakuAST","Name")
                 ),
                 postfix => RakuAST::Call::Method.new(
                   name => RakuAST::Name.from-identifier("from-identifier"),
                   args => RakuAST::ArgList.new(
                     RakuAST::ApplyPostfix.new(
                       operand => RakuAST::Term::Self.new,
                       postfix => RakuAST::Call::Method.new(
                         name => RakuAST::Name.from-identifier("Str")
                       )
                     )
                   )
                 )
               )
             )
           )
         );

    # wrap the statements into a method
    RakuAST::Statement::Expression.new(
      expression => RakuAST::Method.new(
        name  => RakuAST::Name.from-identifier($name),
        body  => RakuAST::Blockoid.new($stmts)
      )
    )
}

#- make-mapper2str -------------------------------------------------------------
# Return the str translation lookup logic, basically:
#
# method $name(str $key) {
#     my constant %mapping = @operands;
#     %mapping{$key} // $key
# }
#
# if there are any operands, otherwise:
#
# method $name(str $key) { $key }
#
sub make-mapper2str(str $name, @operands) {
    my $stmts := @operands
      ?? RakuAST::StatementList.new(
           RakuAST::Statement::Expression.new(
             expression => RakuAST::VarDeclaration::Constant.new(
               scope       => "my",
               name        => "\%mapping",
               initializer => RakuAST::Initializer::Assign.new(
                 RakuAST::ApplyListInfix.new(
                   infix    => RakuAST::Infix.new(","),
                   operands => @operands,
                 )
               )
             )
           ),
           RakuAST::Statement::Expression.new(
             expression => RakuAST::ApplyInfix.new(
               left  => RakuAST::ApplyPostfix.new(
                 operand => RakuAST::Var::Lexical.new("\%mapping"),
                 postfix => RakuAST::Postcircumfix::HashIndex.new(
                   index => RakuAST::SemiList.new(
                     RakuAST::Statement::Expression.new(
                       expression => RakuAST::Var::Lexical.new("\$key")
                     )
                   )
                 )
               ),
               infix => RakuAST::Infix.new("//"),
               right => RakuAST::Var::Lexical.new("\$key")
             )
           )
         )
      !! RakuAST::StatementList.new(
           RakuAST::Statement::Expression.new(
             expression => RakuAST::Var::Lexical.new("\$key")
           )
         );

    # Wrap the statements into a method
    RakuAST::Method.new(
      name      => RakuAST::Name.from-identifier($name),
      signature => RakuAST::Signature.new(
        parameters => (
          RakuAST::Parameter.new(
            type   => RakuAST::Type::Simple.new(
              RakuAST::Name.from-identifier("str")
            ),
            target => RakuAST::ParameterTarget::Var.new(
              :name<$key>
            )
          ),
        )
      ),
      body      => RakuAST::Blockoid.new($stmts)
    )
}

#- slangify --------------------------------------------------------------------
# Append a given key and value to the given array if the value is different
# from the key
sub accept(str $key, str $value, @array) {
    @array.append(
      RakuAST::StrLiteral.new($key),
      RakuAST::StrLiteral.new($value)
    ) if $value ne $key;
}

# Return the RakuAST of a role with the given name from the given translation
# hash to be used to create a slang.
my sub slangify($language, %hash) is export {
    my $statements := RakuAST::StatementList.new;

    # Needs 'use experimental :rakuast' in case source is generated
    $statements.add-statement: RakuAST::Statement::Use.new(
      module-name => RakuAST::Name.from-identifier("experimental"),
      argument    => RakuAST::ColonPair::True.new("rakuast")
    );

    # Run over the given hash, sorted by key
    my @adverb-pc;
    my @adverb-q;
    my @adverb-rx;
    my @core;
    my @named;
    my @enum;
    my @pragma;
    my @quote-lang;
    my @system;
    my @trait-is;
    for %hash.sort(-> $a, $b {
        $a.key.fc cmp $b.key.fc || $b.key cmp $a.key
    }) -> (:key($name), :value($string)) {

        # It's a sub / method name
        if $name.starts-with('core-') {
            accept($string, $name.substr(5), @core);
        }

        # It's a named argument
        elsif $name.starts-with('named-') {
            accept($string, $name.substr(6), @named);
        }

        # It's an "is" trait
        elsif $name.starts-with('trait-is-') {
            accept($string, $name.substr(9), @trait-is);
        }

        # It's a postfix adverb
        elsif $name.starts-with('adverb-pc-') {
            accept($string, $name.substr(10), @adverb-pc);
        }

        # It's a quote adverb
        elsif $name.starts-with('adverb-q-') {
            accept($string, $name.substr(9), @adverb-q);
        }

        # It's a regex adverb
        elsif $name.starts-with('adverb-rx-') {
            accept($string, $name.substr(10), @adverb-rx);
        }

        # It's a pragma
        elsif $name.starts-with('pragma-') {
            accept($string, $name.substr(7), @pragma);
        }

        # It's a system method
        elsif $name.starts-with('system-') {
            accept($string, $name.substr(7), @system);
        }

        # Some other core feature, add a token for it
        else {
            $statements.add-statement: RakuAST::Statement::Expression.new(
              expression => RakuAST::TokenDeclaration.new(
                name => RakuAST::Name.from-identifier(
                  $name.trans('^()' => 'cpp')  # handle bad chars
                ),
                body => RakuAST::Regex::Sequence.new(
                  RakuAST::Regex::Literal.new($string)
                )
              )
            );
        }
    }

    # Add methods for mappers
    $statements.add-statement: make-mapper2ast('core2ast',      @core     );
    $statements.add-statement: make-mapper2ast('trait-is2ast',  @trait-is );
    $statements.add-statement: make-mapper2str('adverb-pc2str', @adverb-pc);
    $statements.add-statement: make-mapper2str('adverb-q2str',  @adverb-q );
    $statements.add-statement: make-mapper2str('adverb-rx2str', @adverb-rx);
    $statements.add-statement: make-mapper2str('named2str',     @named    );
    $statements.add-statement: make-mapper2str('pragma2str',    @pragma   );
    $statements.add-statement: make-mapper2str('system2str',    @system   );

    # Wrap the whole thing up in a role with the given name and return it
    RakuAST::Role.new(
      name => RakuAST::Name.from-identifier-parts('L10N',$language),
      body => RakuAST::RoleBody.new(
        body => RakuAST::Blockoid.new($statements)
      )
    )
}

#- deparsify -------------------------------------------------------------------
# Return the RakuAST of a role with the given name from the given translation
# hash to be used to create a slang.
my sub deparsify($language, %hash) is export {
    my $statements := RakuAST::StatementList.new;

    # Run over the given hash, sorted by key
    my @operands = %hash.sort(-> $a, $b {
        $a.key.fc cmp $b.key.fc || $b.key cmp $a.key
    }).map: {
        (RakuAST::StrLiteral.new(.key), RakuAST::StrLiteral.new(.value)).Slip
          unless .key.ends-with('-' ~ .value)
    }

    # Found something to lookup in at runtime
    my $body := do if @operands {

        # Set up the constant hash
        $statements.add-statement: RakuAST::Statement::Expression.new(
          expression => RakuAST::VarDeclaration::Constant.new(
            scope       => "my",
            name        => "\%xlation",
            initializer => RakuAST::Initializer::Assign.new(
              RakuAST::ApplyListInfix.new(
                infix    => RakuAST::Infix.new(","),
                operands => @operands,
              )
            )
          )
        );

        # %translation{"$prefix-$key"} // $key
        RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new(
              left  => RakuAST::ApplyPostfix.new(
                operand => RakuAST::Var::Lexical.new("\%xlation"),
                postfix => RakuAST::Postcircumfix::HashIndex.new(
                  index => RakuAST::SemiList.new(
                    RakuAST::Statement::Expression.new(
                      expression => RakuAST::QuotedString.new(
                        segments   => (
                          RakuAST::Var::Lexical.new("\$prefix"),
                          RakuAST::StrLiteral.new("-"),
                          RakuAST::Var::Lexical.new("\$key"),
                        )
                      )
                    )
                  )
                )
              ),
              infix => RakuAST::Infix.new("//"),
              right => RakuAST::Var::Lexical.new("\$key")
            )
          )
        )
    }

    # Nothing to look up in, so just return $key
    else {
        RakuAST::Statement::Expression.new(
          expression => RakuAST::Var::Lexical.new("\$key")
        )
    }

    # Add method doing the actual mapping, basically:
    #
    # my role $language is export {
    #     my method xsyn(str $prefix, str $key) {
    #         $body
    #     }
    # }
    #
    $statements.add-statement: RakuAST::Statement::Expression.new(
      expression => RakuAST::Role.new(
        name => RakuAST::Name.from-identifier-parts(
                  'RakuAST','Deparse','L10N',$language
                ),
        body => RakuAST::RoleBody.new(
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::Method.new(
                  name      => RakuAST::Name.from-identifier("xsyn"),
                  signature => RakuAST::Signature.new(
                    parameters => (
                      RakuAST::Parameter.new(
                        type   => RakuAST::Type::Simple.new(
                          RakuAST::Name.from-identifier("str")
                        ),
                        target => RakuAST::ParameterTarget::Var.new(
                          :name<$prefix>
                        )
                      ),
                      RakuAST::Parameter.new(
                        type   => RakuAST::Type::Simple.new(
                          RakuAST::Name.from-identifier("str")
                        ),
                        target => RakuAST::ParameterTarget::Var.new(
                          :name<$key>
                        )
                      ),
                    )
                  ),
                  body      => RakuAST::Blockoid.new(
                    RakuAST::StatementList.new($body)
                  )
                )
              )
            )
          )
        )
      )
    );

    $statements
}

#- update-module ---------------------------------------------------------------
# Values for generating source files
my $generator := $*PROGRAM-NAME;
my $generated := DateTime.now.gist.subst(/\.\d+/,'');
my $start     := '#- start of generated part of localization';
my $end       := '#- end of generated part of localization';

# For all available localizations
my sub update-module(
   IO() $io,
  Str:D $language = $io.basename,
   IO() $root     = "lib"
) is export {

    # Create translation hash
    my %translation := read-hash($io, :core);

    # Create the slang and slangification
    my $slang  := slangify($language, %translation);
    my $source := $slang.DEPARSE ~ Q:to/CODE/.subst('#LANGUAGE#',$language,:g);


# The EXPORT sub that actually does the slanging
my sub EXPORT($dontslang?) {
    unless $dontslang {
        my $LANG := $*LANG;
        $LANG.define_slang('MAIN',
          $LANG.slang_grammar('MAIN').^mixin(L10N::#LANGUAGE#)
        );
    }

    BEGIN Map.new
}
CODE
    write-file
      $root.add(<<L10N "$language.rakumod">>),
      $source,
      Q:to/DEFAULT/;
# This file contains the ……… Slang of the Raku Programming Language

#- start of generated part of localization
#- end of generated part of localization

# vim: expandtab shiftwidth=4
DEFAULT

    # Create the role for mixing in deparsing
    my $deparser := deparsify($language, %translation);
    write-file
      $root.add(<<RakuAST Deparse L10N "$language.rakumod">>),
      $deparser.DEPARSE,
      Q:to/DEFAULT/;
# This file contains the ……… deparsing logic for the Raku
# Programming Language.

#- start of generated part of localization
#- end of generated part of localization

# vim: expandtab shiftwidth=4
DEFAULT

    # Take the first file in the "bin" directory.  If there is one, take
    # that as the target for the localized executor.  After that, make
    # sure it is executable
    my $bin := $root.sibling("bin");
    if $bin.d && $bin.dir.head -> $executor {
        write-file
          $executor,
          Q:to/LOCALIZED/.subst("#LANGUAGE#",$language),
%*ENV<RAKUDO_RAKUAST> = 1;
%*ENV<RAKUDO_OPT>     = '-ML10N::#LANGUAGE#';
LOCALIZED
          Q:to/DEFAULT/;
#!/usr/bin/env raku

# Executor for the ___ localization of the Raku Programming Language

#- start of generated part of localization
#- end of generated part of localization

my $proc := run $*EXECUTABLE, @*ARGS;
exit $proc.exitcode;

# vim: expandtab shiftwidth=4
DEFAULT
        $executor.chmod(0o755);
    }
}

#- write-file ------------------------------------------------------------------
sub write-file(IO() $io, Str:D $src, Str:D $default) {

    # slurp the whole file and set up writing to it
    mkdir $io.parent;
    my @lines = ($io.e && $io.s ?? $io !! $default).lines;

    # for all the lines in the source that don't need special handling
    my $*OUT = $io.open(:w);
    while @lines {
        my $line := @lines.shift;

        # nothing to do yet
        unless $line.starts-with($start) {
            say $line;
            next;
        }

        say "$start ------------------------------------";
        say "#- Generated on $generated by $generator";
        say "#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE";
        say "";

        # skip the old version of the code
        while @lines {
            last if @lines.shift.starts-with($end);
        }

        # Insert the actual logic
        print $src;

        # we're done for this role
        say "";
        say "#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE";
        say "$end --------------------------------------";
    }

    # close the file properly
    $*OUT.close;
}

=finish
# The "null" translation of features of the Raku Programming Language, and
# as such only has meaning as a template for other translations.
#
# Please note that as the Raku Programming Language evolves, further
# elements may be added, so any translations will probably need to be
# updated by then as well.
#
# The keys consist of 2 parts: before the first hyphen is a type indication
# of the syntax, followed by the name of the feature in the syntax.  The
# following type indications exist so far:
#
#   adverb-pc    core adverbs on postcircumfixes (:exists, :delete, etc.)
#   adverb-q     quote language adverbs ("to","ww","v", etc.)
#   adverb-rx    core adverbs on regexes (:i, :m, :ignorecase, etc.)
#   block        syntax involving a block
#   core         sub/method names that are part of the Raku core
#   constraint   related to (ad-hoc) constraints
#   enum         core enums (True, False, Less, More, etc.)
#   infix        infix operators consisting of alphanumeric characters
#   meta         meta-operator prefixes ('R','X','Z')
#   modifier     statement modifier syntax
#   multi        types of multi syntax
#   named        named arguments to methods/subs (:k,:p,:absolute, etc.)
#   package      package declarators
#   pragma       compile-time pragmas ("lib","precompilation", etc.)
#   prefix       prefix operators consisting of alphanumeric characters
#   phaser       types of phasers ("BEGIN","END","CHECK", etc.)
#   routine      types of named code blocks
#   quote-lang   quote language markers ("q","Q","qq", etc.)
#   scope        types of scope ("my","our","state", etc.)
#   stmt-prefix  statement prefixes ("do","eager","lazy", etc.)
#   system       methods called by the system (TWEAK,BUILD,ACCEPTS etc.)
#   term         terms ("time","now","self",etc.)
#   trait        types of traits ("is","does","returns", etc.)
#   typer        type constructors ("enum","subset")
#   use          use related ("use","no","require", etc.)
#
# Each line that does not start with a '#", is considered to contain two
# words: the first is the key known to the localization logic of the Raku
# Programming Language.  The second word is supposed to be the translation
# in the given language.
#
# Please note that the second word currently *must* adhere to Raku identifier
# rules, so that they must start with a \w character (which includes the
# underscore), and may have a hyphen as long as it is not the last character
# if the word, and as long as there are no two consecutive hyphens.

# KEY             TRANSLATION
adverb-pc-delete  delete
adverb-pc-exists  exists
adverb-pc-k       k
adverb-pc-kv      kv
adverb-pc-p       p
adverb-pc-v       v

# KEY                TRANSLATION
adverb-q-a           a
adverb-q-array       array
adverb-q-b           b
adverb-q-backslash   backslash
adverb-q-c           c
adverb-q-closure     closure
adverb-q-double      double
adverb-q-exec        exec
adverb-q-f           f
adverb-q-format      format
adverb-q-function    function
adverb-q-h           h
adverb-q-hash        hash
adverb-q-heredoc     heredoc
adverb-q-o           o
adverb-q-q           q
adverb-q-qq          qq
adverb-q-quotewords  quotewords
adverb-q-s           s
adverb-q-scalar      scalar
adverb-q-single      single
adverb-q-to          to
adverb-q-v           v
adverb-q-val         val
adverb-q-w           w
adverb-q-words       words
adverb-q-ww          ww
adverb-q-x           x

# KEY                 TRANSLATION
adverb-rx-c           c
adverb-rx-continue    continue
adverb-rx-ex          ex
adverb-rx-exhaustive  exhaustive
adverb-rx-g           g
adverb-rx-global      global
adverb-rx-i           i
adverb-rx-ignorecase  ignorecase
adverb-rx-ignoremark  ignoremark
adverb-rx-ii          ii
adverb-rx-m           m
adverb-rx-mm          mm
adverb-rx-nd          nd
adverb-rx-nth         nth
adverb-rx-ov          ov
adverb-rx-overlap     overlap
adverb-rx-p           p
adverb-rx-pos         pos
adverb-rx-r           r
adverb-rx-ratchet     ratchet
adverb-rx-rd          rd
adverb-rx-s           s
adverb-rx-samecase    samecase
adverb-rx-samemark    samemark
adverb-rx-samespace   samespace
adverb-rx-sigspace    sigspace
adverb-rx-ss          ss
adverb-rx-st          st
adverb-rx-to          to
adverb-rx-x           x

# KEY           TRANSLATION
block-default   default
block-else      else
block-elsif     elsif
block-for       for
block-given     given
block-if        if
block-loop      loop
block-orwith    orwith
block-repeat    repeat
block-unless    unless
block-until     until
block-when      when
block-whenever  whenever
block-while     while
block-with      with
block-without   without

# KEY             TRANSLATION
constraint-where  where

# KEY                  TRANSLATION
core-abs               abs
core-all               all
core-antipairs         antipairs
core-any               any
core-append            append
core-ast               ast
core-atomic-add-fetch  atomic-add-fetch
core-atomic-assign     atomic-assign
core-atomic-dec-fetch  atomic-dec-fetch
core-atomic-fetch      atomic-fetch
core-atomic-fetch-add  atomic-fetch-add
core-atomic-fetch-dec  atomic-fetch-dec
core-atomic-fetch-inc  atomic-fetch-inc
core-atomic-fetch-sub  atomic-fetch-sub
core-atomic-inc-fetch  atomic-inc-fetch
core-atomic-sub-fetch  atomic-sub-fetch
core-await             await

# KEY          TRANSLATION
core-bag       bag
core-bail-out  bail-out
core-bless     bless

# KEY              TRANSLATION
core-callframe     callframe
core-callsame      callsame
core-callwith      callwith
core-can-ok        can-ok
core-cas           cas
core-categorize    categorize
core-ceiling       ceiling
core-chars         chars
core-chdir         chdir
core-chmod         chmod
core-chomp         chomp
core-chop          chop
core-chown         chown
core-chr           chr
core-chrs          chrs
core-classify      classify
core-close         close
core-cmp-ok        cmp-ok
core-codes         codes
core-comb          comb
core-combinations  combinations
core-conj          conj
core-contains      contains
core-cross         cross

# KEY         TRANSLATION
core-decode   decode
core-deepmap  deepmap
core-defined  defined
core-diag     diag
core-die      die
core-dies-ok  dies-ok
core-dir      dir
core-does-ok  does-ok
core-done     done
core-duckmap  duckmap

# KEY               TRANSLATION
core-elems          elems
core-emit           emit
core-encode         encode
core-end            end
core-ends-with      ends-with
core-eval-dies-ok   eval-dies-ok
core-eval-lives-ok  eval-lives-ok
core-exit           exit
core-exp            exp
core-expmod         expmod

# KEY              TRANSLATION
core-fail          fail
core-fails-like    fails-like
core-fc            fc
core-first         first
core-flat          flat
core-flip          flip
core-floor         floor
core-flunk         flunk
core-fmt           fmt
core-full-barrier  full-barrier

# KEY      TRANSLATION
core-get   get
core-getc  getc
core-gist  gist
core-grep  grep

# KEY      TRANSLATION
core-hash  hash
core-head  head

# KEY           TRANSLATION
core-indent     indent
core-index      index
core-indices    indices
core-indir      indir
core-is         is
core-is-approx  is-approx
core-is-deeply  is-deeply
core-is-prime   is-prime
core-isa-ok     isa-ok
core-isnt       isnt
core-item       item

# KEY      TRANSLATION
core-join  join

# KEY      TRANSLATION
core-key   key
core-keys  keys
core-kv    kv

# KEY          TRANSLATION
core-last      last
core-lastcall  lastcall
core-lc        lc
core-like      like
core-lines     lines
core-link      link
core-list      list
core-lives-ok  lives-ok
core-lsb       lsb

# KEY        TRANSLATION
core-make    make
core-map     map
core-match   match
core-max     max
core-min     min
core-minmax  minmax
core-mix     mix
core-mkdir   mkdir
core-move    move
core-msb     msb

# KEY            TRANSLATION
core-new         new
core-next        next
core-nextcallee  nextcallee
core-nextsame    nextsame
core-nextwith    nextwith
core-nok         nok
core-none        none
core-not         not
core-note        note

# KEY      TRANSLATION
core-ok    ok
core-one   one
core-open  open
core-ord   ord
core-ords  ords

# KEY              TRANSLATION
core-pair          pair
core-pairs         pairs
core-parse-base    parse-base
core-pass          pass
core-permutations  permutations
core-pick          pick
core-plan          plan
core-pop           pop
core-prepend       prepend
core-print         print
core-printf        printf
core-proceed       proceed
core-prompt        prompt
core-push          push
core-put           put

# KEY            TRANSLATION
core-rand        rand
core-redo        redo
core-reduce      reduce
core-repeated    repeated
core-repl        repl
core-return      return
core-return-rw   return-rw
core-reverse     reverse
core-rindex      rindex
core-rmdir       rmdir
core-roll        roll
core-roots       roots
core-rotate      rotate
core-round       round
core-roundrobin  roundrobin
core-run         run

# KEY             TRANSLATION
core-samecase     samecase
core-samemark     samemark
core-samewith     samewith
core-say          say
core-set          set
core-shell        shell
core-shift        shift
core-sign         sign
core-signal       signal
core-skip         skip
core-skip-rest    skip-rest
core-sleep        sleep
core-sleep-timer  sleep-timer
core-sleep-until  sleep-until
core-slip         slip
core-slurp        slurp
core-snip         snip
core-snitch       snitch
core-so           so
core-sort         sort
core-splice       splice
core-split        split
core-sprintf      sprintf
core-spurt        spurt
core-sqrt         sqrt
core-squish       squish
core-srand        srand
core-starts-with  starts-with
core-subbuf       subbuf
core-subbuf-rw    subbuf-rw
core-subst        subst
core-substr       substr
core-substr-eq    substr-eq
core-substr-rw    substr-rw
core-subtest      subtest
core-succeed      succeed
core-sum          sum
core-symlink      symlink

# KEY               TRANSLATION
core-tail           tail
core-take           take
core-take-rw        take-rw
core-tc             tc
core-tclc           tclc
core-temp           temp
core-throws-like    throws-like
core-todo           todo
core-trans          trans
core-trim           trim
core-trim-leading   trim-leading
core-trim-trailing  trim-trailing
core-truncate       truncate

# KEY          TRANSLATION
core-uc        uc
core-unimatch  unimatch
core-uniname   uniname
core-uninames  uninames
core-uniparse  uniparse
core-uniprop   uniprop
core-uniprops  uniprops
core-unique    unique
core-unival    unival
core-univals   univals
core-unlike    unlike
core-unlink    unlink
core-unpolar   unpolar
core-unshift   unshift
core-use-ok    use-ok

# KEY        TRANSLATION
core-val     val
core-value   value
core-values  values

# KEY          TRANSLATION
core-warn      warn
core-wordcase  wordcase
core-words     words

# KEY     TRANSLATION
core-zip  zip

# KEY                   TRANSLATION
enum-BigEndian          BigEndian
enum-Broken             Broken
enum-False              False
enum-FileChanged        FileChanged
enum-FileRenamed        FileRenamed
enum-Kept               Kept
enum-Less               Less
enum-LittleEndian       LittleEndian
enum-More               More
enum-NativeEndian       NativeEndian
enum-Planned            Planned
enum-Same               Same
enum-SeekFromBeginning  SeekFromBeginning
enum-SeekFromCurrent    SeekFromCurrent
enum-SeekFromEnd        SeekFromEnd
enum-True               True

# KEY             TRANSLATION
infix-(cont)      (cont)
infix-(elem)      (elem)
infix-^ff         ^ff
infix-^ff^        ^ff^
infix-^fff        ^fff
infix-^fff^       ^fff^
infix-after       after
infix-and         and
infix-andthen     andthen
infix-before      before
infix-but         but
infix-cmp         cmp
infix-coll        coll
infix-div         div
infix-does        does
infix-eq          eq
infix-ff          ff
infix-ff^         ff^
infix-fff         fff
infix-fff^        fff^
infix-gcd         gcd
infix-ge          ge
infix-gt          gt
infix-lcm         lcm
infix-le          le
infix-leg         leg
infix-lt          lt
infix-max         max
infix-min         min
infix-minmax      minmax
infix-mod         mod
infix-ne          ne
infix-notandthen  notandthen
infix-o           o
infix-or          or
infix-orelse      orelse
infix-unicmp      unicmp
infix-x           x
infix-X           X
infix-xx          xx
infix-Z           Z

# KEY   TRANSLATION
meta-R  R
meta-X  X
meta-Z  Z

# KEY             TRANSLATION
modifier-for      for
modifier-given    given
modifier-if       if
modifier-unless   unless
modifier-until    until
modifier-when     when
modifier-while    while
modifier-with     with
modifier-without  without

# KEY        TRANSLATION
multi-multi  multi
multi-only   only
multi-proto  proto

# KEY           TRANSLATION
named-absolute  absolute
named-actions   actions
named-api       api
named-append    append
named-arg0      arg0
named-args      args
named-as        as
named-at        at
named-auth      auth

# KEY            TRANSLATION
named-basename   basename
named-batch      batch
named-bin        bin
named-bleed      bleed
named-broadcast  broadcast

# KEY             TRANSLATION
named-catch       catch
named-check       check
named-chomp       chomp
named-close       close
named-command     command
named-complement  complement
named-completely  completely
named-continue    continue
named-control     control
named-count       count
named-create      create
named-createonly  createonly
named-cwd         cwd
named-CWD         CWD

# KEY           TRANSLATION
named-datagram  datagram
named-date      date
named-day       day
named-degree    degree
named-delete    delete
named-dirname   dirname

# KEY             TRANSLATION
named-elems       elems
named-emit-timed  emit-timed
named-enc         enc
named-encoding    encoding
named-end         end
named-err         err
named-every       every
named-ex          ex
named-exclusive   exclusive
named-exhaustive  exhaustive
named-expires     expires

# KEY            TRANSLATION
named-family     family
named-filename   filename
named-filter     filter
named-formatter  formatter

# KEY         TRANSLATION
named-g       g
named-gid     gid
named-global  global

# KEY       TRANSLATION
named-host  host
named-hour  hour

# KEY             TRANSLATION
named-i           i
named-ignorecase  ignorecase
named-ignoremark  ignoremark
named-ii          ii
named-in          in
named-into        into

# KEY         TRANSLATION
named-joiner  joiner

# KEY      TRANSLATION
named-k    k
named-key  key
named-kv   kv

# KEY            TRANSLATION
named-listen     listen
named-localhost  localhost
named-localport  localport

# KEY         TRANSLATION
named-match   match
named-merge   merge
named-minute  minute
named-mm      mm
named-mode    mode
named-month   month

# KEY               TRANSLATION
named-name          name
named-nd            nd
named-nl            nl
named-nl-in         nl-in
named-nl-out        nl-out
named-non-blocking  non-blocking
named-nth           nth

# KEY             TRANSLATION
named-off         off
named-out         out
named-out-buffer  out-buffer
named-ov          ov
named-overlap     overlap

# KEY          TRANSLATION
named-p        p
named-partial  partial
named-parts    parts
named-port     port
named-pos      pos
named-primary  primary

# KEY             TRANSLATION
named-quaternary  quaternary

# KEY              TRANSLATION
named-r            r
named-rd           rd
named-real         real
named-replacement  replacement
named-rule         rule
named-rw           rw
named-rx           rx

# KEY              TRANSLATION
named-samecase     samecase
named-samespace    samespace
named-scheduler    scheduler
named-second       second
named-secondary    secondary
named-seconds      seconds
named-shared       shared
named-size         size
named-slip         slip
named-SPEC         SPEC
named-squash       squash
named-ss           ss
named-st           st
named-status       status
named-strict       strict
named-subscript    subscript
named-superscript  superscript

# KEY               TRANSLATION
named-tertiary      tertiary
named-test          test
named-th            th
named-times         times
named-timezone      timezone
named-translate-nl  translate-nl
named-truncate      truncate

# KEY         TRANSLATION
named-uid     uid
named-update  update

# KEY          TRANSLATION
named-v        v
named-value    value
named-vent-at  vent-at
named-ver      ver
named-volume   volume

# KEY                    TRANSLATION
named-w                  w
named-where              where
named-win-verbatim-args  win-verbatim-args

# KEY    TRANSLATION
named-x  x

# KEY       TRANSLATION
named-year  year

# KEY            TRANSLATION
package-class    class
package-grammar  grammar
package-module   module
package-package  package
package-role     role

# KEY           TRANSLATION
phaser-BEGIN    BEGIN
phaser-CATCH    CATCH
phaser-CHECK    CHECK
phaser-CLOSE    CLOSE
phaser-CONTROL  CONTROL
phaser-DOC      DOC
phaser-END      END
phaser-ENTER    ENTER
phaser-FIRST    FIRST
phaser-INIT     INIT
phaser-KEEP     KEEP
phaser-LAST     LAST
phaser-LEAVE    LEAVE
phaser-NEXT     NEXT
phaser-POST     POST
phaser-PRE      PRE
phaser-QUIT     QUIT
phaser-UNDO     UNDO

# KEY                      TRANSLATION
pragma-dynamic-scope       dynamic-scope
pragma-fatal               fatal
pragma-isms                isms
pragma-lib                 lib
pragma-MONKEY              MONKEY
pragma-MONKEY-GUTS         MONKEY-GUTS
pragma-MONKEY-SEE-NO-EVAL  MONKEY-SEE-NO-EVAL
pragma-MONKEY-TYPING       MONKEY-TYPING
pragma-nqp                 nqp
pragma-precompilation      precompilation
pragma-soft                soft
pragma-strict              strict
pragma-trace               trace
pragma-variables           variables
pragma-worries             worries

# KEY       TRANSLATION
prefix-not  not
prefix-so   so

# KEY          TRANSLATION
quote-lang-m   m
quote-lang-ms  ms
quote-lang-q   q
quote-lang-Q   Q
quote-lang-qq  qq
quote-lang-rx  rx
quote-lang-s   s
quote-lang-S   S
quote-lang-ss  ss
quote-lang-Ss  Ss

# KEY              TRANSLATION
routine-method     method
routine-regex      regex
routine-rule       rule
routine-sub        sub
routine-submethod  submethod
routine-token      token

# KEY           TRANSLATION
scope-anon      anon
scope-augment   augment
scope-constant  constant
scope-has       has
scope-HAS       HAS
scope-my        my
scope-our       our
scope-state     state
scope-unit      unit

# KEY                TRANSLATION
stmt-prefix-also     also
stmt-prefix-do       do
stmt-prefix-eager    eager
stmt-prefix-gather   gather
stmt-prefix-hyper    hyper
stmt-prefix-lazy     lazy
stmt-prefix-quietly  quietly
stmt-prefix-race     race
stmt-prefix-react    react
stmt-prefix-sink     sink
stmt-prefix-start    start
stmt-prefix-supply   supply
stmt-prefix-try      try

# KEY               TRANSLATION
system-ACCEPTS      ACCEPTS
system-BUILD        BUILD
system-CALL-ME      CALL-ME
system-COERCE       COERCE
system-FALLBACK     FALLBACK
system-MAIN         MAIN
system-TWEAK        TWEAK
system-UPGRADE-RAT  UPGRADE-RAT

# KEY         TRANSLATION
term-nano     nano
term-now      now
term-pi       pi
term-rand     rand
term-self     self
term-tau      tau
term-time     time

# KEY                           TRANSLATION
trait-is-built                  built
trait-is-copy                   copy
trait-is-default                default
trait-is-DEPRECATED             DEPRECATED
trait-is-equiv                  equiv
trait-is-export                 export
trait-is-hidden-from-backtrace  hidden-from-backtrace
trait-is-hidden-from-USAGE      hidden-from-USAGE
trait-is-implementation-detail  implementation-detail
trait-is-looser                 looser
trait-is-nodal                  nodal
trait-is-pure                   pure
trait-is-raw                    raw
trait-is-rw                     rw
trait-is-test-assertion         test-assertion
trait-is-tighter                tighter

# KEY             TRANSLATION
traitmod-does     does
traitmod-handles  handles
traitmod-hides    hides
traitmod-is       is
traitmod-of       of
traitmod-returns  returns
traitmod-trusts   trusts

# KEY         TRANSLATION
typer-enum    enum
typer-subset  subset

# KEY        TRANSLATION
use-import   import
use-need     need
use-no       no
use-require  require
use-use      use

# vim: expandtab shiftwidth=4
