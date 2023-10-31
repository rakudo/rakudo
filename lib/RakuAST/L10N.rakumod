# The RakuAST::L10N module provides takes a name and hash, and produces the
# RakuAST tree for a role that can be mixed into the main grammar
# to support that localization of the Raku Programming Language.
#
# use RakuAST::L10N;
# my %xlation = …;  # create hash with translation
# my $slang    = slangify($lang, %xlation);   # role for grammar mixin
# my $deparser = deparsify($lang, %xlation);  # role for deparsing

# Needed for now
use experimental :rakuast;

# Known groups of translation
my constant %known-groups = <
  adverb-pc adverb-q adverb-rx block constraint core infix meta modifier
  multi named package phaser pragma prefix quote-lang routine scope
  stmt-prefix term traitmod trait-is typer use
>.map({ $_ => 1 });
my constant %sub-groups = <core named>.map({ $_ => 1 });

# Produce all words on non-commented lines of given IO as a Slip
sub io2words(IO::Path:D $io) {
    $io.lines.map: { .words.Slip unless .starts-with("#") }
}

# Root of localization files
my constant $root = "tools/templates/L10N";

# IOs of valid localization files, add :core to include CORE
sub localization-files($path = $root, :$core) is export {
    dir("tools/templates/L10N").grep({
        my str $language = .basename;
        !(
          ($language eq 'CORE' and !$core)
            || $language.ends-with('.md')  # any translator documentation
            || $language.starts-with(".")  # ignore editor temp files
        )
    }).sort(*.basename)
}

# Set up core settings
BEGIN my @core = io2words("$root/CORE".IO);

# Read translation hash from given file (as IO object)
sub read-hash(IO::Path:D $io, :$core) is export {
    $core
      ?? %(flat @core, io2words($io))
      !! %(io2words($io))
}

# Write out the localization of the given hash to the given file (as IO object)
sub write-hash(IO::Path:D $io, %mapping) is export {

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
             expression => RakuAST::VarDeclaration::Simple.new(
               scope       => "my",
               sigil       => "\%",
               desigilname => RakuAST::Name.from-identifier("mapping"),
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
                      target => RakuAST::ParameterTarget::Var.new("\$original")
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

sub make-mapper2str(str $name, @operands) {
    my $stmts := @operands
      ?? RakuAST::StatementList.new(
           RakuAST::Statement::Expression.new(
             expression => RakuAST::VarDeclaration::Simple.new(
               scope       => "my",
               sigil       => "\%",
               desigilname => RakuAST::Name.from-identifier("mapping"),
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
            target => RakuAST::ParameterTarget::Var.new("\$key")
          ),
        )
      ),
      body      => RakuAST::Blockoid.new($stmts)
    )
}

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
    my @pragma;
    my @quote-lang;
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

    # Wrap the whole thing up in a role with the given name and return it
    RakuAST::Role.new(
      name => RakuAST::Name.from-identifier-parts('L10N',$language),
      body => RakuAST::Block.new(
        body => RakuAST::Blockoid.new($statements)
      )
    )
}

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
          expression => RakuAST::VarDeclaration::Simple.new(
            scope       => "my",
            sigil       => "\%",
            desigilname => RakuAST::Name.from-identifier("xlation"),
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
    # my role NL is export {
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
        body => RakuAST::Block.new(
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
                        target => RakuAST::ParameterTarget::Var.new("\$prefix")
                      ),
                      RakuAST::Parameter.new(
                        type   => RakuAST::Type::Simple.new(
                          RakuAST::Name.from-identifier("str")
                        ),
                        target => RakuAST::ParameterTarget::Var.new("\$key")
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

# vim: expandtab shiftwidth=4
