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

# Return the AST for translation lookup logic, basically:
#
# method $name {
#     my constant %mapping = @operands;
#     my $ast := self.ast;
#     if %mapping{$ast.simple-identifier} -> $original {
#         RakuAST::Name.from-identifier($original)
#     }
#     else {
#         $ast
#     }
# }
#
# if there are any operands, otherwise:
#
# method $name { self.ast }
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
           RakuAST::Statement::If.new(
             condition => RakuAST::ApplyPostfix.new(
               operand => RakuAST::Var::Lexical.new("\%mapping"),
               postfix => RakuAST::Postcircumfix::HashIndex.new(
                 index => RakuAST::SemiList.new(
                   RakuAST::Statement::Expression.new(
                     expression => RakuAST::ApplyPostfix.new(
                       operand => RakuAST::Var::Lexical.new("\$ast"),
                       postfix => RakuAST::Call::Method.new(
                         name => RakuAST::Name.from-identifier("simple-identifier")
                       )
                     )
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
                     expression => RakuAST::Var::Lexical.new("\$ast")
                   )
                 )
               )
             )
           )
         )
      !! RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyPostfix.new(
              operand => RakuAST::Term::Self.new,
              postfix => RakuAST::Call::Method.new(
                name => RakuAST::Name.from-identifier("ast")
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
# method $name(str $adverb) {
#     my constant %mapping = @operands;
#     %mapping{$adverb} // $adverb
# }
#
# if there are any operands, otherwise:
#
# method $name(str $adverb) { $adverb }

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
                       expression => RakuAST::Var::Lexical.new("\$adverb")
                     )
                   )
                 )
               ),
               infix => RakuAST::Infix.new("//"),
               right => RakuAST::Var::Lexical.new("\$adverb")
             )
           )
         )
      !! RakuAST::StatementList.new(
           RakuAST::Statement::Expression.new(
             expression => RakuAST::Var::Lexical.new("\$adverb")
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
            target => RakuAST::ParameterTarget::Var.new("\$adverb")
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
    my @core;
    my @trait-is;
    my @adverb-pc;
    my @adverb-rx;
    for %hash.sort(*.key.fc) -> (:key($name), :value($string)) {

        # It's a sub / method name
        if $name.starts-with('core-') {
            accept($string, $name.substr(5), @core);
        }

        # It's an "is" trait
        elsif $name.starts-with('trait-is-') {
            accept($string, $name.substr(9), @trait-is);
        }

        # It's a postfix adverb
        elsif $name.starts-with('adverb-pc-') {
            accept($string, $name.substr(10), @adverb-pc);
        }

        # It's a regex adverb
        elsif $name.starts-with('adverb-rx-') {
            accept($string, $name.substr(10), @adverb-rx);
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
    $statements.add-statement: make-mapper2ast('core2ast',      @core    );
    $statements.add-statement: make-mapper2ast('trait-is2ast',  @trait-is);
    $statements.add-statement: make-mapper2str('adverb-pc2str', @adverb-pc);
    $statements.add-statement: make-mapper2str('adverb-rx2str', @adverb-rx);

    # Wrap the whole thing up in a role with the given name and return it
    RakuAST::Package.new(
      declarator => "role",
      name       => RakuAST::Name.from-identifier-parts('L10N',$language),
      body       => RakuAST::Block.new(
        body => RakuAST::Blockoid.new($statements)
      )
    )
}

# Return the RakuAST of a role with the given name from the given translation
# hash to be used to create a slang.
my sub deparsify($language, %hash) is export {
    my $statements := RakuAST::StatementList.new;

    # Run over the given hash, sorted by key
    my @operands = %hash.sort(*.key.fc).map: {
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
    # my role NL is export {
    #     my method xsyn(str $prefix, str $key) {
    #         $body
    #     }
    # }
    #
    $statements.add-statement: RakuAST::Statement::Expression.new(
      expression => RakuAST::Package.new(
        declarator => "role",
        name       => RakuAST::Name.from-identifier-parts(
                        'RakuAST','Deparse','L10N',$language
                      ),
        body       => RakuAST::Block.new(
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
