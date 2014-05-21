use NQPP6QRegex;
use NQPP5QRegex;
use Perl6::Pod;
use Perl6::Ops;
use QRegex;
use QAST;

role STDActions {
    method quibble($/) {
        make $<nibble>.ast;
    }
    
    method trim_heredoc($doc, $stop, $origast) {
        $origast.pop();
        $origast.pop();

        my str $ws := $stop.MATCH<ws>.Str;
        my int $actualchars := nqp::chars($ws);
        my int $indent := -$actualchars;

        my int $tabstop := $*W.find_symbol(['$?TABSTOP']);
        my int $checkidx := 0;
        while $checkidx < $actualchars {
            if nqp::substr($ws, $checkidx, 1) eq "\t" {
                $indent := $indent - ($tabstop - 1);
            }
            $checkidx := $checkidx + 1;
        }

        my $docast := $doc.MATCH.ast;
        if $docast.has_compile_time_value {
            my $dedented := nqp::unbox_s($docast.compile_time_value.indent($indent));
            $origast.push($*W.add_string_constant($dedented));
        }
        else {
            # we need to remove spaces from the beginnings of only textual lines,
            # so we have to track after each concatenation if the spaces at the
            # beginning of our chunk belong to a fresh line or come after an
            # interpolation or something
            my $in-fresh-line := 1;

            sub descend($node) {
                if nqp::istype($node, QAST::Want) {
                    if +@($node) == 3 && $node[1] eq "Ss" {
                        my $strval := $node[0].compile_time_value;
                        if !$in-fresh-line {
                            if $strval ~~ /\n/ {
                                my $strbox := nqp::box_s(nqp::x(" ", -$indent) ~ nqp::unbox_s($strval), $*W.find_symbol(["Str"]));
                                $strval := nqp::unbox_s($strbox.indent($indent));
                                $in-fresh-line := 1;
                                return $*W.add_string_constant($strval);
                            }
                        } else {
                            $strval := nqp::unbox_s($strval.indent($indent));
                            return $*W.add_string_constant($strval);
                        }
                    }
                } elsif nqp::istype($node, QAST::Op) && $node.op eq 'call' && $node.name eq '&infix:<~>' {
                    my @results;
                    # since we have the $in-fresh-line state, we need to traverse
                    # and replace the child nodes in order
                    for @($node) {
                        nqp::push(@results, descend($node.shift))
                    }
                    for @results {
                        nqp::push($node, $_)
                    }
                    return $node;
                }
                $in-fresh-line := 0;
                return $node
            }

            $origast.push(descend($docast))
        }
        $origast;
    }
}

class Perl6::Actions is HLL::Actions does STDActions {
    our @MAX_PERL_VERSION;

    our $FORBID_PIR;
    our $STATEMENT_PRINT;

    INIT {
        # If, e.g., we support Perl up to v6.1.2, set
        # @MAX_PERL_VERSION to [6, 1, 2].
        @MAX_PERL_VERSION[0] := 6;

        $FORBID_PIR := 0;
        $STATEMENT_PRINT := 0;
    }

    sub sink($past) {
        my $name := $past.unique('sink');
        QAST::Want.new(
            $past,
            'v',
            QAST::Op.new(
                :op('locallifetime'),
                QAST::Stmts.new(
                    QAST::Op.new(:op<bind>,
                        QAST::Var.new(:$name, :scope<local>, :decl<var>),
                        $past,
                    ),
                    QAST::Op.new(:op<if>,
                        QAST::Op.new(:op<if>,
                            QAST::Op.new(:op<isconcrete>,
                                QAST::Var.new(:$name, :scope<local>),
                            ),
                            QAST::Op.new(:op<can>,
                                QAST::Var.new(:$name, :scope<local>),
                                QAST::SVal.new(:value('sink')),
                            )
                        ),
                        QAST::Op.new(:op<callmethod>, :name<sink>,
                            QAST::Var.new(:$name, :scope<local>),
                        ),
                    ),
                ),
                $name,
            ),
        );
    }
    my %sinkable := nqp::hash(
            'call',         1,
            'callmethod',   1,
            'while',        1,
            'until',        1,
            'repeat_until', 1,
            'repeat_while', 1,
            'if',           1,
            'unless',       1,
            'handle',       1,
            'hllize',       1,
    );
    sub autosink($past) {
        nqp::istype($past, QAST::Op) && %sinkable{$past.op} && !$past<nosink>
            ?? sink($past)
            !! $past;
    }

    method ints_to_string($ints) {
        if nqp::islist($ints) {
            my $result := '';
            for $ints {
                $result := $result ~ nqp::chr(nqp::unbox_i($_.ast));
            }
            $result;
        } else {
            nqp::chr(nqp::unbox_i($ints.ast));
        }
    }

    sub string_to_bigint($src, $base) {
        my $res := nqp::radix_I($base, ~$src, 0, 2, $*W.find_symbol(['Int']));
        $src.CURSOR.panic("'$src' is not a valid number")
            unless nqp::iseq_i(nqp::unbox_i(nqp::atpos($res, 2)), nqp::chars($src));
        nqp::atpos($res, 0);
    }

    sub xblock_immediate($xblock) {
        $xblock[1] := pblock_immediate($xblock[1]);
        $xblock;
    }

    sub pblock_immediate($pblock) {
        block_immediate($pblock<uninstall_if_immediately_used>.shift);
    }

    our sub block_immediate($block) {
        $block.blocktype('immediate');
        $block;
    }

    # Given a sigil and the the value type specified, works out the
    # container type (what should we instantiate and bind into the
    # attribute/lexpad), bind constraint (what could we bind to this
    # slot later), and if specified a constraint on the inner value
    # and a default value.
    sub container_type_info($/, $sigil, @value_type, $shape?) {
        my %info;
        if $sigil eq '@' {
            %info<container_base>  := $*W.find_symbol(['Array']);
            %info<bind_constraint> := $*W.find_symbol(['Positional']);
            if @value_type {
                %info<container_type>  := $*W.parameterize_type_with_args(
                    %info<container_base>, [@value_type[0]], nqp::hash());
                %info<bind_constraint> := $*W.parameterize_type_with_args(
                    %info<bind_constraint>, [@value_type[0]], nqp::hash());
                %info<value_type>      := @value_type[0];
                %info<default_value>   := @value_type[0];
            }
            else {
                %info<container_type> := %info<container_base>;
                %info<value_type>     := $*W.find_symbol(['Mu']);
                %info<default_value>  := $*W.find_symbol(['Any']);
            }
            if $shape {
                $*W.throw($/, 'X::Comp::NYI', feature => 'Shaped arrays');
            }
        }
        elsif $sigil eq '%' {
            %info<container_base>  := $*W.find_symbol(['Hash']);
            %info<bind_constraint> := $*W.find_symbol(['Associative']);
            if $shape {
                @value_type[0] := $*W.find_symbol(['Any']) unless +@value_type;
                my $shape_ast := $shape[0].ast;
                if $shape_ast.isa(QAST::Stmts) {
                    if +@($shape_ast) == 1 {
                        if $shape_ast[0].has_compile_time_value {
                            @value_type[1] := $shape_ast[0].compile_time_value;
                        } elsif (my $op_ast := $shape_ast[0]).isa(QAST::Op) {
                            if $op_ast.op eq "call" && +@($op_ast) == 2 {
                                if !nqp::isconcrete($op_ast[0].value) && !nqp::isconcrete($op_ast[1].value) {
                                    $*W.throw($/, 'X::Comp::NYI',
                                        feature => "coercive type declarations");
                                }
                            }
                        } else {
                            $*W.throw($/, "X::Comp::AdHoc",
                                payload => "Invalid hash shape; type expected");
                        }
                    } elsif +@($shape_ast) > 1 {
                        $*W.throw($/, 'X::Comp::NYI',
                            feature => "multidimensional shaped hashes");
                    }
                } else {
                    $*W.throw($/, "X::Comp::AdHoc",
                        payload => "Invalid hash shape; type expected");
                }
            }
            if @value_type {
                %info<container_type>  := $*W.parameterize_type_with_args(
                    %info<container_base>, @value_type, nqp::hash());
                %info<bind_constraint> := $*W.parameterize_type_with_args(
                    %info<bind_constraint>, @value_type, nqp::hash());
                %info<value_type>      := @value_type[0];
                %info<default_value>   := @value_type[0];
            }
            else {
                %info<container_type> := %info<container_base>;
                %info<value_type>     := $*W.find_symbol(['Mu']);
                %info<default_value>  := $*W.find_symbol(['Any']);
            }
        }
        elsif $sigil eq '&' {
            %info<container_base>  := $*W.find_symbol(['Scalar']);
            %info<container_type>  := %info<container_base>;
            %info<bind_constraint> := $*W.find_symbol(['Callable']);
            if @value_type {
                %info<bind_constraint> := $*W.parameterize_type_with_args(
                    %info<bind_constraint>, [@value_type[0]], nqp::hash());
            }
            %info<value_type>     := %info<bind_constraint>;
            %info<default_value>  := $*W.find_symbol(['Callable']);
            %info<scalar_value>   := $*W.find_symbol(['Callable']);
        }
        else {
            %info<container_base>     := $*W.find_symbol(['Scalar']);
            %info<container_type>     := %info<container_base>;
            if @value_type {
                %info<bind_constraint> := @value_type[0];
                %info<value_type>      := @value_type[0];
                %info<default_value>   := @value_type[0];
            }
            else {
                %info<bind_constraint> := $*W.find_symbol(['Mu']);
                %info<value_type>      := $*W.find_symbol(['Mu']);
                %info<default_value>   := $*W.find_symbol(['Any']);
            }
            %info<scalar_value> := %info<default_value>;
        }
        %info
    }

    method deflongname($/) {
        if $<colonpair> {
            my $name := ~$<name>;
            if $<colonpair>[0] {
                $name := $name ~ ':';
            }
            if $<colonpair>[0]<identifier> {
                $name := $name ~ ~$<colonpair>[0]<identifier>;
            }
            if $<colonpair>[0]<coloncircumfix> -> $cf {
                if $cf<circumfix> -> $op_name {
                    $name := $name ~ '<' ~ $*W.colonpair_nibble_to_str($/, $op_name<nibble>) ~ '>';
                }
                else {
                    $name := $name ~ '<>';
                }
            }
            make $name;
        }
        else {
            make $*W.dissect_deflongname($/).name(
                :dba("$*IN_DECL declaration"),
                :decl<routine>,
            );
        }
    }
    
    method defterm($/) {
        my $name := ~$<identifier>;
        if $<colonpair> {
            if $<colonpair>[0] {
                $name := $name ~ ':';
            }
            if $<colonpair>[0]<identifier> {
                $name := $name ~ ~$<colonpair>[0]<identifier>;
            }
            if $<colonpair>[0]<coloncircumfix> -> $cf {
                if $cf<circumfix> -> $op_name {
                    $name := $name ~ '<' ~ $*W.colonpair_nibble_to_str($/, $op_name<nibble>) ~ '>';
                }
                else {
                    $name := $name ~ '<>';
                }
            }
        }
        make $name;
    }

    # Turn $code into "for lines() { $code }"
    sub wrap_option_n_code($/, $code) {
        $code := make_topic_block_ref($code, copy => 1);
        QAST::Op.new(
            :op<call>, :name<&eager>,
            QAST::Op.new(:op<callmethod>, :name<map>,
                QAST::Op.new( :op<call>, :name<&flat>,
                    QAST::Op.new(
                        :op<call>, :name<&flat>,
                        QAST::Op.new(
                            :name<&lines>,
                            :op<call>
                        )
                    )
                ),
                $code
            )
        )
    }

    # Turn $code into "for lines() { $code; say $_ }"
    # &wrap_option_n_code already does the C<for> loop, so we just add the
    # C<say> call here
    sub wrap_option_p_code($/, $code) {
        wrap_option_n_code($/,
            QAST::Stmts.new(
                $code,
                QAST::Op.new(:name<&say>, :op<call>,
                    QAST::Var.new(:name<$_>, :scope<lexical>)
                )
            )
        )
    }

    method comp_unit($/) {
        # Finish up code object for the mainline.
        if $*DECLARAND {
            $*W.attach_signature($*DECLARAND, $*W.create_signature(nqp::hash('parameters', [])));
            $*W.finish_code_object($*DECLARAND, $*UNIT);
            $*W.add_phasers_handling_code($*DECLARAND, $*UNIT);
        }
        
        # Checks.
        $*W.assert_stubs_defined($/);
        $*W.sort_protos();

        # Get the block for the unit mainline code.
        my $unit := $*UNIT;
        my $mainline := QAST::Stmts.new(
            $*POD_PAST,
            $<statementlist>.ast,
        );

        if %*COMPILING<%?OPTIONS><p> { # also covers the -np case, like Perl 5
            $mainline := wrap_option_p_code($/, $mainline);
        }
        elsif %*COMPILING<%?OPTIONS><n> {
            $mainline := wrap_option_n_code($/, $mainline);
        }

        # We'll install our view of GLOBAL as the main one; any other
        # compilation unit that is using this one will then replace it
        # with its view later (or be in a position to restore it).
        my $global_install := QAST::Op.new(
            :op('bindcurhllsym'),
            QAST::SVal.new( :value('GLOBAL') ),
            QAST::WVal.new( :value($*GLOBALish) )
        );
        $*W.add_fixup_task(:deserialize_ast($global_install), :fixup_ast($global_install));

        # Get the block for the entire compilation unit.
        my $outer := $*UNIT_OUTER;
        $outer.node($/);
        $*UNIT_OUTER.unshift(QAST::Var.new( :name('__args__'), :scope('local'), :decl('param'), :slurpy(1) ));

        # Load the needed libraries.
        $*W.add_libs($unit);

        # If the unit defines &MAIN, and this is in the mainline,
        # add a &MAIN_HELPER.
        if !$*W.is_precompilation_mode && +(@*MODULES // []) == 0 && $unit.symbol('&MAIN') {
            $mainline := QAST::Op.new(
                :op('call'),
                :name('&MAIN_HELPER'),
                $mainline,
            );
        }

        # If our caller wants to know the mainline ctx, provide it here.
        # (CTXSAVE is inherited from HLL::Actions.) Don't do this when
        # there was an explicit {YOU_ARE_HERE}.
        unless $*HAS_YOU_ARE_HERE {
            $unit.push( self.CTXSAVE() );
        }

        # Add the mainline code to the unit.
        $unit.push($mainline);

        # Executing the compilation unit causes the mainline to be executed.
        $outer.push(QAST::Op.new( :op<call>, $unit ));

        # Do not want closure semantics on this outermost scope.
        $unit.blocktype('declaration_static');

        # Wrap everything in a QAST::CompUnit.
        my $compunit := QAST::CompUnit.new(
            :hll('perl6'),
            
            # Serialization related bits.
            :sc($*W.sc()),
            :code_ref_blocks($*W.code_ref_blocks()),
            :compilation_mode($*W.is_precompilation_mode()),
            :pre_deserialize($*W.load_dependency_tasks()),
            :post_deserialize($*W.fixup_tasks()),
            :repo_conflict_resolver(QAST::Op.new(
                :op('callmethod'), :name('resolve_repossession_conflicts'),
                QAST::Op.new(
                    :op('getcurhllsym'),
                    QAST::SVal.new( :value('ModuleLoader') )
                )
            )),

            # If this unit is loaded as a module, we want it to automatically
            # execute the mainline code above after all other initializations
            # have occurred.
            :load(QAST::Op.new(
                :op('call'),
                QAST::BVal.new( :value($outer) ),
            )),

            # Finally, the outer block, which in turn contains all of the
            # other program elements.
            $outer
        );

        # Pass some extra bits along to the optimizer.
        $compunit<UNIT>      := $unit;
        $compunit<GLOBALish> := $*GLOBALish;
        $compunit<W>         := $*W;
        
        # Do any final compiler state cleanup tasks.
        $*W.cleanup();

        make $compunit;
    }
    
    # XXX Move to HLL::Actions after NQP gets QAST.
    method CTXSAVE() {
        QAST::Stmt.new(
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('ctxsave'), :scope('local'), :decl('var') ),
                QAST::Var.new( :name('$*CTXSAVE'), :scope('contextual') )
            ),
            QAST::Op.new(
                :op('unless'),
                QAST::Op.new(
                    :op('isnull'),
                    QAST::Var.new( :name('ctxsave'), :scope('local') )
                ),
                QAST::Op.new(
                    :op('if'),
                    QAST::Op.new(
                        :op<can>,
                        QAST::Var.new( :name('ctxsave'), :scope('local') ),
                        QAST::SVal.new( :value('ctxsave') )
                    ),
                    QAST::Op.new(
                        :op('callmethod'), :name('ctxsave'),
                        QAST::Var.new( :name('ctxsave'), :scope('local')
                    )))))
    }

    method install_doc_phaser($/) {
        # Add a default DOC INIT phaser
        my $doc := %*COMPILING<%?OPTIONS><doc>;
        if $doc {
            my $block := $*W.push_lexpad($/);

            my $renderer := "Pod::To::$doc";

            my $module := $*W.load_module($/, $renderer, {}, $*GLOBALish);

            my $pod2text := QAST::Op.new(
                :op<callmethod>, :name<render>, :node($/),
                self.make_indirect_lookup([$renderer]),
                QAST::Var.new(:name<$=pod>, :scope('lexical'), :node($/))
            );

            $block.push(
                QAST::Op.new(
                    :op<call>, :node($/),
                    :name('&say'), $pod2text,
                ),
            );

            # TODO: We should print out $?USAGE too,
            # once it's known at compile time

            $block.push(
                QAST::Op.new(
                    :op<call>, :node($/),
                    :name('&exit'),
                )
            );

            $*W.pop_lexpad();
            $*W.add_phaser(
                $/, 'INIT', $*W.create_simple_code_object($block, 'Block'), $block
            );
        }
    }

    method pod_content_toplevel($/) {
        my $child := $<pod_block>.ast;
        # make sure we don't push the same thing twice
        if $child {
            my $id := $/.from ~ "," ~ ~$/.to;
            if !$*POD_BLOCKS_SEEN{$id} {
                $*POD_BLOCKS.push($child);
                $*POD_BLOCKS_SEEN{$id} := 1;
            }
        }
        make $child;
    }

    method pod_content:sym<block>($/) {
        make $<pod_block>.ast;
    }

    method pod_configuration($/) {
        make Perl6::Pod::make_config($/);
    }

    method pod_block:sym<delimited>($/) {
        make Perl6::Pod::any_block($/);
    }

    method pod_block:sym<delimited_comment>($/) {
        make Perl6::Pod::raw_block($/);
    }

    method pod_block:sym<delimited_table>($/) {
        make Perl6::Pod::table($/);
    }

    method pod_block:sym<delimited_code>($/) {
        my $config  := $<pod_configuration>.ast;
        my @content := $<delimited_code_content>.ast;
        my $twine   := Perl6::Pod::serialize_array(@content).compile_time_value;
        make Perl6::Pod::serialize_object(
            'Pod::Block::Code', :content($twine),
            :config($config),
        ).compile_time_value
    }

    method delimited_code_content($/) {
        my @t := [];
        if $<delimited_code_content> {
            @t := Perl6::Pod::merge_twines($<pod_string>) if $<pod_string>;
            @t.push($*W.add_constant(
                'Str', 'str', ~$<pod_newline>
            ).compile_time_value);
            nqp::splice(@t, $<delimited_code_content>.ast,+@t,0);
        }
        make @t;
    }

    method pod_block:sym<paragraph>($/) {
        make Perl6::Pod::any_block($/);
    }

    method pod_block:sym<paragraph_comment>($/) {
        make Perl6::Pod::raw_block($/);
    }

    method pod_block:sym<paragraph_table>($/) {
        make Perl6::Pod::table($/);
    }

    method pod_block:sym<paragraph_code>($/) {
        my $config := $<pod_configuration>.ast;
        my @t := [];
        for $<pod_line> {
            nqp::splice(@t, $_.ast, +@t, 0);
        }
        my $twine  := Perl6::Pod::serialize_array(@t).compile_time_value;
        make Perl6::Pod::serialize_object(
            'Pod::Block::Code', :content($twine),
            :config($config),
        ).compile_time_value
    }

    method pod_block:sym<abbreviated>($/) {
        make Perl6::Pod::any_block($/);
    }

    method pod_block:sym<abbreviated_comment>($/) {
        make Perl6::Pod::raw_block($/);
    }

    method pod_block:sym<abbreviated_table>($/) {
        make Perl6::Pod::table($/);
    }

    method pod_block:sym<abbreviated_code>($/) {
        my @t := [];
        for $<pod_line> {
            nqp::splice(@t, $_.ast, +@t, 0);
        }
        my $twine := Perl6::Pod::serialize_array(@t).compile_time_value;
        make Perl6::Pod::serialize_object(
            'Pod::Block::Code', :content($twine)
        ).compile_time_value
    }

    method pod_line ($/) {
        my @t := Perl6::Pod::merge_twines($<pod_string>);
        @t.push($*W.add_constant(
            'Str', 'str', ~$<pod_newline>
        ).compile_time_value);
        make @t;
    }

    method pod_block:sym<end>($/) {
    }

    method pod_content:sym<config>($/) {
        make Perl6::Pod::config($/);
    }

    method pod_content:sym<text>($/) {
        my @ret := [];
        for $<pod_textcontent> {
            @ret.push($_.ast);
        }
        my $past := Perl6::Pod::serialize_array(@ret);
        make $past.compile_time_value;
    }

    method pod_textcontent:sym<regular>($/) {
        my @t     := Perl6::Pod::merge_twines($<pod_string>);
        my $twine := Perl6::Pod::serialize_array(@t).compile_time_value;
        make Perl6::Pod::serialize_object(
            'Pod::Block::Para', :content($twine)
        ).compile_time_value
    }

    method pod_textcontent:sym<code>($/) {
        my $s := $<spaces>.Str;
        my $t := subst($<text>.Str, /\n$s/, "\n", :global);
        $t    := subst($t, /\n$/, ''); # chomp!
        my $past := Perl6::Pod::serialize_object(
            'Pod::Block::Code',
            :content(Perl6::Pod::serialize_aos([$t]).compile_time_value),
        );
        make $past.compile_time_value;
    }

    method pod_formatting_code($/) {
        if $<code> eq 'V' {
            make ~$<content>;
        } elsif $<code> eq 'E' {
            my @content := [];
            my @meta    := [];
            for $/[0] {
                if $_<html_ref> {
                    @content.push(~$_);
                    @meta.push($*W.add_string_constant(~$_).compile_time_value);
                    #my $s := Perl6::Pod::str_from_entity(~$_);
                    #$s ?? @content.push($s) && @meta.push(~$_)
                    #   !! $/.CURSOR.worry("\"$_\" is not a valid HTML5 entity.");
                } else {
                    my $n := $_<integer>
                          ?? $_<integer>.made
                          !! nqp::codepointfromname(~$_);
                    if $n >= 0 {
                        @content.push(nqp::chr($n));
                        @meta.push($n);
                    } else {
                        $/.CURSOR.worry("\"$_\" is not a valid Unicode character name or code point.");
                    }
                }
            }
            @content := Perl6::Pod::serialize_aos(@content).compile_time_value;
            @meta    := Perl6::Pod::serialize_array(@meta).compile_time_value;
            make Perl6::Pod::serialize_object(
                'Pod::FormattingCode',
                :type($*W.add_string_constant(~$<code>).compile_time_value),
                :@content,
                :@meta,
            ).compile_time_value;
        } else {
            my @content := [];
            for $<pod_string_character> {
                @content.push($_.ast)
            }
            my @meta := [];
            if $<code> eq 'X' {
                for $/[0] {
                    my @tmp := [];
                    for $_<meta> {
                        @tmp.push(~$_);
                    }
                    @meta.push(@tmp);
                }
                @meta := Perl6::Pod::serialize_aoaos(@meta).compile_time_value;
            } else {
                for $<meta> {
                    @meta.push(~$_)
                }
                @meta := Perl6::Pod::serialize_aos(@meta).compile_time_value;
            }
            my @t    := Perl6::Pod::build_pod_string(@content);
            my $past := Perl6::Pod::serialize_object(
                'Pod::FormattingCode',
                :type(
                    $*W.add_string_constant(~$<code>).compile_time_value
                ),
                :content(
                    Perl6::Pod::serialize_array(@t).compile_time_value
                ),
                :meta(@meta),
            );
            make $past.compile_time_value;
        }
    }

    method pod_string($/) {
        my @content := [];
        for $<pod_string_character> {
            @content.push($_.ast)
        }
        make Perl6::Pod::build_pod_string(@content);
    }

    method pod_balanced_braces($/) {
        if $<endtag> {
            my @content := [];
            my @stringparts := [];
            @stringparts.push(~$<start>);
            if $<pod_string_character> {
                for $<pod_string_character> {
                    if nqp::isstr($_.ast) {
                        @stringparts.push($_.ast);
                    } else {
                        @content.push(nqp::join("", @stringparts));
                        @stringparts := nqp::list();
                        @content.push($_.ast);
                    }
                }
            }
            @stringparts.push(~$<endtag>);
            @content.push(nqp::join("", @stringparts));
            if +@content == 1 {
                make @content[0];
            } else {
                make Perl6::Pod::build_pod_string(@content);
            }
        } else {
            make ~$<braces>
        }
    }

    method pod_string_character($/) {
        if $<pod_formatting_code> {
            make $<pod_formatting_code>.ast
        } elsif $<pod_balanced_braces> {
            make $<pod_balanced_braces>.ast
        } else {
            make ~$<char>;
        }
    }

    method table_row($/) {
        make ~$/
    }

    method unitstart($/) {
        # Use SET_BLOCK_OUTER_CTX (inherited from HLL::Actions)
        # to set dynamic outer lexical context and namespace details
        # for the compilation unit.
        self.SET_BLOCK_OUTER_CTX($*UNIT_OUTER);
    }

    method statementlist($/) {
        my $past := QAST::Stmts.new( :node($/) );
        if $<statement> {
            for $<statement> {
                my $ast := $_.ast;
                if $ast {
                    if $ast<statement_level> && $*statement_level {
                        $ast<statement_level>();
                    }
                    if $ast<sink_past> {
                        $ast := QAST::Want.new($ast, 'v', $ast<sink_past>);
                    }
                    elsif $ast<bare_block> {
                        $ast := autosink($ast<bare_block>);
                    }
                    else {
                        $ast := QAST::Stmt.new(autosink($ast), :returns($ast.returns));
                    }
                    $ast.node($_);
                    $past.push( $ast );
                }
            }
        }
        if +$past.list < 1 {
            $past.push(QAST::WVal.new( :value($*W.find_symbol(['Nil'])) ));
        }
        else {
            $past.returns($past[+@($past) - 1].returns);
        }
        make $past;
    }

    method semilist($/) {
        my $past := QAST::Stmts.new( :node($/) );
        if $<statement> {
            for $<statement> { $past.push($_.ast) if $_.ast; }
        }
        unless +@($past) {
            $past.push( QAST::Op.new( :op('call'), :name('&infix:<,>') ) );
        }
        make $past;
    }

    method statement($/, $key?) {
        my $past;
        if $<EXPR> {
            my $mc := $<statement_mod_cond>;
            my $ml := $<statement_mod_loop>;
            $past := $<EXPR>.ast;
            if $mc {
                $mc.ast.push($past);
                $mc.ast.push(QAST::WVal.new( :value($*W.find_symbol(['Nil'])) ));
                $past := $mc.ast;
            }
            if $ml {
                my $cond := $ml<smexpr>.ast;
                if ~$ml<sym> eq 'given' {
                    $past := QAST::Op.new(
                        :op('call'),
                        block_closure(make_topic_block_ref($past)),
                        $cond
                    );
                }
                elsif ~$ml<sym> eq 'for' {
                    unless $past<past_block> {
                        $past := make_topic_block_ref($past);
                    }
                    $past := QAST::Op.new(
                            :op<callmethod>, :name<map>, :node($/),
                            QAST::Op.new(:op('call'), :name('&infix:<,>'), $cond),
                            block_closure($past)
                        );
                    $past := QAST::Want.new(
                        QAST::Op.new( :op<callmethod>, :name<eager>, $past ),
                        'v', QAST::Op.new( :op<callmethod>, :name<sink>, $past ));
                    my $sinkee := $past[0];
                    $past<statement_level> := -> { $sinkee.name('sink') }
                }
                else {
                    $past := QAST::Op.new($cond, $past, :op(~$ml<sym>), :node($/) );
                }
            }
        }
        elsif $<statement> { $past := $<statement>.ast; }
        elsif $<statement_control> { $past := $<statement_control>.ast; }
        else { $past := 0; }
        if $STATEMENT_PRINT && $past {
            $past := QAST::Stmts.new(:node($/),
                QAST::Op.new(
                    :op<say>,
                    QAST::SVal.new(:value(~$/))
                ),
                $past
            );
        }
        make $past;
    }

    method xblock($/) {
        make QAST::Op.new( $<EXPR>.ast, $<pblock>.ast, :op('if'), :node($/) );
    }

    method pblock($/) {
        if $<blockoid><you_are_here> {
            make $<blockoid>.ast;
        }
        else {
            # Locate or build a set of parameters.
            my %sig_info;
            my @params;
            my $block := $<blockoid>.ast;
            if $block<placeholder_sig> && $<signature> {
                $*W.throw($/, ['X', 'Signature', 'Placeholder'],
                    placeholder => $block<placeholder_sig>[0]<placeholder>,
                );
            }
            elsif $block<placeholder_sig> {
                @params := $block<placeholder_sig>;
                %sig_info<parameters> := @params;
                if $*IMPLICIT {
                    $block[0].push(QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name('$_'), :scope('lexical') ),
                        QAST::Op.new( :op('getlexouter'), QAST::SVal.new( :value('$_') ) )
                    ));
                }
            }
            elsif $<signature> {
                %sig_info := $<signature>.ast;
                @params := %sig_info<parameters>;
                if $*IMPLICIT {
                    $block[0].push(QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name('$_'), :scope('lexical') ),
                        QAST::Op.new( :op('getlexouter'), QAST::SVal.new( :value('$_') ) )
                    ));
                }
            }
            else {
                if $*IMPLICIT {
                    @params.push(hash(
                        :variable_name('$_'), :optional(1),
                        :nominal_type($*W.find_symbol(['Mu'])),
                        :default_from_outer(1), :is_parcel(1),
                    ));
                }
                elsif !$block.symbol('$_') {
                    $block[0].push(QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name('$_'), :scope('lexical'), :decl('var') ),
                        QAST::Op.new( :op('getlexouter'), QAST::SVal.new( :value('$_') ) )
                    ));
                    $block.symbol('$_', :scope('lexical'), :type($*W.find_symbol(['Mu'])));
                }
                %sig_info<parameters> := @params;
            }

            # Create signature object and set up binding.
            if $<lambda> eq '<->' {
                for @params { $_<is_rw> := 1 }
            }
            set_default_parameter_type(@params, 'Mu');
            my $signature := create_signature_object($<signature>, %sig_info, $block);
            add_signature_binding_code($block, $signature, @params);

            # We'll install PAST in current block so it gets capture_lex'd.
            # Then evaluate to a reference to the block (non-closure - higher
            # up stuff does that if it wants to).
            ($*W.cur_lexpad())[0].push(my $uninst := QAST::Stmts.new($block));
            $*W.attach_signature($*DECLARAND, $signature);
            $*W.finish_code_object($*DECLARAND, $block);
            $*W.add_phasers_handling_code($*DECLARAND, $block);
            my $ref := reference_to_code_object($*DECLARAND, $block);
            $ref<uninstall_if_immediately_used> := $uninst;
            make $ref;
        }
    }

    method block($/) {
        my $block := $<blockoid>.ast;
        if $block<placeholder_sig> {
            my $name := $block<placeholder_sig>[0]<variable_name>;
            unless $name eq '%_' || $name eq '@_' {
                $name := nqp::concat(nqp::substr($name, 0, 1),
                        nqp::concat('^', nqp::substr($name, 1)));
            }

            $*W.throw( $/, ['X', 'Placeholder', 'Block'],
                placeholder => $name,
            );
        }
        ($*W.cur_lexpad())[0].push(my $uninst := QAST::Stmts.new($block));
        $*W.attach_signature($*DECLARAND, $*W.create_signature(nqp::hash('parameters', [])));
        $*W.finish_code_object($*DECLARAND, $block);
        $*W.add_phasers_handling_code($*DECLARAND, $block);
        my $ref := reference_to_code_object($*DECLARAND, $block);
        $ref<uninstall_if_immediately_used> := $uninst;
        make $ref;
    }

    method blockoid($/) {
        if $<statementlist> {
            my $past := $<statementlist>.ast;
            if %*HANDLERS {
                $past := QAST::Op.new( :op('handle'), $past );
                for %*HANDLERS {
                    $past.push($_.key);
                    $past.push($_.value);
                }
            }
            my $BLOCK := $*CURPAD;
            $BLOCK.blocktype('declaration_static');
            $BLOCK.push($past);
            $BLOCK.node($/);
            $BLOCK<statementlist> := $<statementlist>.ast;
            $BLOCK<handlers>      := %*HANDLERS if %*HANDLERS;
            make $BLOCK;
        }
        else {
            if $*HAS_YOU_ARE_HERE {
                $/.CURSOR.panic('{YOU_ARE_HERE} may only appear once in a setting');
            }
            $*HAS_YOU_ARE_HERE := 1;
            make $<you_are_here>.ast;
        }
    }

    method you_are_here($/) {
        make self.CTXSAVE();
    }

    method newpad($/) {
        my $new_block := $*W.cur_lexpad();
        $new_block<IN_DECL> := $*IN_DECL;
    }

    method finishpad($/) {
        # Generate the $_, $/, and $! lexicals for routines if they aren't
        # already declared. For blocks, $_ will come from the outer if it
        # isn't already declared.
        my $BLOCK := $*W.cur_lexpad();
        my $type := $BLOCK<IN_DECL>;
        if $type eq 'mainline' && %*COMPILING<%?OPTIONS><setting> eq 'NULL' {
            # Don't do anything in the case where we are in the mainline of
            # the setting; we don't have any symbols (Scalar, etc.) yet.
            return 1;
        }
        my $is_routine := $type eq 'sub' || $type eq 'method' ||
                          $type eq 'submethod' || $type eq 'mainline';
        if $is_routine {
            # Generate the lexical variable except if...
            #   (1) the block already has one, or
            #   (2) the variable is '$_' and $*IMPLICIT is set
            #       (this case gets handled by getsig)
            for <$_ $/ $!> {
                unless $BLOCK.symbol($_) || ($_ eq '$_' && $*IMPLICIT) {
                    $*W.install_lexical_magical($BLOCK, $_);
                }
            }
        }
        else {
            unless $BLOCK.symbol('$_') {
                if $*IMPLICIT {
                    $BLOCK[0].push(QAST::Var.new( :name('$_'), :scope('lexical'), :decl('var') ));
                }
                else {
                    $BLOCK[0].push(QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name('$_'), :scope('lexical'), :decl('var') ),
                        QAST::Op.new( :op('getlexouter'), QAST::SVal.new( :value('$_') ) )
                    ));
                }
                $BLOCK.symbol('$_', :scope('lexical'), :type($*W.find_symbol(['Mu'])));
            }
        }
    }


    ## Statement control

    method statement_control:sym<if>($/) {
        my $count := +$<xblock> - 1;
        my $past := xblock_immediate( $<xblock>[$count].ast );
        # push the else block if any, otherwise 'if' returns C<Nil> (per S04)
        $past.push( $<else>
                    ?? pblock_immediate( $<else>.ast )
                    !! QAST::WVal.new( :value($*W.find_symbol(['Nil'])) )
        );
        # build if/then/elsif structure
        while $count > 0 {
            $count--;
            my $else := $past;
            $past := xblock_immediate( $<xblock>[$count].ast );
            $past.push($else);
        }
        make $past;
    }

    method statement_control:sym<unless>($/) {
        my $past := xblock_immediate( $<xblock>.ast );
        $past.op('unless');
        make $past;
    }

    method statement_control:sym<while>($/) {
        my $past := xblock_immediate( $<xblock>.ast );
        $past.op(~$<sym>);
        make tweak_loop($past);
    }

    method statement_control:sym<repeat>($/) {
        my $op := 'repeat_' ~ ~$<wu>;
        my $past;
        if $<xblock> {
            $past := xblock_immediate( $<xblock>.ast );
            $past.op($op);
        }
        else {
            $past := QAST::Op.new( $<EXPR>.ast, pblock_immediate( $<pblock>.ast ),
                                   :op($op), :node($/) );
        }
        make tweak_loop($past);
    }

    method statement_control:sym<for>($/) {
        my $xblock := $<xblock>.ast;
        my $past := QAST::Op.new(
                        :op<callmethod>, :name<map>, :node($/),
                        QAST::Op.new(:name('&infix:<,>'), :op('call'), $xblock[0]),
                        block_closure($xblock[1])
        );
        if $*LABEL {
            $past.push(QAST::WVal.new( :value($*W.find_symbol([$*LABEL])), :named('label') ));
        }
        $past := QAST::Want.new(
            QAST::Op.new( :op<callmethod>, :name<eager>, $past ),
            'v', QAST::Op.new( :op<callmethod>, :name<sink>, $past ));
        my $sinkee := $past[0];
        $past<statement_level> := -> { $sinkee.name('sink') }
        make $past;
    }

    method statement_control:sym<loop>($/) {
        my $block := pblock_immediate($<block>.ast);
        my $cond := $<e2> ?? $<e2>.ast !! QAST::Var.new(:name<True>, :scope<lexical>);
        my $loop := QAST::Op.new( $cond, :op('while'), :node($/) );
        $loop.push($block);
        if $<e3> {
            $loop.push($<e3>.ast);
        }
        $loop := tweak_loop($loop);
        if $<e1> {
            $loop := QAST::Stmts.new( $<e1>.ast, $loop, :node($/) );
        }
        make $loop;
    }
    
    sub tweak_loop($loop) {
        if $*LABEL {
            $loop.push(QAST::WVal.new( :value($*W.find_symbol([$*LABEL])), :named('label') ));
        }
        # Handle phasers.
        my $code := $loop[1]<code_object>;
        my $block_type := $*W.find_symbol(['Block']);
        my $phasers := nqp::getattr($code, $block_type, '$!phasers');
        unless nqp::isnull($phasers) {
            if nqp::existskey($phasers, 'NEXT') {
                my $phascode := $*W.run_phasers_code($code, $block_type, 'NEXT');
                if +@($loop) == 2 {
                    $loop.push($phascode);
                }
                else {
                    $loop[2] := QAST::Stmts.new($phascode, $loop[2]);
                }
            }
            if nqp::existskey($phasers, 'FIRST') {
                $loop := QAST::Stmts.new(
                    QAST::Op.new( :op('p6setfirstflag'), QAST::WVal.new( :value($code) ) ),
                    $loop);
            }
            if nqp::existskey($phasers, 'LAST') {
                $loop := QAST::Stmts.new(
                    :resultchild(0),
                    $loop,
                    $*W.run_phasers_code($code, $block_type, 'LAST'));
            }
        }
        $loop
    }

    method statement_control:sym<need>($/) {
        my $past := QAST::WVal.new( :value($*W.find_symbol(['Nil'])) );
        for $<version> {
            # XXX TODO: Version checks.
        }
        make $past;
    }

    method statement_control:sym<import>($/) {
        my $past := QAST::WVal.new( :value($*W.find_symbol(['Nil'])) );
        make $past;
    }

    method statement_control:sym<use>($/) {
        my $past := QAST::WVal.new( :value($*W.find_symbol(['Nil'])) );
        if $<statementlist> {
            $past := $<statementlist>.ast;
        }
        elsif $<version> {
            # TODO: replace this by code that doesn't always die with
            # a useless error message
#            my $i := -1;
#            for $<version><vnum> {
#                ++$i;
#                if $_ ne '*' && $_ < @MAX_PERL_VERSION[$i] {
#                    last;
#                } elsif $_ > @MAX_PERL_VERSION[$i] {
#                    my $mpv := nqp::join('.', @MAX_PERL_VERSION);
#                    $/.CURSOR.panic("Perl $<version> required--this is only v$mpv")
#                }
#            }
        } elsif $<module_name> {
            if ~$<module_name> eq 'fatal' {
                my $*SCOPE := 'my';
                declare_variable($/, QAST::Stmts.new(), '$', '*', 'FATAL', []);
                $past := QAST::Op.new(
                    :op('p6store'), :node($/),
                    QAST::Var.new( :name('$*FATAL'), :scope('lexical') ),
                    QAST::Op.new( :op('p6bool'), QAST::IVal.new( :value(1) ) )
                );
            }
            elsif ~$<module_name> eq 'FORBID_PIR' {
                $FORBID_PIR := 1;
            }
            elsif ~$<module_name> eq 'Devel::Trace' {
                $STATEMENT_PRINT := 1;
            }
        }
        make $past;
    }

    method statement_control:sym<require>($/) {
        my $past := QAST::Stmts.new(:node($/));
        my $name_past := $<module_name>
                        ?? $*W.dissect_longname($<module_name><longname>).name_past()
                        !! $<file>.ast;
        my $op := QAST::Op.new(
            :op('callmethod'), :name('load_module'),
            QAST::Op.new( :op('getcurhllsym'),
                QAST::SVal.new( :value('ModuleLoader') ) ),
            $name_past,
            QAST::Op.new( :op('hash') ),
            $*W.symbol_lookup(['GLOBAL'], $/),
        );
        if $<module_name> {
            for $<module_name><longname><colonpair> -> $colonpair {
                $op.push(
                    QAST::Op.new( :named(~$colonpair<identifier>), :op<callmethod>, :name<value>,
                        $colonpair.ast
                    )
                );
            }
        }
        else {
            $op.push( QAST::Op.new( :named<file>, :op<callmethod>, :name<Stringy>, $<file>.ast ) );
        }
        $past.push($op);

        if $<EXPR> {
            my $p6_arglist  := $*W.compile_time_evaluate($/, $<EXPR>.ast).list.eager;
            my $arglist     := nqp::getattr($p6_arglist, $*W.find_symbol(['List']), '$!items');
            my $lexpad      := $*W.cur_lexpad();
            my $*SCOPE      := 'my';
            my $import_past := QAST::Op.new(:node($/), :op<call>,
                               :name<&REQUIRE_IMPORT>,
                               $name_past);
            for $arglist {
                my $symbol := nqp::unbox_s($_.Str());
                $*W.throw($/, ['X', 'Redeclaration'], :$symbol)
                    if $lexpad.symbol($symbol);
                declare_variable($/, $past,
                        nqp::substr($symbol, 0, 1), '', nqp::substr($symbol, 1),
                        []);
                $import_past.push($*W.add_string_constant($symbol));
            }
            $past.push($import_past);
        }
        
        $past.push(QAST::WVal.new( :value($*W.find_symbol(['Nil'])) ));

        make $past;
    }

    method statement_control:sym<given>($/) {
        my $past := $<xblock>.ast;
        $past.push($past.shift); # swap [0] and [1] elements
        $past.op('call');
        make $past;
    }

    method statement_control:sym<when>($/) {
        # Get hold of the smartmatch expression and the block.
        my $xblock := $<xblock>.ast;
        my $sm_exp := $xblock.shift;
        my $pblock := $xblock.shift;

        # Handle the smart-match.
        my $match_past := QAST::Op.new( :op('callmethod'), :name('ACCEPTS'),
            $sm_exp,
            QAST::Var.new( :name('$_'), :scope('lexical') )
        );

        # Use the smartmatch result as the condition for running the block,
        # and ensure continue/succeed handlers are in place and that a
        # succeed happens after the block.
        $pblock := pblock_immediate($pblock);
        make QAST::Op.new( :op('if'), :node( $/ ),
            $match_past, when_handler_helper($pblock)
        );
    }

    method statement_control:sym<default>($/) {
        # We always execute this, so just need the block, however we also
        # want to make sure we succeed after running it.
        make when_handler_helper($<block>.ast);
    }

    method term:sym<winner>($/) {
        my @inner_statements := $<xblock><pblock><blockoid><statementlist><statement>;
        my $wild_done;
        my $wild_more;
        my $wait;
        my $wait_time;

        my $past := QAST::Op.new( :op('call'), :name('&WINNER'), :node($/) );
        if $<xblock> {
            if nqp::istype($<xblock><EXPR>.ast.returns, $*W.find_symbol(['Whatever'])) {
                $past.push( QAST::Op.new(
                        :op('callmethod'),
                        :name('new'),
                        QAST::WVal.new( :value($*W.find_symbol(['List'])) ) ));
            } else {
                $past.push( QAST::Op.new(:name('&infix:<,>'), :op('call'), $<xblock><EXPR>.ast) );
            }
        } elsif $<block> {
            $past.push( QAST::Op.new(
                    :op('callmethod'),
                    :name('new'),
                    QAST::WVal.new( :value($*W.find_symbol(['List'])) ) ));
        }

        # TODO verify that the inner block only has more/done/later blocks in it
        for @inner_statements -> $/ {
            if $<statement_control> -> $/ {
                if $<sym> eq 'done' {
                    if nqp::istype($<xblock><EXPR>.ast.returns, $*W.find_symbol(['Whatever'])) {
                        # TODO error
                        $wild_done := block_closure($<xblock><pblock>.ast);
                        $wild_done.named('wild_done');
                    } else {
                        $past.push(QAST::IVal.new(:value(0))); # "DONE"
                        $past.push($<xblock><EXPR>.ast);
                        $past.push(block_closure($<xblock><pblock>.ast));
                    }
                } elsif $<sym> eq 'more' {
                    if nqp::istype($<xblock><EXPR>.ast.returns, $*W.find_symbol(['Whatever'])) {
                        $wild_more := block_closure($<xblock><pblock>.ast);
                        $wild_more.named('wild_more');
                    } else {
                        $past.push(QAST::IVal.new(:value(1))); # "MORE"
                        $past.push($<xblock><EXPR>.ast);
                        $past.push(block_closure($<xblock><pblock>.ast));
                    }
                } elsif $<sym> eq 'wait' {
                    # TODO error
                    $wait_time:= $<xblock><EXPR>.ast;
                    $wait_time.named('wait_time');
                    $wait := block_closure($<xblock><pblock>.ast);
                    $wait.named('wait');
                } else {
                # TODO error
                }
            } else {
                # TODO error
            }
        }
        if $wild_done { $past.push( $wild_done ) }
        if $wild_more { $past.push( $wild_more ) }
        if $wait      { $past.push( $wait ); $past.push( $wait_time ) }

        make $past;
    }

    method term:sym<combine>($/) {
        $*W.throw($/, ['X', 'NYI'], feature => 'combine blocks');
    }

    method statement_control:sym<quit>($/) {
        $*W.throw($/, ['X', 'NYI'], feature => 'combine blocks (and "quit")');
    }

    method statement_control:sym<CATCH>($/) {
        if nqp::existskey(%*HANDLERS, 'CATCH') {
            $*W.throw($/, ['X', 'Phaser', 'Multiple'], block => 'CATCH');
        }
        my $block := $<block>.ast;
        set_block_handler($/, $block, 'CATCH');
        make QAST::WVal.new( :value($*W.find_symbol(['Nil'])) );
    }

    method statement_control:sym<CONTROL>($/) {
        if nqp::existskey(%*HANDLERS, 'CONTROL') {
            $*W.throw($/, ['X', 'Phaser', 'Multiple'], block => 'CONTROL');
        }
        my $block := $<block>.ast;
        set_block_handler($/, $block, 'CONTROL');
        make QAST::WVal.new( :value($*W.find_symbol(['Nil'])) );
    }

    method statement_prefix:sym<BEGIN>($/)   { make $*W.add_phaser($/, 'BEGIN', ($<blorst>.ast)<code_object>); }
    method statement_prefix:sym<COMPOSE>($/) { make $*W.add_phaser($/, 'COMPOSE', ($<blorst>.ast)<code_object>); }
    method statement_prefix:sym<CHECK>($/)   { make $*W.add_phaser($/, 'CHECK', ($<blorst>.ast)<code_object>); }
    method statement_prefix:sym<INIT>($/)    { make $*W.add_phaser($/, 'INIT', ($<blorst>.ast)<code_object>, ($<blorst>.ast)<past_block>); }
    method statement_prefix:sym<ENTER>($/)   { make $*W.add_phaser($/, 'ENTER', ($<blorst>.ast)<code_object>); }
    method statement_prefix:sym<FIRST>($/)   { make $*W.add_phaser($/, 'FIRST', ($<blorst>.ast)<code_object>); }
    
    method statement_prefix:sym<END>($/)   { make $*W.add_phaser($/, 'END', ($<blorst>.ast)<code_object>); }
    method statement_prefix:sym<LEAVE>($/) { make $*W.add_phaser($/, 'LEAVE', ($<blorst>.ast)<code_object>); }
    method statement_prefix:sym<KEEP>($/)  { make $*W.add_phaser($/, 'KEEP', ($<blorst>.ast)<code_object>); }
    method statement_prefix:sym<UNDO>($/)  { make $*W.add_phaser($/, 'UNDO', ($<blorst>.ast)<code_object>); }
    method statement_prefix:sym<NEXT>($/)  { make $*W.add_phaser($/, 'NEXT', ($<blorst>.ast)<code_object>); }
    method statement_prefix:sym<LAST>($/)  { make $*W.add_phaser($/, 'LAST', ($<blorst>.ast)<code_object>); }
    method statement_prefix:sym<PRE>($/)   { make $*W.add_phaser($/, 'PRE', ($<blorst>.ast)<code_object>, ($<blorst>.ast)<past_block>); }
    method statement_prefix:sym<POST>($/)  { make $*W.add_phaser($/, 'POST', ($<blorst>.ast)<code_object>, ($<blorst>.ast)<past_block>); }

    method statement_prefix:sym<DOC>($/)   {
        $*W.add_phaser($/, ~$<phase>, ($<blorst>.ast)<code_object>)
            if %*COMPILING<%?OPTIONS><doc>;
    }

    method statement_prefix:sym<do>($/) {
        make QAST::Op.new( :op('call'), $<blorst>.ast );
    }

    method statement_prefix:sym<gather>($/) {
        my $past := block_closure($<blorst>.ast);
        $past<past_block>.push(QAST::WVal.new( :value($*W.find_symbol(['Nil'])) ));
        make QAST::Op.new( :op('call'), :name('&GATHER'), $past );
    }

    method statement_prefix:sym<once>($/) {

        # create state variable to remember whether we ran the block
        my $pad := $*W.cur_lexpad();
        my $sym := $pad.unique('once_');
        my $mu := $*W.find_symbol(['Mu']);
        my $descriptor := $*W.create_container_descriptor($mu, 1, $sym);
        my %info;
        %info<container_type> := %info<container_base> := $*W.find_symbol(['Scalar']);
        %info<scalar_value> := %info<default_value> := %info<bind_constraint> := %info<value_type> := $mu;
        $*W.install_lexical_container($pad, $sym, %info, $descriptor, :scope('state'));

        # generate code that runs the block only once
        make QAST::Op.new(
            :op('if'),
            QAST::Op.new( :op('p6stateinit') ),
            QAST::Op.new(
                :op('p6store'),
                QAST::Var.new( :name($sym), :scope('lexical') ),
                QAST::Op.new( :op('call'), $<blorst>.ast )
            ),
            QAST::Var.new( :name($sym), :scope('lexical') )
        );
    }

    method statement_prefix:sym<lazy>($/) {
        make QAST::Op.new( :op('call'), $<blorst>.ast );
    }

    method statement_prefix:sym<eager>($/) {
        my $blast := QAST::Op.new( :op('call'), $<blorst>.ast );
        make QAST::Op.new( :name('&eager'), :op('call'), :node($/), $blast );
    }

    method statement_prefix:sym<sink>($/) {
        my $blast := QAST::Op.new( :op('call'), $<blorst>.ast );
        make QAST::Stmts.new(
            QAST::Op.new( :name('&eager'), :op('call'), $blast ),
            QAST::Var.new( :name('Nil'), :scope('lexical')),
            :node($/)
        );
    }

    method statement_prefix:sym<try>($/) {
        my $block := $<blorst>.ast;
        my $past;
        if $block<past_block><handlers> && $block<past_block><handlers><CATCH> {
            # we already have a CATCH block, nothing to do here
            $past := QAST::Op.new( :op('call'), $block );
        } else {
            $block := QAST::Op.new(:op<call>, $block);
            $past := QAST::Op.new(
                :op('handle'),
                
                # Success path puts Any into $! and evaluates to the block.
                QAST::Stmt.new(
                    :resultchild(0),
                    $block,
                    QAST::Op.new(
                        :op('p6store'),
                        QAST::Var.new( :name<$!>, :scope<lexical> ),
                        QAST::Var.new( :name<Any>, :scope<lexical> )
                    )
                ),

                # On failure, capture the exception object into $!.
                'CATCH', QAST::Stmts.new(
                    QAST::Op.new(
                        :op('p6store'),
                        QAST::Var.new(:name<$!>, :scope<lexical>),
                        QAST::Op.new(
                            :name<&EXCEPTION>, :op<call>,
                            QAST::Op.new( :op('exception') )
                        ),
                    ),
                    QAST::VM.new(
                        :parrot(QAST::VM.new(
                            pirop => 'perl6_invoke_catchhandler 1PP',
                            QAST::Op.new( :op('null') ),
                            QAST::Op.new( :op('exception') )
                        )),
                        :jvm(QAST::Op.new( :op('null') )),
                        :moar(QAST::Op.new( :op('null') ))
                    ),
                    QAST::WVal.new(
                        :value( $*W.find_symbol(['Nil']) ),
                    ),
                )
            );
        }
        make $past;
    }

    method blorst($/) {
        make block_closure($<block> ?? $<block>.ast !! make_thunk_ref($<statement>.ast, $/));
    }

    # Statement modifiers

    method modifier_expr($/) { make $<EXPR>.ast; }

    method statement_mod_cond:sym<if>($/)     {
        make QAST::Op.new( :op<if>, $<modifier_expr>.ast, :node($/) );
    }

    method statement_mod_cond:sym<unless>($/) {
        make QAST::Op.new( :op<unless>, $<modifier_expr>.ast, :node($/) );
    }

    method statement_mod_cond:sym<when>($/) {
        make QAST::Op.new( :op<if>,
            QAST::Op.new( :name('ACCEPTS'), :op('callmethod'),
                          $<modifier_expr>.ast, 
                          QAST::Var.new( :name('$_'), :scope('lexical') ) ),
            :node($/)
        );
    }

    method statement_mod_loop:sym<while>($/)  { make $<smexpr>.ast; }
    method statement_mod_loop:sym<until>($/)  { make $<smexpr>.ast; }
    method statement_mod_loop:sym<for>($/)    { make $<smexpr>.ast; }
    method statement_mod_loop:sym<given>($/)  { make $<smexpr>.ast; }

    ## Terms

    method term:sym<fatarrow>($/)           { make $<fatarrow>.ast; }
    method term:sym<colonpair>($/)          { make $<colonpair>.ast; }
    method term:sym<variable>($/)           { make $<variable>.ast; }
    method term:sym<package_declarator>($/) { make $<package_declarator>.ast; }
    method term:sym<scope_declarator>($/)   { make $<scope_declarator>.ast; }
    method term:sym<routine_declarator>($/) { make $<routine_declarator>.ast; }
    method term:sym<multi_declarator>($/)   { make $<multi_declarator>.ast; }
    method term:sym<regex_declarator>($/)   { make $<regex_declarator>.ast; }
    method term:sym<type_declarator>($/)    { make $<type_declarator>.ast; }
    method term:sym<circumfix>($/)          { make $<circumfix>.ast; }
    method term:sym<statement_prefix>($/)   { make $<statement_prefix>.ast; }
    method term:sym<sigterm>($/)            { make $<sigterm>.ast; }
    method term:sym<lambda>($/) {
        my $ast   := $<pblock>.ast;
        my $block := $ast<past_block>;
        $block[0].push(QAST::Var.new( :name('$*DISPATCHER'), :scope('lexical'), :decl('var') ));
        $block[0].push(QAST::Op.new(
            :op('takedispatcher'),
            QAST::SVal.new( :value('$*DISPATCHER') )
        ));
        make block_closure($ast);
    }
    method term:sym<unquote>($/) {
        make QAST::Unquote.new(:position(+@*UNQUOTE_ASTS));
        @*UNQUOTE_ASTS.push($<statementlist>.ast);
    }

    method name($/) { }

    method fatarrow($/) {
        make make_pair($<key>.Str, $<val>.ast);
    }
    
    method coloncircumfix($/) {
        make $<circumfix>
            ?? $<circumfix>.ast
            !! QAST::WVal.new( :value($*W.find_symbol(['Nil'])) );
    }

    method colonpair($/) {
        if $*key {
            if $<var> {
                make make_pair($*key, $<var>.ast);
            }
            elsif $*value ~~ NQPMatch {
                my $val_ast := $*value.ast;
                if $val_ast.isa(QAST::Stmts) && +@($val_ast) == 1 {
                    $val_ast := $val_ast[0];
                }
                make make_pair($*key, $val_ast);
            }
            else {
                make make_pair($*key, QAST::Op.new(
                    :op('p6bool'),
                    QAST::IVal.new( :value($*value) ) 
                ));
            }
        }
        elsif $<fakesignature> {
            make $<fakesignature>.ast;
        }
        else {
            make $*value.ast;
        }
    }
    
    method colonpair_variable($/) {
        if $<capvar> {
            make QAST::Op.new(
                :op('call'),
                :name('&postcircumfix:<{ }>'),
                QAST::Var.new(:name('$/'), :scope('lexical')),
                $*W.add_string_constant(~$<desigilname>)
            );
        }
        else {
            make make_variable($/, [~$/]);
        }
    }

    sub make_pair($key_str, $value) {
        my $key := $*W.add_string_constant($key_str);
        $key.named('key');
        $value.named('value');
        QAST::Op.new(
            :op('callmethod'), :name('new'), :returns($*W.find_symbol(['Pair'])),
            QAST::Var.new( :name('Pair'), :scope('lexical') ),
            $key, $value
        )
    }
    
    method desigilname($/) {
        if $<variable> {
            make QAST::Op.new( :op('callmethod'), $<variable>.ast );
        }
    }

    method variable($/) {
        my $past;
        if $<index> {
            $past := QAST::Op.new(
                :op('call'),
                :name('&postcircumfix:<[ ]>'),
                QAST::Var.new(:name('$/'), :scope('lexical')),
                $*W.add_constant('Int', 'int', +$<index>),
            );
            if $<sigil> eq '@' || $<sigil> eq '%' {
                my $name := $<sigil> eq '@' ?? 'list' !! 'hash';
                $past := QAST::Op.new( :op('callmethod'), :name($name), $past );
            }
        }
        elsif $<postcircumfix> {
            $past := $<postcircumfix>.ast;
            $past.unshift( QAST::Var.new( :name('$/'), :scope('lexical') ) );
            if $<sigil> eq '@' || $<sigil> eq '%' {
                my $name := $<sigil> eq '@' ?? 'list' !! 'hash';
                $past := QAST::Op.new( :op('callmethod'), :name($name), $past );
            }
        }
        elsif $<semilist> {
            $past := $<semilist>.ast;
            if $<sigil> eq '$' && ~$<semilist> eq '' { # for '$()'
                my $result_var := $past.unique('sm_result');
                $past := QAST::Stmt.new(
                    # Evaluate RHS and call ACCEPTS on it, passing in $_. Bind the
                    # return value to a result variable.
                    QAST::Op.new( :op('bind'),
                        QAST::Var.new( :name($result_var), :scope('local'), :decl('var') ),
                        QAST::Op.new(
                            :op('if'),
                            # condition
                            QAST::Op.new(
                                :op('callmethod'), :name('ast'),
                                QAST::Var.new( :name('$/'), :scope('lexical') )
                            ),
                            # when true
                            QAST::Op.new(
                                :op('callmethod'), :name('ast'),
                                QAST::Var.new( :name('$/'), :scope('lexical') )
                            ),
                            # when false
                            QAST::Op.new(
                                :op('callmethod'), :name('Str'),
                                QAST::Var.new( :name('$/'), :scope('lexical') )
                            )
                        )
                    ),
                    # And finally evaluate to the smart-match result.
                    QAST::Var.new( :name($result_var), :scope('local') )
                );
                $past := QAST::Op.new( :op('locallifetime'), $past, $result_var );
            }
            else {
                my $name := ~$<sigil> eq '@' ?? 'list' !!
                            ~$<sigil> eq '%' ?? 'hash' !!
                                                'item';
                # @() and %()
                $past := QAST::Var.new( :name('$/'), :scope('lexical') ) if ~$<semilist> eq '';

                $past := QAST::Op.new( :op('callmethod'), :name($name), $past );
            }
        }
        elsif $<infixish> {
            my $name := '&infix:<' ~ $<infixish>.Str ~ '>';
            $past := QAST::Op.new(
                :op('ifnull'),
                QAST::Var.new( :name($name), :scope('lexical') ),
                QAST::Op.new(
                    :op('die_s'),
                    QAST::SVal.new( :value("Could not find sub $name") )
                ));
        }
        elsif $<desigilname><variable> {
            $past := $<desigilname>.ast;
            $past.name(~$<sigil> eq '@' ?? 'list' !!
                       ~$<sigil> eq '%' ?? 'hash' !!
                                           'item');
        }
        else {
            my $indirect;
            if $<desigilname> && $<desigilname><longname> {
                my $longname := $*W.dissect_longname($<desigilname><longname>);
                if $longname.contains_indirect_lookup() {
                    if $*IN_DECL {
                        $*W.throw($/, ['X', 'Syntax', 'Variable', 'IndirectDeclaration']);
                    }
                    $past := self.make_indirect_lookup($longname.components(), ~$<sigil>);
                    $indirect := 1;
                }
                else {
                    $past := make_variable($/, $longname.variable_components(
                        ~$<sigil>, $<twigil> ?? ~$<twigil> !! ''));
                }
            }
            else {
                $past := make_variable($/, [~$/]);
            }
        }
        if $*IN_DECL eq 'variable' {
            $past<sink_ok> := 1;
        }
        make $past;
    }

    sub make_variable($/, @name) {
        make_variable_from_parts($/, @name, $<sigil>.Str, $<twigil>, ~$<desigilname>);
    }

    sub make_variable_from_parts($/, @name, $sigil, $twigil, $desigilname) {
        my $past := QAST::Var.new( :name(@name[+@name - 1]), :node($/));
        if $twigil eq '*' {
            $past := QAST::Op.new(
                :op('call'), :name('&DYNAMIC'),
                $*W.add_string_constant($past.name()));
        }
        elsif $twigil eq '!' {
            # In a declaration, don't produce anything here.
            if $*IN_DECL ne 'variable' {
                unless $*HAS_SELF {
                    $*W.throw($/, ['X', 'Syntax', 'NoSelf'], variable => $past.name());
                }
                my $attr := get_attribute_meta_object($/, $past.name(), $past);
                $past.returns($attr.type) if $attr;
                $past.scope('attribute');
                $past.unshift(instantiated_type(['$?CLASS'], $/));
                $past.unshift(QAST::Var.new( :name('self'), :scope('lexical') ));
            }
        }
        elsif $twigil eq '.' && $*IN_DECL ne 'variable' {
            if !$*HAS_SELF {
                $*W.throw($/, ['X', 'Syntax', 'NoSelf'], variable => $past.name());
            } elsif $*HAS_SELF eq 'partial' {
                $*W.throw($/, ['X', 'Syntax', 'VirtualCall'], call => $past.name());
            }
            # Need to transform this to a method call.
            $past := $<arglist> ?? $<arglist>.ast !! QAST::Op.new();
            $past.op('callmethod');
            $past.name($desigilname);
            $past.unshift(QAST::Var.new( :name('self'), :scope('lexical') ));
            # Contextualize based on sigil.
            $past := QAST::Op.new(
                :op('callmethod'),
                :name($sigil eq '@' ?? 'list' !!
                      $sigil eq '%' ?? 'hash' !!
                      'item'),
                $past);
        }
        elsif $twigil eq '^' || $twigil eq ':' {
            $past := add_placeholder_parameter($/, $sigil, $desigilname,
                                :named($twigil eq ':'), :full_name($past.name()));
        }
        elsif $past.name() eq '@_' {
            if $*W.nearest_signatured_block_declares('@_') {
                $past.scope('lexical');
            }
            else {
                $past := add_placeholder_parameter($/, '@', '_',
                                :pos_slurpy(1), :full_name($past.name()));
            }
        }
        elsif $past.name() eq '%_' {
            if $*W.nearest_signatured_block_declares('%_') || $*METHODTYPE {
                $past.scope('lexical');
            }
            else {
                $past := add_placeholder_parameter($/, '%', '_', :named_slurpy(1),
                                :full_name($past.name()));
            }
        }
        elsif $past.name() eq '$?LINE' || $past.name eq '$?FILE' {
            if $*IN_DECL eq 'variable' {
                $*W.throw($/, 'X::Syntax::Variable::Twigil',
                        twigil  => '?',
                        scope   => $*SCOPE,
                );
            }
            if $past.name() eq '$?LINE' {
                $past := $*W.add_constant('Int', 'int',
                        HLL::Compiler.lineof($/.orig, $/.from, :cache(1)));
            }
            else {
                $past := $*W.add_string_constant(nqp::getlexdyn('$?FILES') // '<unknown file>');
            }
        }
        elsif +@name > 1 {
            $past := $*W.symbol_lookup(@name, $/, :lvalue(1));
        }
        elsif $*IN_DECL ne 'variable' && (my $attr_alias := $*W.is_attr_alias($past.name)) {
            $past.name($attr_alias);
            $past.scope('attribute');
            $past.unshift(instantiated_type(['$?CLASS'], $/));
            $past.unshift(QAST::Var.new( :name('self'), :scope('lexical') ));
        }
        elsif $*IN_DECL ne 'variable' {
            # Expect variable to have been declared somewhere.
            # Locate descriptor and thus type.
            $past.scope('lexical');
            try {
                my $type := $*W.find_lexical_container_type($past.name);
                $past.returns($type);
            }
            
            # If it's a late-bound sub lookup, we may not find it, so be sure
            # to handle the case where the lookup comes back null.
            if $sigil eq '&' {
                $past := QAST::Op.new(
                    :op('ifnull'), $past,
                    QAST::WVal.new( :value($*W.find_symbol(['Nil'])) ));
            }
        }
        $past
    }
    
    sub get_attribute_meta_object($/, $name, $later?) {
        unless nqp::can($*PACKAGE.HOW, 'get_attribute_for_usage') {
            $/.CURSOR.panic("Cannot understand $name in this context");
        }
        my $attr;
        my int $found := 0;
        try {
            $attr := $*PACKAGE.HOW.get_attribute_for_usage($*PACKAGE, $name);
            $found := 1;
        }
        unless $found {

            # need to check later
            if $later {
                my $seen := %*ATTR_USAGES{$name};
                unless $seen {
                    %*ATTR_USAGES{$name} := $seen := nqp::list();
                    $later.node($/); # only need $/ for first error
                }
                $seen.push($later);
            }

            # now is later
            else {
                $*W.throw($/, ['X', 'Attribute', 'Undeclared'],
                  symbol       => $name,
                  package-kind => $*PKGDECL,
                  package-name => $*PACKAGE.HOW.name($*PACKAGE),
                  what         => 'attribute',
                );
            }
        }
        $attr
    }

    method package_declarator:sym<package>($/) { make $<package_def>.ast; }
    method package_declarator:sym<module>($/)  { make $<package_def>.ast; }
    method package_declarator:sym<class>($/)   { make $<package_def>.ast; }
    method package_declarator:sym<grammar>($/) { make $<package_def>.ast; }
    method package_declarator:sym<role>($/)    { make $<package_def>.ast; }
    method package_declarator:sym<knowhow>($/) { make $<package_def>.ast; }
    method package_declarator:sym<native>($/)  { make $<package_def>.ast; }

    method package_declarator:sym<trusts>($/) {
        $*W.apply_trait($/, '&trait_mod:<trusts>', $*PACKAGE, $<typename>.ast);
    }

    method package_declarator:sym<also>($/) {
        for $<trait> {
            if $_.ast { ($_.ast)($*DECLARAND) }
        }
    }

    method package_def($/) {
        # Get the body block PAST.
        my $block;
        if $<blockoid> {
            $block := $<blockoid>.ast;
        }
        else {
            $block := $*CURPAD;
            $block.push($<statementlist>.ast);
            $block.node($/);
        }
        $block.blocktype('immediate');

        if $*PKGDECL ne 'role' && $block<placeholder_sig> {
            my $name := $block<placeholder_sig>[0]<variable_name>;
            unless $name eq '%_' || $name eq '@_' {
                $name := nqp::concat(nqp::substr($name, 0, 1),
                        nqp::concat('^', nqp::substr($name, 1)));
            }
            $*W.throw( $/, ['X', 'Placeholder', 'Block'],
                placeholder => $name,
            );
        }

        # If it's a stub, add it to the "must compose at some point" list,
        # then just evaluate to the type object. Don't need to do any more
        # just yet.
        if nqp::substr($<blockoid><statementlist><statement>[0], 0, 3) eq '...' {
            unless $*PKGDECL eq 'role' {
                $*W.add_stub_to_check($*PACKAGE);
            }
            $block.blocktype('declaration');
            make QAST::Stmts.new( $block, QAST::WVal.new( :value($*PACKAGE) ) );
            return 1;
        }

        # Handle parametricism for roles.
        if $*PKGDECL eq 'role' {
            # Set up signature. Needs to have $?CLASS as an implicit
            # parameter, since any mention of it is generic.
            my %sig_info := $<signature> ?? $<signature>.ast !! hash(parameters => []);
            my @params := %sig_info<parameters>;
            @params.unshift(hash(
                is_multi_invocant => 1,
                type_captures     => ['$?CLASS', '::?CLASS']
            ));
            set_default_parameter_type(@params, 'Mu');
            my $sig := create_signature_object($<signature>, %sig_info, $block);
            add_signature_binding_code($block, $sig, @params);
            $block.blocktype('declaration_static');

            # Need to ensure we get lexical outers fixed up properly. To
            # do this we make a list of closures, which each point to the
            # outer context. These surive serialization and thus point at
            # what has to be fixed up.
            my $throwaway_block_past := QAST::Block.new( 
                :blocktype('declaration'),
                QAST::Var.new( :name('$_'), :scope('lexical'), :decl('var') )
            );
            $throwaway_block_past<outer> := $block;
            $block[0].push($throwaway_block_past);
            my $throwaway_block := $*W.create_code_object($throwaway_block_past,
                'Block', $*W.create_signature(nqp::hash('parameters', [])));
            my $fixup := $*W.create_lexical_capture_fixup();
            $fixup.push(QAST::Op.new(
                :op('callmethod'), :name('clone'),
                QAST::Op.new(
                    :op('p6capturelex'),
                    QAST::WVal.new( :value($throwaway_block) )
                )));
            $block[1].push($fixup);

            # As its last act, it should grab the current lexpad so that
            # we have the type environment, and also return the parametric
            # role we're in (because if we land it through a multi-dispatch,
            # we won't know).
            $block[1].push(QAST::Op.new(
                :op('list'),
                QAST::WVal.new( :value($*PACKAGE) ),
                QAST::Op.new( :op('curlexpad') )));

            # Create code object and add it as the role's body block.
            my $code := $*W.create_code_object($block, 'Sub', $sig);
            $*W.pkg_set_role_body_block($/, $*PACKAGE, $code, $block);
            
            # Compose before we add the role to the group, so the group sees
            # it composed.
            $*W.pkg_compose($*PACKAGE);
            
            # Add this role to the group if needed.
            my $group := $*PACKAGE.HOW.group($*PACKAGE);
            unless $group =:= $*PACKAGE {
                $*W.pkg_add_role_group_possibility($/, $group, $*PACKAGE);
            }
        }
        else {
            # Compose.
            $*W.pkg_compose($*PACKAGE);
            
            # Make a code object for the block.
            $*W.create_code_object($block, 'Block',
                $*W.create_signature(nqp::hash('parameters', [])));
        }

        # check up any private attribute usage
        for %*ATTR_USAGES {
            my $name   := $_.key;
            my @usages := $_.value;
            for @usages {
                my $past := $_;
                my $attr := get_attribute_meta_object($past.node, $name);
                $past.returns($attr.type);
            }
        }

        # Document
        Perl6::Pod::document($/, $*PACKAGE, $*DOC);

        make QAST::Stmts.new(
            $block, QAST::WVal.new( :value($*PACKAGE) )
        );
    }

    method scope_declarator:sym<my>($/)      { make $<scoped>.ast; }
    method scope_declarator:sym<our>($/)     { make $<scoped>.ast; }
    method scope_declarator:sym<has>($/)     { make $<scoped>.ast; }
    method scope_declarator:sym<anon>($/)    { make $<scoped>.ast; }
    method scope_declarator:sym<augment>($/) { make $<scoped>.ast; }
    method scope_declarator:sym<state>($/)   { make $<scoped>.ast; }

    method declarator($/) {
        if    $<routine_declarator>  { make $<routine_declarator>.ast  }
        elsif $<regex_declarator>    { make $<regex_declarator>.ast    }
        elsif $<type_declarator>     { make $<type_declarator>.ast     }
        elsif $<variable_declarator> {
            my $past := $<variable_declarator>.ast;
            if $<initializer> {
                my $orig_past := $past;
                if $*SCOPE eq 'has' {
                    if $<initializer><sym> eq '=' {
                        self.install_attr_init($<initializer>, $past<metaattr>,
                            $<initializer>.ast, $*ATTR_INIT_BLOCK);
                    }
                    else {
                        $/.CURSOR.panic("Cannot use " ~ $<initializer><sym> ~
                            " to initialize an attribute");
                    }
                }
                elsif $<initializer><sym> eq '=' {
                    $past := assign_op($/, $past, $<initializer>.ast);
                }
                elsif $<initializer><sym> eq '.=' {
                    $past := make_dot_equals($past, $<initializer>.ast);
                }
                else {
                    if nqp::istype($past, QAST::Var) {
                        find_var_decl($*W.cur_lexpad(), $past.name).decl('var');
                    }
                    $past := bind_op($/, $past, $<initializer>.ast,
                        $<initializer><sym> eq '::=');
                }
                if $*SCOPE eq 'state' {
                    $past := QAST::Op.new( :op('if'),
                        QAST::Op.new( :op('p6stateinit') ),
                        $past,
                        $orig_past);
                    $past<nosink> := 1;
                }
            }
            make $past;
        }
        elsif $<signature> {
            # Go over the params and declare the variable defined
            # in them.
            my $list   := QAST::Op.new( :op('call'), :name('&infix:<,>') );
            my @params := $<signature>.ast<parameters>;
            for @params {
                if $_<variable_name> {
                    my $past := QAST::Var.new( :name($_<variable_name>) );
                    $past := declare_variable($/, $past, $_<sigil>, $_<twigil>,
                        $_<desigilname>, []);
                    unless $past.isa(QAST::Op) && $past.op eq 'null' {
                        $list.push($past);
                    }
                }
                else {
                    my %cont_info := container_type_info($/, $_<sigil> || '$', []);
                    $list.push($*W.build_container_past(
                      %cont_info,
                      $*W.create_container_descriptor(
                        %cont_info<value_type>, 1, 'anon', %cont_info<default_value>)));
                }
            }
            
            if $<initializer> {
                my $orig_list := $list;
                if $<initializer><sym> eq '=' {
                    $/.CURSOR.panic("Cannot assign to a list of 'has' scoped declarations")
                        if $*SCOPE eq 'has';
                    $list := assign_op($/, $list, $<initializer>.ast);
                }
                elsif $<initializer><sym> eq '.=' {
                    $/.CURSOR.panic("Cannot use .= initializer with a list of declarations");
                }
                else {
                    my %sig_info := $<signature>.ast;
                    my @params := %sig_info<parameters>;
                    set_default_parameter_type(@params, 'Mu');
                    my $signature := create_signature_object($/, %sig_info, $*W.cur_lexpad());
                    $list := QAST::Op.new(
                        :op('p6bindcaptosig'),
                        QAST::WVal.new( :value($signature) ),
                        QAST::Op.new(
                            :op('callmethod'), :name('Capture'),
                            $<initializer>.ast
                        )
                    );
                }
                if $*SCOPE eq 'state' {
                    $list := QAST::Op.new( :op('if'),
                        QAST::Op.new( :op('p6stateinit') ),
                        $list, $orig_list);
                }
            }
            
            make $list;
        }
        elsif $<identifier> {
            # 'my \foo' style declaration
            if $*SCOPE ne 'my' {
                $*W.throw($/, 'X::Comp::NYI',
                    feature => "$*SCOPE scoped term definitions (only 'my' is supported at the moment)");
            }
            my $name       :=  ~$<identifier>;
            my $cur_lexpad := $*W.cur_lexpad;
            if $cur_lexpad.symbol($name) {
                $*W.throw($/, ['X', 'Redeclaration'], symbol => $name);
            }
            if $*OFTYPE {
                my $type := $*OFTYPE.ast;
                $cur_lexpad[0].push(QAST::Var.new( :$name, :scope('lexical'),
                    :decl('var'), :returns($type) ));
                $cur_lexpad.symbol($name, :$type, :scope<lexical>);
                make QAST::Op.new(
                    :op<bind>,
                    QAST::Var.new(:$name, :scope<lexical>),
                    $type =:= $*W.find_symbol(['Mu'])
                        ?? $<term_init>.ast
                        !! QAST::Op.new(
                            :op('p6bindassert'),
                            $<term_init>.ast,
                            QAST::WVal.new( :value($type) ),
                        )
                );
            }
            else {
                $cur_lexpad[0].push(QAST::Var.new(:$name, :scope('lexical'), :decl('var')));
                $cur_lexpad.symbol($name, :scope('lexical'));
                make QAST::Op.new(
                    :op<bind>,
                    QAST::Var.new(:$name, :scope<lexical>),
                    $<term_init>.ast
                );
                }
        }
        else {
            $/.CURSOR.panic('Unknown declarator type');
        }
    }

    method multi_declarator:sym<multi>($/) { make $<declarator> ?? $<declarator>.ast !! $<routine_def>.ast }
    method multi_declarator:sym<proto>($/) { make $<declarator> ?? $<declarator>.ast !! $<routine_def>.ast }
    method multi_declarator:sym<only>($/)  { make $<declarator> ?? $<declarator>.ast !! $<routine_def>.ast }
    method multi_declarator:sym<null>($/)  { make $<declarator>.ast }

    method scoped($/) {
        make $<DECL>.ast;
    }

    method variable_declarator($/) {
        my $past   := $<variable>.ast;
        my $sigil  := $<variable><sigil>;
        my $twigil := $<variable><twigil>;
        my $name   := ~$sigil ~ ~$twigil ~ ~$<variable><desigilname>;
        if $<variable><desigilname> {
            my $lex := $*W.cur_lexpad();
            if $lex.symbol($name) {
                $/.CURSOR.typed_worry('X::Redeclaration', symbol => $name);
            }
            elsif $lex<also_uses> && $lex<also_uses>{$name} {
                $/.CURSOR.typed_sorry('X::Redeclaration::Outer', symbol => $name);
            }
            make declare_variable($/, $past, ~$sigil, ~$twigil, ~$<variable><desigilname>, $<trait>, $<semilist>);
        }
        else {
            make declare_variable($/, $past, ~$sigil, ~$twigil, ~$<variable><desigilname>, $<trait>, $<semilist>);
        }
    }

    sub declare_variable($/, $past, $sigil, $twigil, $desigilname, $trait_list, $shape?) {
        my $name  := $sigil ~ $twigil ~ $desigilname;
        my $BLOCK := $*W.cur_lexpad();

        if $*SCOPE eq 'has' {
            # Ensure current package can take attributes.
            unless nqp::can($*PACKAGE.HOW, 'add_attribute') {
                if $*PKGDECL {
                    $*W.throw($/, ['X', 'Attribute', 'Package'],
                        package-kind => $*PKGDECL,
                        :$name,
                    );
                } else {
                    $*W.throw($/, ['X', 'Attribute', 'NoPackage'], :$name);
                }
            }

            # Create container descriptor and decide on any default value.
            if $desigilname eq '' {
                $/.CURSOR.panic("Cannot declare an anonymous attribute");
            }
            my $attrname   := ~$sigil ~ '!' ~ $desigilname;
            my %cont_info  := container_type_info($/, $sigil, $*OFTYPE ?? [$*OFTYPE.ast] !! [], $shape);
            my $descriptor := $*W.create_container_descriptor(
              %cont_info<value_type>, 1, $attrname, %cont_info<default_value>);

            # Create meta-attribute and add it.
            my $metaattr := %*HOW{$*PKGDECL ~ '-attr'};
            my $attr := $*W.pkg_add_attribute($/, $*PACKAGE, $metaattr,
                hash(
                    name => $attrname,
                    has_accessor => $twigil eq '.'
                ),
                hash(
                    container_descriptor => $descriptor,
                    type => %cont_info<bind_constraint>,
                    package => $*W.find_symbol(['$?CLASS'])),
                %cont_info, $descriptor);

            # Document it
            # Perl6::Pod::document($/, $attr, $*DOC); #XXX var traits NYI

            # If no twigil, note $foo is an alias to $!foo.
            if $twigil eq '' {
                $BLOCK.symbol($name, :attr_alias($attrname));
            }

            # Apply any traits.
            for $trait_list {
                my $applier := $_.ast;
                if $applier { $applier($attr); }
            }

            # Nothing to emit here; hand back a Nil.
            $past := QAST::WVal.new( :value($*W.find_symbol(['Nil'])) );
            $past<metaattr> := $attr;
        }
        elsif $*SCOPE eq 'my' || $*SCOPE eq 'our' || $*SCOPE eq 'state' {
            # Some things can't be done to our vars.
            if $*SCOPE eq 'our' {
                if $*OFTYPE {
                    $/.CURSOR.panic("Cannot put a type constraint on an 'our'-scoped variable");
                }
                elsif $shape {
                    $/.CURSOR.panic("Cannot put a shape on an 'our'-scoped variable");
                }
                elsif $desigilname eq '' {
                    $/.CURSOR.panic("Cannot have an anonymous 'our'-scoped variable");
                }
            }

            # Create a container descriptor. Default to rw and set a
            # type if we have one; a trait may twiddle with that later.
            my %cont_info := container_type_info($/, $sigil, $*OFTYPE ?? [$*OFTYPE.ast] !! [], $shape);
            my $descriptor := $*W.create_container_descriptor(
              %cont_info<value_type>, 1, $name, %cont_info<default_value>);

            # Install the container.
            if $desigilname eq '' {
                $name := QAST::Node.unique('ANON_VAR_');
            }
            my $cont := $*W.install_lexical_container($BLOCK, $name, %cont_info, $descriptor,
                :scope($*SCOPE), :package($*PACKAGE));
            
            # Set scope and type on container, and if needed emit code to
            # reify a generic type.
            if $past.isa(QAST::Var) {
                $past.name($name);
                $past.scope('lexical');
                $past.returns(%cont_info<bind_constraint>);
                if %cont_info<bind_constraint>.HOW.archetypes.generic {
                    $past := QAST::Op.new(
                        :op('callmethod'), :name('instantiate_generic'),
                        QAST::Op.new( :op('p6var'), $past ),
                        QAST::Op.new( :op('curlexpad') ));
                }
                
                if $*SCOPE eq 'our' {
                    $BLOCK[0].push(QAST::Op.new(
                        :op('bind'),
                        $past,
                        $*W.symbol_lookup([$name], $/, :package_only(1), :lvalue(1))
                    ));
                }
            }
            
            # Twigil handling.
            if $twigil eq '.' {
                add_lexical_accessor($/, $past, $desigilname, $*W.cur_lexpad());
                $name := $sigil ~ $desigilname;
            }
            elsif $twigil eq '!' {
                $*W.throw($/, ['X', 'Syntax', 'Variable', 'Twigil'],
                    twigil => $twigil,
                    scope  => $*SCOPE,
                );
            }
            
            # Apply any traits.
            if $trait_list {
                my $Variable := $*W.find_symbol(['Variable']);
                my $varvar   := nqp::create($Variable);
                nqp::bindattr_s($varvar, $Variable, '$!name', $name);
                nqp::bindattr_s($varvar, $Variable, '$!scope', $*SCOPE);
                nqp::bindattr($varvar, $Variable, '$!var', $cont);
                nqp::bindattr($varvar, $Variable, '$!block', $*DECLARAND);
                nqp::bindattr($varvar, $Variable, '$!slash', $/);
                for $trait_list {
                    my $applier := $_.ast;
                    if $applier { $applier($varvar); }
                }
            }
        }
        else {
            $*W.throw($/, 'X::Comp::NYI',
                feature => "$*SCOPE scoped variables");
        }

        $past
    }
    
    sub add_lexical_accessor($/, $var_past, $meth_name, $install_in) {
        # Generate and install code block for accessor.
        my $a_past := $*W.push_lexpad($/);
        $a_past.name($meth_name);
        $a_past.push($var_past);
        $*W.pop_lexpad();
        $install_in[0].push($a_past);

        # Produce a code object and install it.
        my $invocant_type := $*W.find_symbol([$*W.is_lexical('$?CLASS') ?? '$?CLASS' !! 'Mu']);
        my %sig_info := hash(parameters => []);
        my $code := methodize_block($/, $*W.stub_code_object('Method'), 
            $a_past, %sig_info, $invocant_type);
        install_method($/, $meth_name, 'has', $code, $install_in);
    }

    method routine_declarator:sym<sub>($/) { make $<routine_def>.ast; }
    method routine_declarator:sym<method>($/) { make $<method_def>.ast; }
    method routine_declarator:sym<submethod>($/) { make $<method_def>.ast; }

    method routine_def($/) {
        my $block;

        if $<onlystar> {
            $block := $<onlystar>.ast;
        }
        else {
            $block := $<blockoid>.ast;
            if is_clearly_returnless($block) {
                unless nqp::objprimspec($block[1].returns) {
                    $block[1] := QAST::Op.new(
                        :op('p6decontrv'),
                        QAST::WVal.new( :value($*DECLARAND) ),
                        $block[1]);
                }
                $block[1] := wrap_return_type_check($block[1], $*DECLARAND);
            }
            else {
                $block[1] := wrap_return_handler($block[1]);
            }
        }
        $block.blocktype('declaration_static');

        # Obtain parameters, create signature object and generate code to
        # call binder.
        if $block<placeholder_sig> && $<multisig> {
            $*W.throw($/, ['X', 'Signature', 'Placeholder'],
                placeholder => $block<placeholder_sig>[0]<placeholder>,
            );
        }
        my %sig_info;
        if $<multisig> {
            %sig_info := $<multisig>.ast;
        }
        else {
            %sig_info<parameters> := $block<placeholder_sig> ?? $block<placeholder_sig> !!
                                                                [];
        }
        my @params := %sig_info<parameters>;
        set_default_parameter_type(@params, 'Any');
        my $signature := create_signature_object($<multisig> ?? $<multisig> !! $/, %sig_info, $block);
        add_signature_binding_code($block, $signature, @params);

        # Needs a slot that can hold a (potentially unvivified) dispatcher;
        # if this is a multi then we'll need it to vivify to a MultiDispatcher.
        if $*MULTINESS eq 'multi' {
            $*W.install_lexical_symbol($block, '$*DISPATCHER', $*W.find_symbol(['MultiDispatcher']));
        }
        else {
            $block[0].push(QAST::Var.new( :name('$*DISPATCHER'), :scope('lexical'), :decl('var') ));
        }
        $block[0].push(QAST::Op.new(
            :op('takedispatcher'),
            QAST::SVal.new( :value('$*DISPATCHER') )
        ));
        
        # If it's a proto but not an onlystar, need some variables for the
        # {*} implementation to use.
        if $*MULTINESS eq 'proto' && !$<onlystar> {
            $block[0].push(QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('CURRENT_DISPATCH_CAPTURE'), :scope('lexical'), :decl('var') ),
                QAST::Op.new( :op('savecapture') )
            ));
            $block[0].push(QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('&*CURRENT_DISPATCHER'), :scope('lexical'), :decl('var') ),
                QAST::Op.new( :op('getcodeobj'), QAST::Op.new( :op('curcode') ) )
            ));
        }

        # Set name.
        if $<deflongname> {
            $block.name(~$<deflongname>.ast);
        }
        
        # Finish code object, associating it with the routine body.
        my $code := $*DECLARAND;
        $*W.attach_signature($code, $signature);
        $*W.finish_code_object($code, $block, $*MULTINESS eq 'proto', :yada(is_yada($/)));

        # attach return type
        if $*OFTYPE {
            my $sig := $code.signature;
            if $sig.has_returns {
                my $prev_returns := $sig.returns;
                $*W.throw($*OFTYPE, 'X::Redeclaration',
                    what    => 'return type for',
                    symbol  => $code,
                    postfix => " (previous return type was " 
                                ~ $prev_returns.HOW.name($prev_returns)
                                ~ ')',
                );
            }
            $sig.set_returns($*OFTYPE.ast);
        }

        # Document it
        Perl6::Pod::document($/, $code, $*DOC);

        # Install PAST block so that it gets capture_lex'd correctly and also
        # install it in the lexpad.
        my $outer := $*W.cur_lexpad();
        $outer[0].push(QAST::Stmt.new($block));

        # Install &?ROUTINE.
        $*W.install_lexical_symbol($block, '&?ROUTINE', $code);

        my $past;
        if $<deflongname> {
            # If it's a multi, need to associate it with the surrounding
            # proto.
            # XXX Also need to auto-multi things with a proto in scope.
            my $name := '&' ~ ~$<deflongname>.ast;
            if $*MULTINESS eq 'multi' {
                # Do we have a proto in the current scope?
                my $proto;
                if $outer.symbol($name) {
                    $proto := $outer.symbol($name)<value>;
                }
                else {
                    unless $*SCOPE eq '' || $*SCOPE eq 'my' {
                        $*W.throw($/, 'X::Declaration::Scope::Multi',
                            scope       => $*SCOPE,
                            declaration => 'multi',
                        );
                    }
                    # None; search outer scopes.
                    my $new_proto;
                    try {
                        $proto := $*W.find_symbol([$name]);
                    }
                    if $proto && $proto.is_dispatcher {
                        # Found in outer scope. Need to derive.
                        $new_proto := $*W.derive_dispatcher($proto);
                    }
                    else {
                        $new_proto := self.autogenerate_proto($/, $block.name, $outer[0]);
                    }

                    # Install in current scope.
                    $*W.install_lexical_symbol($outer, $name, $new_proto, :clone(1));
                    $proto := $new_proto;
                }

                # Ensure it's actually a dispatcher.
                unless nqp::can($proto, 'is_dispatcher') && $proto.is_dispatcher {
                    $*W.throw($/, ['X', 'Redeclaration'],
                        what    => 'routine',
                        symbol  => ~$<deflongname>.ast,
                    );
                }

                # Install the candidate.
                $*W.add_dispatchee_to_proto($proto, $code);
            }
            else {
                # Install.
                if $outer.symbol($name) {
                    $*W.throw($/, ['X', 'Redeclaration'],
                            symbol => ~$<deflongname>.ast,
                            what   => 'routine',
                    );
                }
                if $*SCOPE eq '' || $*SCOPE eq 'my' {
                    $*W.install_lexical_symbol($outer, $name, $code, :clone(1));
                }
                elsif $*SCOPE eq 'our' {
                    # Install in lexpad and in package, and set up code to
                    # re-bind it per invocation of its outer.
                    $*W.install_lexical_symbol($outer, $name, $code, :clone(1));
                    $*W.install_package_symbol($*PACKAGE, $name, $code);
                    $outer[0].push(QAST::Op.new(
                        :op('bindkey'),
                        QAST::Op.new( :op('who'), QAST::WVal.new( :value($*PACKAGE) ) ),
                        QAST::SVal.new( :value($name) ),
                        QAST::Var.new( :name($name), :scope('lexical') )
                    ));
                }
                elsif $*SCOPE eq 'anon' {
                    # don't do anything
                }
                else {
                    $*W.throw($/, 'X::Declaration::Scope',
                            scope       => $*SCOPE,
                            declaration => 'sub',
                    );
                }
            }
        }
        elsif $*MULTINESS {
            $*W.throw($/, 'X::Anon::Multi', multiness => $*MULTINESS);
        }

        # Apply traits.
        for $<trait> -> $t {
            if $t.ast { $*W.ex-handle($t, { ($t.ast)($code) }) }
        }
        if $<onlystar> {
            # Protect with try; won't work when declaring the initial
            # trait_mod proto in CORE.setting!
            try $*W.apply_trait($/, '&trait_mod:<is>', $*DECLARAND, :onlystar(1));
        }

        # Handle any phasers.
        $*W.add_phasers_handling_code($code, $block);

        # Add inlining information if it's inlinable; also mark soft if the
        # appropriate pragma is in effect.
        if $<deflongname> {
            if $*SOFT {
                $*W.find_symbol(['&infix:<does>'])($code, $*W.find_symbol(['SoftRoutine']));
            }
            else {
                self.add_inlining_info_if_possible($/, $code, $block, @params);
            }
        }
        
        # If it's a proto, add it to the sort-at-CHECK-time queue.
        if $*MULTINESS eq 'proto' {
            $*W.add_proto_to_sort($code);
        }

        my $closure := block_closure(reference_to_code_object($code, $past));
        $closure<sink_past> := QAST::Op.new( :op('null') );
        make $closure;
    }
    
    method autogenerate_proto($/, $name, $install_in) {
        my $p_past := $*W.push_lexpad($/);
        $p_past.name(~$name);
        $p_past.push(QAST::Op.new(
            :op('invokewithcapture'),
            QAST::Op.new(
                :op('ifnull'),
                QAST::Op.new(
                    :op('multicachefind'),
                    QAST::Var.new(
                        :name('$!dispatch_cache'), :scope('attribute'),
                        QAST::Op.new( :op('getcodeobj'), QAST::Op.new( :op('curcode') ) ),
                        QAST::WVal.new( :value($*W.find_symbol(['Routine'])) ),
                    ),
                    QAST::Op.new( :op('usecapture') )
                ),
                QAST::Op.new(
                    :op('callmethod'), :name('find_best_dispatchee'),
                    QAST::Op.new( :op('getcodeobj'), QAST::Op.new( :op('curcode') ) ),
                    QAST::Op.new( :op('savecapture') )
                ),
            ),
            QAST::Op.new( :op('usecapture') )
        ));
        $*W.pop_lexpad();
        $install_in.push(QAST::Stmt.new($p_past));
        my @p_params := [hash(is_capture => 1, nominal_type => $*W.find_symbol(['Mu']) )];
        my $p_sig := $*W.create_signature(nqp::hash('parameters', [$*W.create_parameter(@p_params[0])]));
        add_signature_binding_code($p_past, $p_sig, @p_params);
        my $code := $*W.create_code_object($p_past, 'Sub', $p_sig, 1);
        $*W.apply_trait($/, '&trait_mod:<is>', $code, :onlystar(1));
        $*W.add_proto_to_sort($code);
        $code
    }
    
    method add_inlining_info_if_possible($/, $code, $past, @params) {
        # Make sure the block has the common structure we expect
        # (decls then statements).
        return 0 unless +@($past) == 2;

        # Ensure all parameters are simple and build placeholders for
        # them.
        my %arg_placeholders;
        my int $arg_num := 0;
        for @params {
            return 0 if $_<optional> || $_<is_capture> || $_<pos_slurpy> ||
                $_<named_slurpy> || $_<pos_lol> || $_<bind_attr> ||
                $_<bind_accessor> || $_<nominal_generic> || $_<named_names> ||
                $_<type_captures> || $_<post_constraints>;
            %arg_placeholders{$_<variable_name>} :=
                QAST::InlinePlaceholder.new( :position($arg_num) );
            $arg_num := $arg_num + 1;
        }

        # Ensure nothing extra is declared.
        for @($past[0]) {
            if nqp::istype($_, QAST::Var) && $_.scope eq 'lexical' {
                my $name := $_.name;
                return 0 if $name ne '$*DISPATCHER' && $name ne '$_' &&
                    $name ne '$/' && $name ne '$!' && $name ne '&?ROUTINE' &&
                    !nqp::existskey(%arg_placeholders, $name);
            }
        }

        # If all is well, we try to build the QAST for inlining. This dies
        # if we fail.
        my $PseudoStash;
        try $PseudoStash := $*W.find_symbol(['PseudoStash']);
        sub clear_node($qast) {
            $qast.node(nqp::null());
            $qast
        }
        sub clone_qast($qast) {
            my $cloned := nqp::clone($qast);
            nqp::bindattr($cloned, QAST::Node, '@!array',
                nqp::clone(nqp::getattr($cloned, QAST::Node, '@!array')));
            $cloned
        }
        sub node_walker($node) {
            # Simple values are always fine; just return them as they are, modulo
            # removing any :node(...).
            if nqp::istype($node, QAST::IVal) || nqp::istype($node, QAST::SVal)
            || nqp::istype($node, QAST::NVal) {
                return $node.node ?? clear_node(clone_qast($node)) !! $node;
            }
            
            # WVal is OK, though special case for PseudoStash usage (which means
            # we are doing funny lookup stuff).
            elsif nqp::istype($node, QAST::WVal) {
                if $node.value =:= $PseudoStash {
                    nqp::die("Routines using pseudo-stashes are not inlinable");
                }
                else {
                    return $node.node ?? clear_node(clone_qast($node)) !! $node;
                }
            }
            
            # Operations need checking for their inlinability. If they are OK in
            # themselves, it comes down to the children.
            elsif nqp::istype($node, QAST::Op) {
                if nqp::getcomp('QAST').operations.is_inlinable('perl6', $node.op) {
                    my $replacement := clone_qast($node);
                    my int $i := 0;
                    my int $n := +@($node);
                    while $i < $n {
                        $replacement[$i] := node_walker($node[$i]);
                        $i := $i + 1;
                    }
                    return clear_node($replacement);
                }
                else {
                    nqp::die("Non-inlinable op '" ~ $node.op ~ "' encountered");
                }
            }
            
            # Variables are fine *if* they are arguments.
            elsif nqp::istype($node, QAST::Var) && ($node.scope eq 'lexical' || $node.scope eq '') {
                if nqp::existskey(%arg_placeholders, $node.name) {
                    my $replacement := %arg_placeholders{$node.name};
                    if $node.named || $node.flat {
                        $replacement := clone_qast($replacement);
                        if $node.named { $replacement.named($node.named) }
                        if $node.flat { $replacement.flat($node.flat) }
                    }
                    return $replacement;
                }
                else {
                    nqp::die("Cannot inline with non-argument variables");
                }
            }
            
            # Statements need to be cloned and then each of the nodes below them
            # visited.
            elsif nqp::istype($node, QAST::Stmt) || nqp::istype($node, QAST::Stmts) {
                my $replacement := clone_qast($node);
                my int $i := 0;
                my int $n := +@($node);
                while $i < $n {
                    $replacement[$i] := node_walker($node[$i]);
                    $i := $i + 1;
                }
                return clear_node($replacement);
            }
            
            # Want nodes need copying and every other child visiting.
            elsif nqp::istype($node, QAST::Want) {
                my $replacement := clone_qast($node);
                my int $i := 0;
                my int $n := +@($node);
                while $i < $n {
                    $replacement[$i] := node_walker($node[$i]);
                    $i := $i + 2;
                }
                return clear_node($replacement);
            }
            
            # Otherwise, we don't know what to do with it.
            else {
                nqp::die("Unhandled node type; won't inline");
            }
        };
        my $inline_info;
        try $inline_info := node_walker($past[1]);
        return 0 unless nqp::istype($inline_info, QAST::Node);

        # Attach inlining information.
        $*W.apply_trait($/, '&trait_mod:<is>', $code, inlinable => $inline_info)
    }

    method method_def($/) {
        my $past;
        if $<onlystar> {
            $past := $<onlystar>.ast;
        }
        else {
            $past := $<blockoid>.ast;
            if is_clearly_returnless($past) {
                $past[1] := wrap_return_type_check(
                    QAST::Op.new( :op('p6decontrv'), QAST::WVal.new( :value($*DECLARAND) ), $past[1] ),
                    $*DECLARAND);
            }
            else {
                $past[1] := wrap_return_handler($past[1]);
            }
        }
        $past.blocktype('declaration_static');

        my $name;
        if $<longname> {
            my $longname := $*W.dissect_longname($<longname>);
            $name := $longname.name(:dba('method name'),
                            :decl<routine>, :with_adverbs);
        }
        elsif $<sigil> {
            if $<sigil> eq '@'    { $name := 'postcircumfix:<[ ]>' }
            elsif $<sigil> eq '%' { $name := 'postcircumfix:<{ }>' }
            elsif $<sigil> eq '&' { $name := 'postcircumfix:<( )>' }
            else {
                $/.CURSOR.panic("Cannot use " ~ $<sigil> ~ " sigil as a method name");
            }
        }
        $past.name($name ?? $name !! '<anon>');

        # Do the various tasks to trun the block into a method code object.
        my %sig_info := $<multisig> ?? $<multisig>.ast !! hash(parameters => []);
        my $inv_type  := $*W.find_symbol([
            $<longname> && $*W.is_lexical('$?CLASS') ?? '$?CLASS' !! 'Mu']);
        my $code := methodize_block($/, $*DECLARAND, $past, %sig_info, $inv_type, :yada(is_yada($/)));

        # If it's a proto but not an onlystar, need some variables for the
        # {*} implementation to use.
        if $*MULTINESS eq 'proto' && !$<onlystar> {
            # Also stash the current lexical dispatcher and capture, for the {*}
            # to resolve.
            $past[0].push(QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('CURRENT_DISPATCH_CAPTURE'), :scope('lexical'), :decl('var') ),
                QAST::Op.new( :op('savecapture') )
            ));
            $past[0].push(QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('&*CURRENT_DISPATCHER'), :scope('lexical'), :decl('var') ),
                QAST::Op.new( :op('getcodeobj'), QAST::Op.new( :op('curcode') ) )
            ));
        }
        
        # Document it
        Perl6::Pod::document($/, $code, $*DOC);

        # Install &?ROUTINE.
        $*W.install_lexical_symbol($past, '&?ROUTINE', $code);

        # Install PAST block so that it gets capture_lex'd correctly.
        my $outer := $*W.cur_lexpad();
        $outer[0].push($past);

        # Apply traits.
        for $<trait> {
            if $_.ast { ($_.ast)($code) }
        }
        if $<onlystar> {
            $*W.apply_trait($/, '&trait_mod:<is>', $*DECLARAND, :onlystar(1));
        }
        $*W.add_phasers_handling_code($code, $past);

        # Install method.
        if $name {
            install_method($/, $name, $*SCOPE, $code, $outer,
                :private($<specials> && ~$<specials> eq '!'));
        }
        elsif $*MULTINESS {
            $*W.throw($/, 'X::Anon::Multi',
                multiness       => $*MULTINESS,
                routine-type    => 'method',
            );
        }
        
        # If it's a proto, add it to the sort-at-CHECK-time queue.
        if $*MULTINESS eq 'proto' {
            $*W.add_proto_to_sort($code);
        }

        my $closure := block_closure(reference_to_code_object($code, $past));
        $closure<sink_past> := QAST::Op.new( :op('null') );
        make $closure;
    }

    method macro_def($/) {
        my $block;

        $block := $<blockoid>.ast;
        $block.blocktype('declaration_static');
        if is_clearly_returnless($block) {
            $block[1] := QAST::Op.new(
                :op('p6decontrv'),
                QAST::WVal.new( :value($*DECLARAND) ),
                $block[1]);
        }
        else {
            $block[1] := wrap_return_handler($block[1]);
        }

        # Obtain parameters, create signature object and generate code to
        # call binder.
        if $block<placeholder_sig> && $<multisig> {
            $*W.throw($/, 'X::Signature::Placeholder',
                placeholder => $block<placeholder_sig>[0]<placeholder>,
            );
        }
        my %sig_info;
        if $<multisig> {
            %sig_info := $<multisig>.ast;
        }
        else {
            %sig_info<parameters> := $block<placeholder_sig> ?? $block<placeholder_sig> !!
                                                                [];
        }
        my @params := %sig_info<parameters>;
        set_default_parameter_type(@params, 'Any');
        my $signature := create_signature_object($<multisig> ?? $<multisig> !! $/, %sig_info, $block);
        add_signature_binding_code($block, $signature, @params);

        # Finish code object, associating it with the routine body.
        if $<deflongname> {
            $block.name(~$<deflongname>.ast);
        }
        my $code := $*DECLARAND;
        $*W.attach_signature($code, $signature);
        $*W.finish_code_object($code, $block, $*MULTINESS eq 'proto');

        # Document it
        Perl6::Pod::document($/, $code, $*DOC);

        # Install PAST block so that it gets capture_lex'd correctly and also
        # install it in the lexpad.
        my $outer := $*W.cur_lexpad();
        $outer[0].push(QAST::Stmt.new($block));

        # Install &?ROUTINE.
        $*W.install_lexical_symbol($block, '&?ROUTINE', $code);

        my $past;
        if $<deflongname> {
            my $name := '&' ~ ~$<deflongname>.ast;
            # Install.
            if $outer.symbol($name) {
                $/.CURSOR.panic("Illegal redeclaration of macro '" ~
                    ~$<deflongname>.ast ~ "'");
            }
            if $*SCOPE eq '' || $*SCOPE eq 'my' {
                $*W.install_lexical_symbol($outer, $name, $code);
            }
            elsif $*SCOPE eq 'our' {
                # Install in lexpad and in package, and set up code to
                # re-bind it per invocation of its outer.
                $*W.install_lexical_symbol($outer, $name, $code);
                $*W.install_package_symbol($*PACKAGE, $name, $code);
                $outer[0].push(QAST::Op.new(
                    :op('bind'),
                    $*W.symbol_lookup([$name], $/, :package_only(1)),
                    QAST::Var.new( :name($name), :scope('lexical') )
                ));
            }
            else {
                $/.CURSOR.panic("Cannot use '$*SCOPE' scope with a macro");
            }
        }
        elsif $*MULTINESS {
            $/.CURSOR.panic('Cannot put ' ~ $*MULTINESS ~ ' on anonymous macro');
        }

        # Apply traits.
        for $<trait> {
            if $_.ast { ($_.ast)($code) }
        }
        $*W.add_phasers_handling_code($code, $past);

        my $closure := block_closure(reference_to_code_object($code, $past));
        $closure<sink_past> := QAST::Op.new( :op('null') );
        make $closure;
    }

    sub methodize_block($/, $code, $past, %sig_info, $invocant_type, :$yada) {
        # Get signature and ensure it has an invocant and *%_ if needed.
        my @params := %sig_info<parameters>;
        if $past<placeholder_sig> {
            $/.CURSOR.panic('Placeholder variables cannot be used in a method');
        }
        unless @params[0]<is_invocant> {
            @params.unshift(hash(
                nominal_type => $invocant_type,
                is_invocant => 1,
                is_multi_invocant => 1
            ));
        }
        unless @params[+@params - 1]<named_slurpy> || @params[+@params - 1]<is_capture> {
            unless nqp::can($*PACKAGE.HOW, 'hidden') && $*PACKAGE.HOW.hidden($*PACKAGE) {
                @params.push(hash(
                    variable_name => '%_',
                    nominal_type => $*W.find_symbol(['Mu']),
                    named_slurpy => 1,
                    is_multi_invocant => 1,
                    sigil => '%'
                ));
                $past[0].unshift(QAST::Var.new( :name('%_'), :scope('lexical'), :decl('var') ));
                $past.symbol('%_', :scope('lexical'));
            }
        }
        set_default_parameter_type(@params, 'Any');
        my $signature := create_signature_object($/, %sig_info, $past);
        add_signature_binding_code($past, $signature, @params);

        # Place to store invocant.
        $past[0].unshift(QAST::Var.new( :name('self'), :scope('lexical'), :decl('var') ));
        $past.symbol('self', :scope('lexical'));

        # Needs a slot to hold a multi or method dispatcher.
        $*W.install_lexical_symbol($past, '$*DISPATCHER',
            $*W.find_symbol([$*MULTINESS eq 'multi' ?? 'MultiDispatcher' !! 'MethodDispatcher']));
        $past[0].push(QAST::Op.new(
            :op('takedispatcher'),
            QAST::SVal.new( :value('$*DISPATCHER') )
        ));
        
        # Finish up code object.
        $*W.attach_signature($code, $signature);
        $*W.finish_code_object($code, $past, $*MULTINESS eq 'proto', :yada($yada));
        $*W.add_phasers_handling_code($code, $past);
        $code
    }

    # Installs a method into the various places it needs to go.
    sub install_method($/, $name, $scope, $code, $outer, :$private) {
        # Ensure that current package supports methods, and if so
        # add the method.
        my $meta_meth;
        if $private {
            if $*MULTINESS { $/.CURSOR.panic("Private multi-methods are not supported"); }
            $meta_meth := 'add_private_method';
        }
        else {
            $meta_meth := $*MULTINESS eq 'multi' ?? 'add_multi_method' !! 'add_method';
        }
        if $scope eq '' || $scope eq 'has' {
            if nqp::can($*PACKAGE.HOW, $meta_meth) {
                $*W.pkg_add_method($/, $*PACKAGE, $meta_meth, $name, $code);
            }
            else {
                my $nocando := $*MULTINESS eq 'multi' ?? 'multi-method' !! 'method';
                nqp::printfh(nqp::getstderr(),
                    "Useless declaration of a has-scoped $nocando in " ~
                    ($*PKGDECL || "mainline") ~ " (did you mean 'my $*METHODTYPE $name'?)\n");
            }
        }

        # May also need it in lexpad and/or package.
        if $*SCOPE eq 'my' {
            $*W.install_lexical_symbol($outer, '&' ~ $name, $code, :clone(1));
        }
        elsif $*SCOPE eq 'our' {
            $*W.install_lexical_symbol($outer, '&' ~ $name, $code, :clone(1));
            $*W.install_package_symbol($*PACKAGE, '&' ~ $name, $code);
        }
    }

    sub is_clearly_returnless($block) {
        sub returnless_past($past) {
            return 0 unless
                # It's a simple operation.
                nqp::istype($past, QAST::Op)
                    && nqp::getcomp('QAST').operations.is_inlinable('perl6', $past.op) ||
                # Just a variable lookup.
                nqp::istype($past, QAST::Var) ||
                # Just a QAST::Want
                nqp::istype($past, QAST::Want) ||
                # Just a primitive or world value.
                nqp::istype($past, QAST::WVal) ||
                nqp::istype($past, QAST::IVal) ||
                nqp::istype($past, QAST::NVal) ||
                nqp::istype($past, QAST::SVal);
            for @($past) {
                if nqp::istype($_, QAST::Node) {
                    if !returnless_past($_) {
                        return 0;
                    }
                }
            }
            1;
        }
        
        # Only analyse things with a single simple statement.
        if +$block[1].list == 1 && nqp::istype($block[1][0], QAST::Stmt) && +$block[1][0].list == 1 {
            # Ensure there's no nested blocks.
            for @($block[0]) {
                if nqp::istype($_, QAST::Block) { return 0; }
                if nqp::istype($_, QAST::Stmts) {
                    for @($_) {
                        if nqp::istype($_, QAST::Block) { return 0; }
                    }
                }
            }

            # Ensure that the PAST is whitelisted things.
            returnless_past($block[1][0][0])
        }
        else {
            0
        }
    }
    
    sub is_yada($/) {
        if $<blockoid><statementlist> && +$<blockoid><statementlist><statement> == 1 {
            my $btxt := ~$<blockoid><statementlist><statement>[0];
            if $btxt ~~ /^ \s* ['...'|'???'|'!!!'] \s* $/ {
                return 1;
            }
        }
        0
    }

    method onlystar($/) {
        my $BLOCK := $*CURPAD;

        # Remove special variables; no need for them in onlystar.
        my int $i := 0;
        my int $n := +@($BLOCK[0]);
        while $i < $n {
            my $consider := $BLOCK[0][$i];
            if nqp::istype($consider, QAST::Var) {
                my $name := $consider.name;
                if $name eq '$_' || $name eq '$/' || $name eq '$!' {
                    $BLOCK[0][$i] := QAST::Op.new( :op('null') );
                }
            }
            $i++;
        }

        # Add dispatching code.
        $BLOCK.push(QAST::Op.new(
            :op('invokewithcapture'),
            QAST::Op.new(
                :op('ifnull'),
                QAST::Op.new(
                    :op('multicachefind'),
                    QAST::Var.new(
                        :name('$!dispatch_cache'), :scope('attribute'),
                        QAST::Op.new( :op('getcodeobj'), QAST::Op.new( :op('curcode') ) ),
                        QAST::WVal.new( :value($*W.find_symbol(['Routine'])) ),
                    ),
                    QAST::Op.new( :op('usecapture') )
                ),
                QAST::Op.new(
                    :op('callmethod'), :name('find_best_dispatchee'),
                    QAST::Op.new( :op('getcodeobj'), QAST::Op.new( :op('curcode') ) ),
                    QAST::Op.new( :op('savecapture') )
                )
            ),
            QAST::Op.new( :op('usecapture') )
        ));
        $BLOCK.node($/);
        $BLOCK.is_thunk(1);
        make $BLOCK;
    }

    method regex_declarator:sym<regex>($/, $key?) {
        make $<regex_def>.ast;
    }

    method regex_declarator:sym<token>($/, $key?) {
        make $<regex_def>.ast;
    }

    method regex_declarator:sym<rule>($/, $key?) {
        make $<regex_def>.ast;
    }

    method regex_def($/) {
        my $coderef;
        my $name := ~%*RX<name>;
        $*CURPAD.blocktype('declaration_static');
        my %sig_info := $<signature> ?? $<signature>[0].ast !! hash(parameters => []);

        if $*MULTINESS eq 'proto' {
            unless $<onlystar> {
                $/.CURSOR.panic("Proto regex body must be \{*\} (or <*> or <...>, which are deprecated)");
            }
            my $proto_body := QAST::Op.new(
                :op('callmethod'), :name('!protoregex'),
                QAST::Var.new( :name('self'), :scope('local') ),
                QAST::SVal.new( :value($name) ));
            $coderef := regex_coderef($/, $*DECLARAND, $proto_body, $*SCOPE, $name, %sig_info, $*CURPAD, $<trait>, :proto(1));
        } else {
            $coderef := regex_coderef($/, $*DECLARAND, $<nibble>.ast, $*SCOPE, $name, %sig_info, $*CURPAD, $<trait>) if $<nibble>.ast;
        }

        # Install &?ROUTINE.
        $*W.install_lexical_symbol($*CURPAD, '&?ROUTINE', $*DECLARAND);

        # Return closure if not in sink context.
        my $closure := block_closure($coderef);
        $closure<sink_past> := QAST::Op.new( :op('null') );
        make $closure;
    }

    sub regex_coderef($/, $code, $qast, $scope, $name, %sig_info, $block, $traits?, :$proto, :$use_outer_match) {
        # create a code reference from a regex qast tree
        my $past;
        if $proto {
            $block[1] := $qast;
            $past := $block;
        }
        else {
            $block[0].unshift(QAST::Var.new(:name<$¢>, :scope<lexical>, :decl('var')));
            $block.symbol('$¢', :scope<lexical>);
            unless $use_outer_match {
                $*W.install_lexical_magical($block, '$/');
            }
            $past := %*RX<P5>
                ?? %*LANG<P5Regex-actions>.qbuildsub($qast, $block, code_obj => $code)
                !! %*LANG<Regex-actions>.qbuildsub($qast, $block, code_obj => $code);
        }
        $past.name($name);
        $past.blocktype("declaration_static");
        
        # Install a $?REGEX (mostly for the benefit of <~~>).
        $block[0].push(QAST::Op.new(
            :op('bind'),
            QAST::Var.new(:name<$?REGEX>, :scope<lexical>, :decl('var')),
            QAST::Op.new(
                :op('getcodeobj'),
                QAST::Op.new( :op('curcode') )
            )));
        $block.symbol('$?REGEX', :scope<lexical>);

        # Do the various tasks to turn the block into a method code object.
        my $inv_type  := $*W.find_symbol([ # XXX Maybe Cursor below, not Mu...
            $name && $*SCOPE ne 'my' && $*W.is_lexical('$?CLASS') ?? '$?CLASS' !! 'Mu']);
        methodize_block($/, $code, $past, %sig_info, $inv_type);

        # Need to put self into a register for the regex engine.
        $past[0].push(QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name('self'), :scope('local'), :decl('var') ),
            QAST::Var.new( :name('self'), :scope('lexical') )));

        # Install PAST block so that it gets capture_lex'd correctly.
        my $outer := $*W.cur_lexpad();
        $outer[0].push($past);
        
        # Apply traits.
        if $traits {
            for $traits {
                if $_.ast { ($_.ast)($code) }
            }
        }
        
        # Install in needed scopes.
        install_method($/, $name, $scope, $code, $outer) if $name ne '';

        # Return a reference to the code object
        reference_to_code_object($code, $past);
    }

    method type_declarator:sym<enum>($/) {
        # If it's an anonymous enum, just call anonymous enum former
        # and we're done.
        unless $<longname> || $<variable> {
            make QAST::Op.new( :op('call'), :name('&ANON_ENUM'), $<term>.ast );
            return 1;
        }

        # Get, or find, enumeration base type and create type object with
        # correct base type.
        my $longname  := $<longname> ?? $*W.dissect_longname($<longname>) !! 0;
        my $name      := $<longname> ?? $longname.name() !! $<variable><desigilname>;

        my $type_obj;
        my sub make_type_obj($base_type) {
            $type_obj := $*W.pkg_create_mo($/, %*HOW<enum>, :$name, :$base_type);
            # Add roles (which will provide the enum-related methods).
            $*W.apply_trait($/, '&trait_mod:<does>', $type_obj, $*W.find_symbol(['Enumeration']));
            if istype($type_obj, $*W.find_symbol(['Numeric'])) {
                $*W.apply_trait($/, '&trait_mod:<does>', $type_obj, $*W.find_symbol(['NumericEnumeration']));
            }
            if istype($type_obj, $*W.find_symbol(['Stringy'])) {
                $*W.apply_trait($/, '&trait_mod:<does>', $type_obj, $*W.find_symbol(['StringyEnumeration']));
            }
            # Apply traits, compose and install package.
            for $<trait> {
                ($_.ast)($type_obj) if $_.ast;
            }
            $*W.pkg_compose($type_obj);
        }
        my $base_type;
        my int $has_base_type := 0;
        if $*OFTYPE {
            $base_type     := $*OFTYPE.ast;
            $has_base_type := 1;
            make_type_obj($base_type);
        }

        if $<variable> {
            $*W.throw($/, 'X::Comp::NYI',
                feature => "Variable case of enums",
            );
        }

        # Get list of either values or pairs; fail if we can't.
        my $Pair := $*W.find_symbol(['Pair']);
        my @values;
        my $term_ast := $<term>.ast;
        if $term_ast.isa(QAST::Stmts) && +@($term_ast) == 1 {
            $term_ast := $term_ast[0];
        }
        if $term_ast.isa(QAST::Op) && $term_ast.name eq '&infix:<,>' {
            for @($term_ast) {
                if istype($_.returns(), $Pair) && $_[1].has_compile_time_value {
                    @values.push($_);
                }
                elsif $_.has_compile_time_value {
                    @values.push($_);
                }
                else {
                    @values.push($*W.compile_time_evaluate($<term>, $_));
                }
            }
        }
        elsif $term_ast.has_compile_time_value {
            @values.push($term_ast);
        }
        elsif istype($term_ast.returns, $Pair) && $term_ast[1].has_compile_time_value {
            @values.push($term_ast);
        }
        else {
            @values.push($*W.compile_time_evaluate($<term>, $<term>.ast));
        }

        # Now we have them, we can go about computing the value
        # for each of the keys, unless they have them supplied.
        # XXX Should not assume integers, and should use lexically
        # scoped &postfix:<++> or so.
        my $cur_value := nqp::box_i(-1, $*W.find_symbol(['Int']));
        for @values {
            # If it's a pair, take that as the value; also find
            # key.
            my $cur_key;
            if nqp::istype($_, QAST::Node) && istype($_.returns(), $Pair) {
                $cur_key := $_[1].compile_time_value;
                $cur_value := $*W.compile_time_evaluate($<term>, $_[2]);
                if $has_base_type {
                    unless istype($cur_value, $base_type) {
                        $/.CURSOR.panic("Type error in enum. Got '"
                                ~ $cur_value.HOW.name($cur_value)
                                ~ "' Expected: '"
                                ~ $base_type.HOW.name($base_type)
                                ~ "'"
                        );
                    }
                }
                else {
                    $base_type     :=  $cur_value.WHAT;
                    $has_base_type := 1;
                    make_type_obj($base_type);
                }
            }
            else {
                unless $has_base_type {
                    $base_type := $*W.find_symbol(['Int']);
                    make_type_obj($base_type);
                    $has_base_type := 1;
                }

                if nqp::istype($_, QAST::Node) {
                    $cur_key := $*W.compile_time_evaluate($<term>, $_);
                }
                else {
                    $cur_key := $_;
                }

                $cur_value := $cur_value.succ();
            }
            unless nqp::istype($cur_key, $*W.find_symbol(['Str'])) {
                $cur_key := $cur_key.Str;
            }

            # Create and install value.
            my $val_obj := $*W.create_enum_value($type_obj, $cur_key, $cur_value);
            $*W.install_package_symbol($type_obj, nqp::unbox_s($cur_key), $val_obj);
            if $*SCOPE ne 'anon' {
                $*W.install_lexical_symbol($*W.cur_lexpad(), nqp::unbox_s($cur_key), $val_obj);
            }
            if $*SCOPE eq '' || $*SCOPE eq 'our' {
                $*W.install_package_symbol($*PACKAGE, nqp::unbox_s($cur_key), $val_obj);
            }
        }
        # create a type object even for empty enums
        make_type_obj($*W.find_symbol(['Int'])) unless $has_base_type;

        $*W.install_package($/, $longname.type_name_parts('enum name', :decl(1)),
            ($*SCOPE || 'our'), 'enum', $*PACKAGE, $*W.cur_lexpad(), $type_obj);

        # We evaluate to the enum type object.
        make QAST::WVal.new( :value($type_obj) );
    }

    method type_declarator:sym<subset>($/) {
        # We refine Any by default; "of" may override.
        my $refinee := $*W.find_symbol(['Any']);

        # If we have a refinement, make sure it's thunked if needed. If none,
        # just always true.
        my $refinement := make_where_block($<EXPR> ?? $<EXPR>.ast !!
            QAST::Op.new( :op('p6bool'), QAST::IVal.new( :value(1) ) ));

        # Create the meta-object.
        my $longname := $<longname> ?? $*W.dissect_longname($<longname>) !! 0;
        my $subset := $<longname> ??
            $*W.create_subset(%*HOW<subset>, $refinee, $refinement, :name($longname.name())) !!
            $*W.create_subset(%*HOW<subset>, $refinee, $refinement);

        # Apply traits.
        for $<trait> {
            ($_.ast)($subset) if $_.ast;
        }

        # Install it as needed.
        if $<longname> && $longname.type_name_parts('subset name', :decl(1)) {
            $*W.install_package($/, $longname.type_name_parts('subset name', :decl(1)),
                ($*SCOPE || 'our'), 'subset', $*PACKAGE, $*W.cur_lexpad(), $subset);
        }

        # We evaluate to the refinement type object.
        make QAST::WVal.new( :value($subset) );
    }

    method type_declarator:sym<constant>($/) {
        # Get constant value.
        my $con_block := $*W.pop_lexpad();
        my $value_ast := $<initializer>.ast;
        my $value;
        if $value_ast.has_compile_time_value {
            $value := $value_ast.compile_time_value;
        }
        else {
            $con_block.push($value_ast);
            my $value_thunk := $*W.create_simple_code_object($con_block, 'Block');
            $value := $value_thunk();
            $*W.add_constant_folded_result($value);
        }

        # Provided it's named, install it.
        my $name;
        if $<defterm> {
            $name := $<defterm>.ast;
        }
        elsif $<variable> {
            # Don't handle twigil'd case yet.
            if $<variable><twigil> {
                $*W.throw($/, 'X::Comp::NYI',
                    feature => "Twigil-Variable constants"
                );
            }
            $name := ~$<variable>;
        }
        if $name {
            $*W.install_package($/, [$name], ($*SCOPE || 'our'),
                'constant', $*PACKAGE, $*W.cur_lexpad(), $value);
        }
        $*W.ex-handle($/, {
            for $<trait> -> $t {
                ($t.ast)($value, :SYMBOL($name));
            }
        });

        # Evaluate to the constant.
        make QAST::WVal.new( :value($value) );
    }
    
    method initializer:sym<=>($/) {
        make $<EXPR>.ast;
    }
    method initializer:sym<:=>($/) {
        make $<EXPR>.ast;
    }
    method initializer:sym<::=>($/) {
        make $<EXPR>.ast;
    }
    method initializer:sym<.=>($/) {
        make $<dottyopish><term>.ast;
    }

    method capterm($/) {
        # Construct a Parcel, and then call .Capture to coerce it to a capture.
        my $past := $<termish> ?? $<termish>.ast !!
                    $<capture> ?? $<capture>.ast !!
                    QAST::Op.new( :op('call'), :name('&infix:<,>') );
        unless $past.isa(QAST::Op) && $past.name eq '&infix:<,>' {
            $past := QAST::Op.new( :op('call'), :name('&infix:<,>'), $past );
        }
        make QAST::Op.new( :op('callmethod'), :name('Capture'), $past);
    }

    method capture($/) {
        make $<EXPR>.ast;
    }

    method multisig($/) {
        make $<signature>.ast;
    }

    method fakesignature($/) {
        my $fake_pad := $*W.pop_lexpad();
        my %sig_info := $<signature>.ast;
        my @params := %sig_info<parameters>;
        set_default_parameter_type(@params, 'Mu');
        my $sig := create_signature_object($/, %sig_info, $fake_pad, :no_attr_check(1));

        $*W.cur_lexpad()[0].push($fake_pad);
        $*W.create_code_object($fake_pad, 'Block', $sig);
        
        make QAST::WVal.new( :value($sig) );
    }

    method signature($/) {
        # Fix up parameters with flags according to the separators.
        # TODO: Handle $<typename>, which contains the return type declared
        # with the --> syntax.
        my %signature;
        my @parameter_infos;
        my int $param_idx := 0;
        my int $multi_invocant := 1;
        for $<parameter> {
            my %info := $_.ast;
            %info<is_multi_invocant> := $multi_invocant;
            my $sep := @*seps[$param_idx];
            if ~$sep eq ':' {
                if $param_idx != 0 {
                    $*W.throw($/, 'X::Syntax::Signature::InvocantMarker')
                }
                %info<is_invocant> := 1;
            }
            elsif ~$sep eq ';;' {
                $multi_invocant := 0;
            }
            @parameter_infos.push(%info);
            $param_idx := $param_idx + 1;
        }
        %signature<parameters> := @parameter_infos;
        if $<typename> {
            %signature<returns> := $<typename>.ast;
        }

        # Mark current block as having a signature.
        $*W.mark_cur_lexpad_signatured();

        # Result is set of parameter descriptors.
        make %signature;
    }

    method parameter($/) {
        # If it's a defterm, need to do parameter setup here.
        if $<defterm> {
            my $name := $<defterm>.ast;
            %*PARAM_INFO<variable_name> := $name;
            %*PARAM_INFO<desigilname>   := $name;
            %*PARAM_INFO<sigil>         := '';
            self.declare_param($/, $name);
        }
        
        # Sanity checks.
        my $quant := $<quant>;
        if $<default_value> {
            my $name := %*PARAM_INFO<variable_name> // '';
            if $quant eq '*' {
                $/.CURSOR.typed_sorry('X::Parameter::Default', how => 'slurpy',
                            parameter => $name);
            }
            if $quant eq '!' {
                $/.CURSOR.typed_sorry('X::Parameter::Default', how => 'required',
                            parameter => $name);
            }
            my $val := $<default_value>[0].ast;
            if $val.has_compile_time_value {
                %*PARAM_INFO<default_value> := $val.compile_time_value;
                %*PARAM_INFO<default_is_literal> := 1;
            }
            else {
                %*PARAM_INFO<default_value> :=
                    $*W.create_thunk($<default_value>[0], $val);
            }
        }

        # Set up various flags.
        %*PARAM_INFO<pos_slurpy>   := $quant eq '*' && %*PARAM_INFO<sigil> eq '@';
        %*PARAM_INFO<pos_lol>      := $quant eq '**' && %*PARAM_INFO<sigil> eq '@';
        %*PARAM_INFO<named_slurpy> := $quant eq '*' && %*PARAM_INFO<sigil> eq '%';
        %*PARAM_INFO<optional>     := $quant eq '?' || $<default_value> || ($<named_param> && $quant ne '!');
        %*PARAM_INFO<is_parcel>    := $quant eq '\\';
        %*PARAM_INFO<is_capture>   := $quant eq '|';

        # Stash any traits.
        %*PARAM_INFO<traits> := $<trait>;

        if (%*PARAM_INFO<pos_slurpy> || %*PARAM_INFO<pos_lol>) && $<type_constraint> {
            $/.CURSOR.sorry("Slurpy positionals with type constraints are not supported.");
        }

        # Result is the parameter info hash.
        make %*PARAM_INFO;
    }

    method param_var($/) {
        if $<signature> {
            if nqp::existskey(%*PARAM_INFO, 'sub_signature_params') {
                $/.CURSOR.panic('Cannot have more than one sub-signature for a parameter');
            }
            %*PARAM_INFO<sub_signature_params> := $<signature>.ast;
            if nqp::substr(~$/, 0, 1) eq '[' {
                %*PARAM_INFO<sigil> := '@';
                %*PARAM_INFO<nominal_type> := $*W.find_symbol(['Positional']);
            }
        }
        else {
            # Set name, if there is one.
            if $<name> {
                %*PARAM_INFO<variable_name> := ~$/;
                %*PARAM_INFO<desigilname> := ~$<name>;
            }
            %*PARAM_INFO<sigil> := my $sigil := ~$<sigil>;

            # Depending on sigil, use appropriate role.
            my int $need_role;
            my $role_type;
            if $sigil eq '@' {
                $role_type := $*W.find_symbol(['Positional']);
                $need_role := 1;
            }
            elsif $sigil eq '%' {
                $role_type := $*W.find_symbol(['Associative']);
                $need_role := 1;
            }
            elsif $sigil eq '&' {
                $role_type := $*W.find_symbol(['Callable']);
                $need_role := 1;
            }
            if $need_role {
                if nqp::existskey(%*PARAM_INFO, 'nominal_type') {
                    %*PARAM_INFO<nominal_type> := $*W.parameterize_type_with_args(
                        $role_type, [%*PARAM_INFO<nominal_type>], nqp::hash());
                }
                else {
                    %*PARAM_INFO<nominal_type> := $role_type;
                }
            }

            # Handle twigil.
            my $twigil := $<twigil> ?? ~$<twigil> !! '';
            %*PARAM_INFO<twigil> := $twigil;
            if $twigil eq '' || $twigil eq '*' {
                # Need to add the name.
                if $<name> {
                    self.declare_param($/, ~$/);
                }
            }
            elsif $twigil eq '!' {
                %*PARAM_INFO<bind_attr>    := 1;
                my int $succ := 1;
                try {
                    %*PARAM_INFO<attr_package> := $*W.find_symbol(['$?CLASS']);
                    CATCH {
                        $succ := 0;
                    }
                }
                unless $succ {
                    $/.CURSOR.panic('cannot use a $! parameter in a signature where no $?CLASS is available');
                }
            }
            elsif $twigil eq '.' {
                %*PARAM_INFO<bind_accessor> := 1;
                if $<name> {
                    %*PARAM_INFO<variable_name> := ~$<name>;
                }
                else {
                    $/.CURSOR.panic("Cannot declare $. parameter in signature without an accessor name");
                }
            }
            else {
                if $twigil eq ':' {
                    $/.CURSOR.typed_sorry('X::Parameter::Placeholder',
                        parameter => ~$/,
                        right     => ':' ~ $<sigil> ~ ~$<name>,
                    );
                }
                else {
                    $/.CURSOR.typed_sorry('X::Parameter::Twigil',
                        parameter => ~$/,
                        twigil    => $twigil,
                    );
                }
            }
        }
    }

    method declare_param($/, $name) {
        my $cur_pad := $*W.cur_lexpad();
        if $cur_pad.symbol($name) {
            $*W.throw($/, ['X', 'Redeclaration'], symbol => $name);
        }
        if nqp::existskey(%*PARAM_INFO, 'nominal_type') {
            $cur_pad[0].push(QAST::Var.new( :$name, :scope('lexical'),
                :decl('var'), :returns(%*PARAM_INFO<nominal_type>) ));
            %*PARAM_INFO<container_descriptor> := $*W.create_container_descriptor(
                %*PARAM_INFO<nominal_type>, 0, %*PARAM_INFO<variable_name>);
            $cur_pad.symbol(%*PARAM_INFO<variable_name>, :descriptor(%*PARAM_INFO<container_descriptor>),
                :type(%*PARAM_INFO<nominal_type>));
        } else {
            $cur_pad[0].push(QAST::Var.new( :name($name), :scope('lexical'), :decl('var') ));
        }
        $cur_pad.symbol($name, :scope('lexical'));
    }

    method named_param($/) {
        %*PARAM_INFO<named_names> := %*PARAM_INFO<named_names> || [];
        if $<name>               { %*PARAM_INFO<named_names>.push(~$<name>); }
        elsif $<param_var><name> { %*PARAM_INFO<named_names>.push(~$<param_var><name>); }
        else                     { %*PARAM_INFO<named_names>.push(''); }
    }

    method default_value($/) {
        make $<EXPR>.ast;
    }

    method type_constraint($/) {
        if $<typename> {
            if nqp::substr(~$<typename>, 0, 2) eq '::' && nqp::substr(~$<typename>, 2, 1) ne '?' {
                # Set up signature so it will find the typename.
                my $desigilname := nqp::substr(~$<typename>, 2);
                unless %*PARAM_INFO<type_captures> {
                    %*PARAM_INFO<type_captures> := []
                }
                %*PARAM_INFO<type_captures>.push($desigilname);

                # Install type variable in the static lexpad. Of course,
                # we'll find the real thing at runtime, but in the static
                # view it's a type variable to be reified.
                $*W.install_lexical_symbol($*W.cur_lexpad(), $desigilname,
                    $<typename>.ast);
            }
            else {
                if nqp::existskey(%*PARAM_INFO, 'nominal_type') {
                    $*W.throw($/, ['X', 'Parameter', 'MultipleTypeConstraints'],
                        parameter => (%*PARAM_INFO<variable_name> // ''),
                    );
                }
                my $type := $<typename>.ast;
                if nqp::isconcrete($type) {
                    # Actual a value that parses type-ish.
                    %*PARAM_INFO<nominal_type> := $type.WHAT;
                    unless %*PARAM_INFO<post_constraints> {
                        %*PARAM_INFO<post_constraints> := [];
                    }
                    %*PARAM_INFO<post_constraints>.push($type);
                }
                elsif $type.HOW.archetypes.nominal {
                    %*PARAM_INFO<nominal_type> := $type;
                }
                elsif $type.HOW.archetypes.generic {
                    %*PARAM_INFO<nominal_type> := $type;
                    %*PARAM_INFO<nominal_generic> := 1;
                }
                elsif $type.HOW.archetypes.nominalizable {
                    my $nom := $type.HOW.nominalize($type);
                    %*PARAM_INFO<nominal_type> := $nom;
                    unless %*PARAM_INFO<post_constraints> {
                        %*PARAM_INFO<post_constraints> := [];
                    }
                    %*PARAM_INFO<post_constraints>.push($type);
                }
                else {
                    $/.CURSOR.panic("Type " ~ ~$<typename><longname> ~
                        " cannot be used as a nominal type on a parameter");
                }
                for ($<typename><longname> ?? $<typename><longname><colonpair> !! $<typename><colonpair>) {
                    if $_<identifier> {
                        if $_<identifier>.Str eq 'D' {
                            %*PARAM_INFO<defined_only> := 1;
                        }
                        elsif $_<identifier>.Str eq 'U' {
                            %*PARAM_INFO<undefined_only> := 1;
                        }
                    }
                }
            }
        }
        elsif $<value> {
            if nqp::existskey(%*PARAM_INFO, 'nominal_type') {
                $*W.throw($/, ['X', 'Parameter', 'MultipleTypeConstraints'],
                        parameter => (%*PARAM_INFO<variable_name> // ''),
                );
            }
            my $ast := $<value>.ast;
            unless $ast.has_compile_time_value {
                $/.CURSOR.panic('Cannot use a value type constraints whose value is unknown at compile time');
            }
            my $val := $ast.compile_time_value;
            %*PARAM_INFO<nominal_type> := $val.WHAT;
            unless %*PARAM_INFO<post_constraints> {
                %*PARAM_INFO<post_constraints> := [];
            }
            %*PARAM_INFO<post_constraints>.push($val);
        }
        else {
            $/.CURSOR.panic('Cannot do non-typename cases of type_constraint yet');
        }
    }

    method post_constraint($/) {
        if $<signature> {
            if nqp::existskey(%*PARAM_INFO, 'sub_signature_params') {
                $/.CURSOR.panic('Cannot have more than one sub-signature for a parameter');
            }
            %*PARAM_INFO<sub_signature_params> := $<signature>.ast;
            if nqp::substr(~$/, 0, 1) eq '[' {
                %*PARAM_INFO<sigil> := '@';
            }
        }
        else {
            unless %*PARAM_INFO<post_constraints> {
                %*PARAM_INFO<post_constraints> := [];
            }
            %*PARAM_INFO<post_constraints>.push(make_where_block($<EXPR>.ast));
        }
    }

    # Sets the default parameter type for a signature.
    sub set_default_parameter_type(@parameter_infos, $type_name) {
        my $type := $*W.find_symbol([$type_name]);
        for @parameter_infos {
            unless nqp::existskey($_, 'nominal_type') {
                $_<nominal_type> := $type;
            }
            if nqp::existskey($_, 'sub_signature_params') {
                set_default_parameter_type($_<sub_signature_params><parameters>, $type_name);
            }
        }
    }

    # Create Parameter objects, along with container descriptors
    # if needed. Parameters will be bound into the specified
    # lexpad.
    sub create_signature_object($/, %signature_info, $lexpad, :$no_attr_check) {
        my @parameters;
        my %seen_names;
        for %signature_info<parameters> {
            # Check we don't have duplicated named parameter names.
            if $_<named_names> {
                for $_<named_names> {
                    if %seen_names{$_} {
                        $*W.throw($/, ['X', 'Signature', 'NameClash'],
                            name => $_
                        );
                    }
                    %seen_names{$_} := 1;
                }
            }
            
            # If it's !-twigil'd, ensure the attribute it mentions exists unless
            # we're in a context where we should not do that.
            if $_<bind_attr> && !$no_attr_check {
                get_attribute_meta_object($/, $_<variable_name>, QAST::Var.new);
            }
            
            # If we have a sub-signature, create that.
            if nqp::existskey($_, 'sub_signature_params') {
                $_<sub_signature> := create_signature_object($/, $_<sub_signature_params>, $lexpad);
            }
            
            # Add variable as needed.
            if $_<variable_name> {
                my %sym := $lexpad.symbol($_<variable_name>);
                if +%sym && !nqp::existskey(%sym, 'descriptor') {
                    $_<container_descriptor> := $*W.create_container_descriptor(
                        $_<nominal_type>, $_<is_rw> ?? 1 !! 0, $_<variable_name>);
                    $lexpad.symbol($_<variable_name>, :descriptor($_<container_descriptor>));
                }
            }

            # Create parameter object and apply any traits.
            my $param_obj := $*W.create_parameter($_);
            if $_<traits> {
                for $_<traits> {
                    ($_.ast)($param_obj) if $_.ast;
                }
            }

            # Add it to the signature.
            @parameters.push($param_obj);
        }
        %signature_info<parameters> := @parameters;
        $*W.create_signature(%signature_info)
    }

    method trait($/) {
        make $<trait_mod> ?? $<trait_mod>.ast !! $<colonpair>.ast;
    }

    method trait_mod:sym<is>($/) {
        # Handle is repr specially.
        if ~$<longname> eq 'repr' {
            if $<circumfix> {
                $*REPR := compile_time_value_str($<circumfix>[0].ast[0], "is repr(...) trait", $/);
            }
            else {
                $/.CURSOR.panic("is repr(...) trait needs a parameter");
            }
        }
        else
        {
            # If we have an argument, get its compile time value or
            # evaluate it to get that.
            my @trait_arg;
            if $<circumfix> {
                my $arg := $<circumfix>[0].ast[0];
                @trait_arg[0] := $arg.has_compile_time_value ??
                    $arg.compile_time_value !!
                    $*W.create_thunk($/, $<circumfix>[0].ast)();
            }
        
            # If we have a type name then we need to dispatch with that type; otherwise
            # we need to dispatch with it as a named argument.
            my @name := $*W.dissect_longname($<longname>).components();
            if $*W.is_name(@name) {
                my $trait := $*W.find_symbol(@name);
                make -> $declarand {
                    $*W.apply_trait($/, '&trait_mod:<is>', $declarand, $trait, |@trait_arg);
                };
            }
            else {
                my %arg;
                %arg{~$<longname>} := @trait_arg ?? @trait_arg[0] !!
                    $*W.find_symbol(['Bool', 'True']);
                make -> $declarand, *%additional {
                    $*W.apply_trait($/, '&trait_mod:<is>', $declarand, |%arg, |%additional);
                };
            }
        }
    }

    method trait_mod:sym<hides>($/) {
        make -> $declarand {
            $*W.apply_trait($/, '&trait_mod:<hides>', $declarand, $<typename>.ast);
        };
    }

    method trait_mod:sym<does>($/) {
        make -> $declarand {
            $*W.apply_trait($/, '&trait_mod:<does>', $declarand, $<typename>.ast);
        };
    }

    method trait_mod:sym<will>($/) {
        my %arg;
        %arg{~$<identifier>} := ($*W.add_constant('Int', 'int', 1)).compile_time_value;
        make -> $declarand {
            $*W.apply_trait($/, '&trait_mod:<will>', $declarand,
                ($<pblock>.ast)<code_object>, |%arg);
        };
    }

    method trait_mod:sym<of>($/) {
        make -> $declarand {
            $*W.apply_trait($/, '&trait_mod:<of>', $declarand, $<typename>.ast);
        };
    }

    method trait_mod:sym<as>($/) {
        make -> $declarand {
            $*W.apply_trait($/, '&trait_mod:<as>', $declarand, $<typename>.ast);
        };
    }

    method trait_mod:sym<returns>($/) {
        make -> $declarand {
            $*W.apply_trait($/, '&trait_mod:<returns>', $declarand, $<typename>.ast);
        };
    }

    method trait_mod:sym<handles>($/) {
        # The term may be fairly complex. Thus we make it into a thunk
        # which the trait handler can use to get the term and work with
        # it.
        my $thunk := $*W.create_thunk($/, $<term>.ast);
        make -> $declarand {
            $*W.apply_trait($/, '&trait_mod:<handles>', $declarand, $thunk);
        };
    }

    method postop($/) {
        if $<postfix> {
            make $<postfix>.ast
                 || QAST::Op.new( :name('&postfix:<' ~ $<postfix>.Str ~ '>'), :op<call> )
        } else {
            make $<postcircumfix>.ast
                 || QAST::Op.new( :name('&postcircumfix:<' ~ $<postcircumfix>.Str ~ '>'), :op<call> );
        }
    }

    method dotty:sym<.>($/) { make $<dottyop>.ast; }

    method dotty:sym<.*>($/) {
        my $past := $<dottyop>.ast;
        unless $past.isa(QAST::Op) && $past.op() eq 'callmethod' {
            $/.CURSOR.panic("Cannot use " ~ $<sym>.Str ~ " on a non-identifier method call");
        }
        $past.unshift($*W.add_string_constant($past.name))
            if $past.name ne '';
        $past.name('dispatch:<' ~ ~$<sym> ~ '>');
        make $past;
    }

    method dottyop($/) {
        if $<methodop> {
            make $<methodop>.ast;
        }
        elsif $<postop> {
            make $<postop>.ast;
        }
        else {
            make $<colonpair>.ast;
        }
    }

    method privop($/) {
        # Compiling private method calls is somewhat interesting. If it's
        # in any way qualified, we need to ensure that the current package
        # is trusted by the target class. Otherwise we assume that the call
        # is to a private method in the current (non-virtual) package.
        # XXX Optimize the case where the method is declared up front - but
        # maybe this is for the optimizer, not for here.
        # XXX Attribute accesses? Again, maybe for the optimizer, since it
        # runs after CHECK time.
        my $past := $<methodop>.ast;
        if $<methodop><longname> {
            my @parts   := $*W.dissect_longname($<methodop><longname>).components();
            my $name    := @parts.pop;
            if @parts {
                my $methpkg := $*W.find_symbol(@parts);
                unless nqp::can($methpkg.HOW, 'is_trusted') && $methpkg.HOW.is_trusted($methpkg, $*PACKAGE) {
                    $*W.throw($/, ['X', 'Method', 'Private', 'Permission'],
                        :method(         $name),
                        :source-package( $methpkg.HOW.name($methpkg)),
                        :calling-package( $*PACKAGE.HOW.name($*PACKAGE)),
                    );
                }
                $past[1].returns($methpkg);
            }
            else {
                unless nqp::can($*PACKAGE.HOW, 'find_private_method') {
                    $*W.throw($/, ['X', 'Method', 'Private', 'Unqualified'],
                        :method($name),
                    );
                }
                $past.unshift(QAST::WVal.new( :value($*PACKAGE) ));
                $past[0].returns($*PACKAGE);
                $past.unshift($*W.add_string_constant($name));
            }
            $past.name('dispatch:<!>');
        }
        elsif $<methodop><quote> {
            my $name := $past.shift;
            $past.unshift(QAST::WVal.new( :value($*PACKAGE) ));
            $past.unshift($name);
            $past.name('dispatch:<!>');
        }
        else {
            $/.CURSOR.panic("Cannot use this form of method call with a private method");
        }
        make $past;
    }

    method methodop($/) {
        my $past := $<args> ?? $<args>.ast !! QAST::Op.new( :node($/) );
        $past.op('callmethod');
        if $<longname> {
            # May just be .foo, but could also be .Foo::bar. Also handle the
            # macro-ish cases.
            my @parts := $*W.dissect_longname($<longname>).components();
            my $name := @parts.pop;
            if +@parts {
                $past.unshift($*W.symbol_lookup(@parts, $/));
                $past.unshift($*W.add_string_constant($name));
                $past.name('dispatch:<::>');
            }
            elsif $name eq 'WHAT' {
                whine_if_args($/, $past, $name);
                $past.op('what');
            }
            elsif $name eq 'HOW' {
                whine_if_args($/, $past, $name);
                $past.op('how');
            }
            elsif $name eq 'WHO' {
                whine_if_args($/, $past, $name);
                $past.op('who');
            }
            elsif $name eq 'VAR' {
                whine_if_args($/, $past, $name);
                $past.op('p6var');
            }
            elsif $name eq 'REPR' {
                whine_if_args($/, $past, $name);
                $past.op('p6reprname');
            }
            elsif $name eq 'DEFINITE' {
                whine_if_args($/, $past, $name);
                $past.op('p6definite');
            }
            else {
                $past.name( $name );
            }
        }
        elsif $<quote> {
            $past.unshift(
                QAST::Op.new(
                    :op<unbox_s>,
                    $<quote>.ast
                )
            );
        }
        elsif $<variable> {
            $past.unshift($<variable>.ast);
            $past.name('dispatch:<var>');
        }
        make $past;
    }
    
    sub whine_if_args($/, $past, $name) {
        if +@($past) > 0 {
           $*W.throw($/, ['X', 'Syntax', 'Argument', 'MOPMacro'], macro => $name);
        }
    }
    
    method term:sym<::?IDENT>($/) {
        make instantiated_type([~$/], $/);
    }

    method term:sym<self>($/) {
        make QAST::Var.new( :name('self'), :scope('lexical'), :returns($*PACKAGE), :node($/) );
    }

    method term:sym<now>($/) {
        make QAST::Op.new( :op('call'), :name('&term:<now>'), :node($/) );
    }

    method term:sym<time>($/) {
        make QAST::Op.new( :op('call'), :name('&term:<time>'), :node($/) );
    }

    method term:sym<∅>($/) {
        make QAST::Op.new( :op('call'), :name('&set'), :node($/) );
    }

    method term:sym<rand>($/) {
        make QAST::Op.new( :op('call'), :name('&rand'), :node($/) );
    }

    sub make_yada($name, $/) {
	    my $past := $<args>.ast;
	    $past.name($name);
	    $past.node($/);
	    unless +$past.list() {
            $past.push($*W.add_string_constant('Stub code executed'));
	    }
        $past
    }

    method term:sym<...>($/) {
        make make_yada('&fail', $/);
    }

    method term:sym<???>($/) {
        make make_yada('&warn', $/);
    }

    method term:sym<!!!>($/) {
        make make_yada('&die', $/);
    }

    method term:sym<dotty>($/) {
        my $past := $<dotty>.ast;
        $past.unshift(QAST::Var.new( :name('$_'), :scope('lexical') ) );
        make QAST::Op.new( :op('hllize'), $past);
    }

    sub find_macro_routine(@symbol) {
        my $routine;
        try {
            $routine := $*W.find_symbol(@symbol);
            if istype($routine, $*W.find_symbol(['Macro'])) {
                return $routine;
            }
        }
        0
    }

    sub expand_macro($macro, $name, $/, &collect_argument_asts) {
        my @argument_asts := &collect_argument_asts();
        my $macro_ast := $*W.ex-handle($/, { $macro(|@argument_asts) });
        my $nil_class := $*W.find_symbol(['Nil']);
        if istype($macro_ast, $nil_class) {
            return QAST::WVal.new( :value($nil_class) );
        }
        my $ast_class := $*W.find_symbol(['AST']);
        unless istype($macro_ast, $ast_class) {
            $*W.throw('X::TypeCheck::Splice',
                got         => $macro_ast,
                expected    => $ast_class,
                symbol      => $name,
                action      => 'macro application',
            );
        }
        my $macro_ast_qast := nqp::getattr(
            nqp::decont($macro_ast),
            $ast_class,
            '$!past'
        );
        unless nqp::defined($macro_ast_qast) {
            return QAST::WVal.new( :value($*W.find_symbol(['Nil'])) );
        }
        my $block := QAST::Block.new(:blocktype<raw>, $macro_ast_qast);
        $*W.add_quasi_fixups($macro_ast, $block);
        my $past := QAST::Stmts.new(
            $block,
            QAST::Op.new( :op('call'), QAST::BVal.new( :value($block) ) )
        );
        $past
    }

    method term:sym<identifier>($/) {
        my $macro := find_macro_routine(['&' ~ ~$<identifier>]);
        if $macro {
            make expand_macro($macro, ~$<identifier>, $/, sub () {
                my @argument_asts := [];
                if $<args><semiarglist> {
                    for $<args><semiarglist><arglist> {
                        if $_<EXPR> {
                            add_macro_arguments($_<EXPR>.ast, @argument_asts);
                        }
                    }
                }
                return @argument_asts;
            });
        }
        else {
            my $past := capture_or_parcel($<args>.ast, ~$<identifier>);
            $past.name('&' ~ $<identifier>);
            $past.node($/);
            make $past;
        }
    }

    sub add_macro_arguments($expr, @argument_asts) {
        my $ast_class := $*W.find_symbol(['AST']);

        sub wrap_and_add_expr($expr) {
            my $quasi_ast := $ast_class.new();
            my $wrapped := QAST::Op.new( :op('call'), make_thunk_ref($expr, $expr.node) );
            nqp::bindattr($quasi_ast, $ast_class, '$!past', $wrapped);
            @argument_asts.push($quasi_ast);
        }

        if nqp::istype($expr, QAST::Op) && $expr.name eq '&infix:<,>' {
            for $expr.list {
                wrap_and_add_expr($_);
            }
        }
        else {
            wrap_and_add_expr($expr);
        }
    }

    method make_indirect_lookup(@components, $sigil?) {
        my $past := QAST::Op.new(
            :op<call>,
            :name<&INDIRECT_NAME_LOOKUP>,
            QAST::Op.new(
                :op<callmethod>, :name<new>,
                QAST::WVal.new( :value($*W.find_symbol(['PseudoStash'])) )
            )
        );
        $past.push($*W.add_string_constant($sigil)) if $sigil;
        for @components {
            if nqp::can($_, 'isa') && $_.isa(QAST::Node) {
                $past.push($_);
            } else {
                $past.push($*W.add_string_constant(~$_));
            }
        }
        $past;
    }

    method term:sym<name>($/) {
        my $past;
        if $*longname.contains_indirect_lookup() {
            if $<args> {
                $/.CURSOR.panic("Combination of indirect name lookup and call not (yet?) allowed");
            }
            $past := self.make_indirect_lookup($*longname.components())
        }
        elsif $<args> {
            # If we have args, it's a call. Look it up dynamically
            # and make the call.
            # Add & to name.
            my @name := nqp::clone($*longname.components());
            my $final := @name[+@name - 1];
            if nqp::substr($final, 0, 1) ne '&' {
                @name[+@name - 1] := '&' ~ $final;
            }
            my $macro := find_macro_routine(@name);
            if $macro {
                $past := expand_macro($macro, $*longname.text, $/, sub () {
                    my @argument_asts := [];
                    if $<args><semiarglist> {
                        for $<args><semiarglist><arglist> {
                            if $_<EXPR> {
                                add_macro_arguments($_<EXPR>.ast, @argument_asts);
                            }
                        }
                    }
                    elsif $<args><arglist> {
                        if $<args><arglist><EXPR> {
                            add_macro_arguments($<args><arglist><EXPR>.ast, @argument_asts);
                        }
                    }
                    return @argument_asts;
                });
            }
            else {
                $past := capture_or_parcel($<args>.ast, ~$<longname>);
                if +@name == 1 {
                    $past.name(@name[0]);
                }
                else {
                    $past.unshift($*W.symbol_lookup(@name, $/));
                }
            }
        }
        else {
            # Otherwise, it's a type name; build a reference to that
            # type, since we can statically resolve them.
            my @name := $*longname.type_name_parts('type name');
            if $<arglist> {
                # Look up parametric type.
                my $ptype := $*W.find_symbol(@name);
                
                # Do we know all the arguments at compile time?
                my int $all_compile_time := 1;
                for @($<arglist>.ast) {
                    unless $_.has_compile_time_value {
                        $all_compile_time := 0;
                    }
                }
                if $all_compile_time {
                    my $curried := $*W.parameterize_type($ptype, $<arglist>.ast, $/);
                    $past := QAST::WVal.new( :value($curried) );
                }
                else {
                    my $ptref := QAST::WVal.new( :value($ptype) );
                    $past := $<arglist>.ast;
                    $past.op('callmethod');
                    $past.name('parameterize');
                    $past.unshift($ptref);
                    $past.unshift(QAST::Op.new( :op('how'), $ptref ));
                }
            }
            elsif +@name == 0 {
                $past := QAST::Op.new(
                    :op<callmethod>, :name<new>,
                    QAST::WVal.new( :value($*W.find_symbol(['PseudoStash'])) )
                );
            }
            elsif $*W.is_pseudo_package(@name[0]) {
                $past := $*W.symbol_lookup(@name, $/);
            }
            elsif +@name == 1 && $*W.is_name(@name)
                    && !$*W.symbol_has_compile_time_value(@name) {
                # it's a sigilless param or variable
                $past := make_variable_from_parts($/, @name, '', '', @name[0]);
            }
            else {
                $past := instantiated_type(@name, $/);
            }
            
            # Names ending in :: really want .WHO.
            if $*longname.get_who {
                $past := QAST::Op.new( :op('who'), $past );
            }
        }

        $past.node($/);
        make $past;
    }

    method term:sym<pir::op>($/) {
        if $FORBID_PIR {
            nqp::die("pir::op forbidden in safe mode\n");
        }
        my $pirop := nqp::join(' ', nqp::split('__', ~$<op>));
        unless nqp::index($pirop, ' ') > 0 {
            nqp::die("pir::$pirop missing a signature");
        }
        my $past := QAST::VM.new( :pirop($pirop), :node($/) );
        if $<args> {
            for $<args>.ast.list {
                $past.push($_);
            }
        }
        make $past;
    }

    method term:sym<pir::const>($/) {
        make QAST::VM.new( :pirconst(~$<const>) );
    }

    method term:sym<nqp::op>($/) {
        $/.CURSOR.panic("nqp::op forbidden in safe mode\n") if $FORBID_PIR;
        my @args := $<args> ?? $<args>.ast.list !! [];
        my $past := QAST::Op.new( :op(~$<op>), |@args );
        if $past.op eq 'want' || $past.op eq 'handle' {
            my int $i := 1;
            my int $n := nqp::elems($past.list);
            while $i < $n {
                $past[$i] := compile_time_value_str($past[$i], 'want specification', $/);
                $i := $i + 2;
            }
        }
        nqp::getcomp('QAST').operations.attach_result_type('perl6', $past);
        make $past;
    }

    method term:sym<nqp::const>($/) {
        make QAST::Op.new( :op('const'), :name(~$<const>) );
    }

    method term:sym<*>($/) {
        my $whatever := $*W.find_symbol(['Whatever']);
        make QAST::Op.new(
            :op('callmethod'), :name('new'), :node($/), :returns($whatever),
            QAST::Var.new( :name('Whatever'), :scope('lexical') )
        )
    }

    method term:sym<capterm>($/) {
        make $<capterm>.ast;
    }
    
    method term:sym<onlystar>($/) {
        my $dc_name := QAST::Node.unique('dispatch_cap');
        my $stmts := QAST::Stmts.new(
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name($dc_name), :scope('local'), :decl('var') ),
                QAST::Var.new( :name('CURRENT_DISPATCH_CAPTURE'), :scope('lexical') )
            ),
            QAST::Op.new(
                :op('invokewithcapture'),
                QAST::Op.new(
                    :op('ifnull'),
                    QAST::Op.new(
                        :op('multicachefind'),
                        QAST::Var.new(
                            :name('$!dispatch_cache'), :scope('attribute'),
                            QAST::Var.new( :name('&*CURRENT_DISPATCHER'), :scope('lexical') ),
                            QAST::WVal.new( :value($*W.find_symbol(['Routine'])) ),
                        ),
                        QAST::Var.new( :name($dc_name), :scope('local') )
                    ),
                    QAST::Op.new(
                        :op('callmethod'), :name('find_best_dispatchee'),
                        QAST::Var.new( :name('&*CURRENT_DISPATCHER'), :scope('lexical') ),
                        QAST::Var.new( :name($dc_name), :scope('local') )
                    )
                ),
                QAST::Var.new( :name($dc_name), :scope('local') )
            ));
        make QAST::Op.new( :op('locallifetime'), $stmts, $dc_name );
    }

    method args($/) {
        my $past;
        if    $<semiarglist> { $past := $<semiarglist>.ast; }
        elsif $<arglist>     { $past := $<arglist>.ast; }
        else {
            $past := QAST::Op.new( :op('call'), :node($/) );
        }
        make $past;
    }

    method semiarglist($/) {
        if +$<arglist> == 1 {
            make $<arglist>[0].ast;
        }
        else {
            my $past := QAST::Op.new( :op('call'), :node($/) );
            for $<arglist> {
                my $ast := $_.ast;
                $ast.name('&infix:<,>');
                $past.push($ast);
            }
            make $past;
        }
    }

    sub hunt_loose_adverbs_in_arglist($/, @past) {
        # if we have a non-comma-separated list of adverbial pairs in an
        # arglist or semiarglist, only the first will be passed as a named
        # argument.

        # Thus, we need to find chained adverbs. They show up on the parse
        # tree as colonpair rules followed by a fake_infix.

        if nqp::getenvhash<COLONPAIR> eq 'trace' { say($/.dump) }
        if +$/.list == 1 && nqp::istype($/[0].ast, QAST::Op) && $/[0].ast.op eq 'call' && $/[0].ast.name ne 'infix:<,>' {
            nqp::die("these adverbs belong to a deeper-nested thing");
        }
        if $<fake_infix>
               || $<colonpair> && +($/.list) == 0 && +($/.hash) == 1 {
            if +($/.list) == 1 {
                hunt_loose_adverbs_in_arglist($/[0], @past);
            }
            my $Pair := $*W.find_symbol(['Pair']);
            if $<colonpair> && istype($<colonpair>.ast.returns, $Pair) {
                if $*WAS_SKIPPED {
                    nqp::push(@past, $<colonpair>.ast);
                } else {
                    $*WAS_SKIPPED := 1;
                }
            }
        } elsif $<OPER>.Str eq ',' {
           my $*WAS_SKIPPED := 0;
            for $/.list {
                hunt_loose_adverbs_in_arglist($_, @past);
            }
        }
    }

    method arglist($/) {
        my $Pair := $*W.find_symbol(['Pair']);
        my $past := QAST::Op.new( :op('call'), :node($/) );
        my @names;
        if $<EXPR> {
            # Make first pass over arguments, finding any duplicate named
            # arguments.
            my $expr := $<EXPR>.ast;
            my @args := nqp::istype($expr, QAST::Op) && $expr.name eq '&infix:<,>'
                ?? $expr.list
                !! [$expr];
            # but first, look for any chained adverb pairs
            my $*WAS_SKIPPED := 0;
            try {
                if $*FAKE_INFIX_FOUND {
                    hunt_loose_adverbs_in_arglist($<EXPR>, @args);
                }
            }
            my %named_counts;
            for @args {
                if nqp::istype($_, QAST::Op) && istype($_.returns, $Pair) {
                    my $name := compile_time_value_str($_[1], 'LHS of pair', $/);
                    %named_counts{$name} := +%named_counts{$name} + 1;
                    $_[2].named($name);
                }
            }

            # Make result.
            for @args {
                if nqp::istype($_, QAST::Op) && istype($_.returns, $Pair) {
                    my $name := $_[2].named();
                    if %named_counts{$name} == 1 {
                        $past.push($_[2]);
                        $_[2]<before_promotion> := $_;
                    }
                    else {
                        %named_counts{$name} := %named_counts{$name} - 1;
                    }
                }
                elsif nqp::istype($_, QAST::Op) && $_.name eq '&prefix:<|>' {
                    my $reg := $past.unique('flattening_');
                    $past.push(QAST::Op.new(
                        :op('callmethod'), :name('FLATTENABLE_LIST'),
                        QAST::Op.new(
                            :op('bind'),
                            QAST::Var.new( :name($reg), :scope('local'), :decl('var') ),
                            $_[0]),
                        :flat(1) ));
                    $past.push(QAST::Op.new(
                        :op('callmethod'), :name('FLATTENABLE_HASH'),
                        QAST::Var.new( :name($reg), :scope('local') ),
                        :flat(1), :named(1) ));
                    @names.push($reg);
                }
                else {
                    $past.push($_);
                }
            }
        }

        make $past; # TODO figure out how to work a locallifetime in here
    }

    method term:sym<value>($/) { make $<value>.ast; }

    method circumfix:sym<( )>($/) {
        my $past := $<semilist>.ast;
        my $size := +$past.list;
        if $size == 0 {
            $past := QAST::Op.new( :op('call'), :name('&infix:<,>') );
        }
        else {
            my $last := $past[ $size - 1 ];
            $past.returns($last.returns);
            if nqp::istype($last, QAST::Block) {
                $past.arity($last.arity);
            }
        }
        make $past;
    }

    method circumfix:sym<ang>($/) { make $<nibble>.ast; }

    method circumfix:sym«<< >>»($/) { make $<nibble>.ast; }
    
    method circumfix:sym<« »>($/) { make $<nibble>.ast; }

    method circumfix:sym<{ }>($/) {
        # If it was {YOU_ARE_HERE}, nothing to do here.
        my $past := $<pblock>.ast;
        if ~$/ eq '{YOU_ARE_HERE}' {
            make $past;
            return 1;
        }

        # If it is completely empty or consists of a single list, the first
        # element of which is either a hash or a pair, it's a hash constructor.
        # Note that if it declares any symbols it is also not one.
        my $Pair := $*W.find_symbol(['Pair']);
        my int $is_hash   := 0;
        my int $has_stuff := 1;
        my $stmts := +$<pblock><blockoid><statementlist><statement>;
        my $bast  := $<pblock><blockoid>.ast;
        if $bast.symbol('$_')<used> || $bast<also_uses> && $bast<also_uses><$_> {
            # Uses $_, so not a hash.
        }
        elsif $stmts == 0 {
            # empty block, so a hash
            $is_hash   := 1;
            $has_stuff := 0;
        }
        elsif $stmts == 1 {
            my $elem := $past<past_block>[1][0][0];
            $elem := $elem[0] if $elem ~~ QAST::Want;
            if $elem ~~ QAST::Op && $elem.name eq '&infix:<,>' {
                # block contains a list, so test the first element
                $elem := $elem[0];
            }
            if $elem ~~ QAST::Op
                    && (istype($elem.returns, $Pair) || $elem.name eq '&infix:<=>>') {
                # first item is a pair
                $is_hash := 1;
            }
            elsif $elem ~~ QAST::Var
                    && nqp::substr($elem.name, 0, 1) eq '%' {
                # first item is a hash
                $is_hash := 1;
            }
        }
        if $is_hash {
            for $past<past_block>.symtable() {
                my $sym := $_.key;
                if $sym ne '$_' && $sym ne '$*DISPATCHER' {
                    $is_hash := 0;
                }
            }
        }
        if $is_hash && $past<past_block>.arity == 0 {
            migrate_blocks($past<past_block>, $*W.cur_lexpad());
            my @children := @($past<past_block>[1]);
            $past := QAST::Op.new(
                :op('call'),
                :name('&circumfix:<{ }>'),
                :node($/)
            );
            if $has_stuff {
                for @children {
                    if nqp::istype($_, QAST::Stmt) {
                        # Mustn't use QAST::Stmt here, as it will cause register
                        # re-use within a statemnet, which is a problem.
                        $_ := QAST::Stmts.new( |$_.list );
                    }
                    $past.push($_);
                }
            }
        }
        else {
            my $block := $past<past_block>;
            $block[0].push(QAST::Var.new( :name('$*DISPATCHER'), :scope('lexical'), :decl('var') ));
            $block[0].push(QAST::Op.new(
                :op('takedispatcher'),
                QAST::SVal.new( :value('$*DISPATCHER') )
            ));
            $past := block_closure($past);
            $past<bare_block> := QAST::Op.new(
                :op('call'),
                QAST::BVal.new( :value($past<past_block>) ));
        }
        make $past;
    }

    # Some constructs are parsed and compiled with blocks inside of them, but
    # then the outer block goes away (for example, when a {...} becomes a
    # hash). This is used to move blocks out of the discarded inner one to
    # the outer one, so they're correctly lexically scoped.
    sub migrate_blocks($from, $to) {
        my @decls := @($from[0]);
        my int $n := nqp::elems(@decls);
        my int $i := 0;
        while $i < $n {
            if nqp::istype(@decls[$i], QAST::Block) {
                $to[0].push(@decls[$i]);
                @decls[$i] := QAST::Op.new( :op('null') );
            }
            elsif (nqp::istype(@decls[$i], QAST::Stmt) || nqp::istype(@decls[$i], QAST::Stmts)) &&
                  nqp::istype(@decls[$i][0], QAST::Block) {
                $to[0].push(@decls[$i][0]);
                @decls[$i][0] := QAST::Op.new( :op('null') );
            }
            $i++;
        }
    }

    method circumfix:sym<[ ]>($/) {
        make QAST::Op.new( :op('call'), :name('&circumfix:<[ ]>'), $<semilist>.ast, :node($/) );
    }

    ## Expressions
    my %specials := nqp::hash(
        '==>',  -> $/, $sym { make_feed($/) },
        '==>>', -> $/, $sym { make_feed($/) },
        '<==',  -> $/, $sym { make_feed($/) },
        '<<==', -> $/, $sym { make_feed($/) },
        '~~',   -> $/, $sym { make_smartmatch($/, 0) },
        '!~~',  -> $/, $sym { make_smartmatch($/, 1) },
        '=',    -> $/, $sym { assign_op($/, $/[0].ast, $/[1].ast) },
        ':=',   -> $/, $sym { bind_op($/, $/[0].ast, $/[1].ast, 0) },
        '::=',  -> $/, $sym { bind_op($/, $/[0].ast, $/[1].ast, 1) },
        'ff',   -> $/, $sym { flipflop($/[0].ast, $/[1].ast, 0, 0, 0) },
        '^ff',  -> $/, $sym { flipflop($/[0].ast, $/[1].ast, 1, 0, 0) },
        'ff^',  -> $/, $sym { flipflop($/[0].ast, $/[1].ast, 0, 1, 0) },
        '^ff^', -> $/, $sym { flipflop($/[0].ast, $/[1].ast, 1, 1, 0) },
        'fff',  -> $/, $sym { flipflop($/[0].ast, $/[1].ast, 0, 0, 1) },
        '^fff', -> $/, $sym { flipflop($/[0].ast, $/[1].ast, 1, 0, 1) },
        'fff^', -> $/, $sym { flipflop($/[0].ast, $/[1].ast, 0, 1, 1) },
        '^fff^',-> $/, $sym { flipflop($/[0].ast, $/[1].ast, 1, 1, 1) }
    );
    method EXPR($/, $key?) {
        unless $key { return 0; }
        my $past := $/.ast // $<OPER>.ast;
        my $sym := ~$<infix><sym>;
        my int $return_map := 0;
        if !$past && $sym eq '.=' {
            make make_dot_equals($/[0].ast, $/[1].ast);
            return 1;
        }
        elsif $past && nqp::substr($past.name, 0, 19) eq '&METAOP_TEST_ASSIGN' {
            $past.push($/[0].ast);
            $past.push(block_closure(make_thunk_ref($/[1].ast, $/)));
            make $past;
            return 1;
        }
        elsif nqp::existskey(%specials, $sym) {
            make %specials{$sym}($/, $sym);
            return 1;
        }
        elsif !$past && ($sym eq 'does' || $sym eq 'but') {
            make mixin_op($/, $sym);
            return 1;
        }
        elsif !$past && $sym eq 'xx' {
            make xx_op($/, $/[0].ast, $/[1].ast);
            return 1;
        }
        elsif !$past && $sym eq 'andthen' {
            make andthen_op($/);
            return 1;
        }
        unless $past {
            if $<OPER><O><pasttype> {
                $past := QAST::Op.new( :node($/), :op( ~$<OPER><O><pasttype> ) );
            }
            elsif $<OPER><O><pirop> {
                $past := QAST::VM.new( :node($/), :pirop(~$<OPER><O><pirop>) );
            }
            else {
                $past := QAST::Op.new( :node($/), :op('call') );
            }
            my $name;
            if $past.isa(QAST::Op) && !$past.name {
                if $key eq 'LIST' { $key := 'infix'; }
                $name := nqp::lc($key) ~ ':<' ~ $<OPER><sym> ~ '>';
                $past.name('&' ~ $name);
            }
            my $macro := find_macro_routine(['&' ~ $name]);
            if $macro {
                make expand_macro($macro, $name, $/, sub () {
                    my @argument_asts := [];
                    for @($/) {
                        add_macro_arguments($_.ast, @argument_asts);
                    }
                    return @argument_asts;
                });
                return 'an irrelevant value';
            }
        }
        if $key eq 'POSTFIX' {
            # If may be an adverb.
            if $<colonpair> {
                my $target := $past := $/[0].ast;
                if nqp::istype($target, QAST::Op) && $target.op eq 'hllize' {
                    $target := $target[0];
                }
                unless nqp::istype($target, QAST::Op) && ($target.op eq 'call' || $target.op eq 'callmethod') {
                    $/.CURSOR.panic("You can't adverb that");
                }
                my $cpast := $<colonpair>.ast;
                $cpast[2].named(compile_time_value_str($cpast[1], 'LHS of pair', $/));
                $target.push($cpast[2]);
                make $past;
                return 1;
            }
            
            # Method calls may be to a foreign language, and thus return
            # values may need type mapping into Perl 6 land.
            $past.unshift($/[0].ast);
            if $past.isa(QAST::Op) && $past.op eq 'callmethod' {
                $return_map := 1;
            }
        }
        else {
            for $/.list { if $_.ast { $past.push($_.ast); } }
        }
        if $past.op eq 'xor' {
            $past.push(QAST::WVal.new( :named<false>, :value($*W.find_symbol(['Nil'])) ));
        }
        if $key eq 'PREFIX' || $key eq 'INFIX' || $key eq 'POSTFIX' {
            $past := whatever_curry($/, (my $orig := $past), $key eq 'INFIX' ?? 2 !! 1);
            if $return_map && $orig =:= $past {
                $past := QAST::Op.new($past,
                    :op('hllize'), :returns($past.returns()));
            }
        }
        make $past;
    }

    sub make_feed($/) {
        # Assemble into list of AST of each step in the pipeline.
        my @stages;
        if $/<infix><sym> eq '==>' {
            for @($/) { @stages.push($_.ast); }
        }
        elsif $/<infix><sym> eq '<==' {
            for @($/) { @stages.unshift($_.ast); }
        }
        else {
            $*W.throw($/, 'X::Comp::NYI',
                feature => $/<infix> ~ " feed operator"
            );
        }

        # Check what's in each stage and make a chain of blocks
        # that call each other. They'll return lazy things, which
        # will be passed in as var-arg parts to other things. The
        # first thing is just considered the result.
        my $result := @stages.shift;
        for @stages {
            # Wrap current result in a block, so it's thunked and can be
            # called at the right point.
            $result := QAST::Block.new( $result );

            # Check what we have. XXX Real first step should be looking
            # for @(*) since if we find that it overrides all other things.
            # But that's todo...soon. :-)
            if $_.isa(QAST::Op) && $_.op eq 'call' {
                # It's a call. Stick a call to the current supplier in
                # as its last argument.
                $_.push(QAST::Op.new( :op('call'), $result ));
            }
            elsif $_ ~~ QAST::Var {
                # It's a variable. We need code that gets the results, pushes
                # them onto the variable and then returns them (since this
                # could well be a tap.
                my $tmp := QAST::Node.unique('feed_tmp');
                $_ := QAST::Stmts.new(
                    QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :scope('local'), :name($tmp), :decl('var') ),
                        QAST::Op.new( :op('call'), $result )
                    ),
                    QAST::Op.new(
                        :op('callmethod'), :name('push'),
                        $_,
                        QAST::Var.new( :scope('local'), :name($tmp) )
                    ),
                    QAST::Var.new( :scope('local'), :name($tmp) )
                );
                $_ := QAST::Op.new( :op('locallifetime'), $_, $tmp );
            }
            else {
                $/.CURSOR.panic('Sorry, do not know how to handle this case of a feed operator yet.');
            }
            $result := $_;
        }

        $result
    }

    sub make_smartmatch($/, $negated) {
        my $lhs := $/[0].ast;
        my $rhs := $/[1].ast;
        my $old_topic_var := $lhs.unique('old_topic');
        my $result_var := $lhs.unique('sm_result');
        my $sm_call;

        # In case the rhs is a substitution, the result should say if it actually
        # matched something. Calling ACCEPTS will always be True for this case.
        if $rhs<is_subst> {
            $sm_call := QAST::Stmt.new(
                $rhs,
                QAST::Op.new(
                    :op('callmethod'), :name('Bool'),
                    QAST::Var.new( :name('$/'), :scope('lexical') )
                )
            );
        }
        else {
            # Call $rhs.ACCEPTS( $_ ), where $_ is $lhs.
            $sm_call := QAST::Op.new(
                :op('callmethod'), :name('ACCEPTS'),
                $rhs,
                QAST::Var.new( :name('$_'), :scope('lexical') )
            );
        }

        if $negated {
            $sm_call := QAST::Op.new( :op('call'), :name('&prefix:<!>'), $sm_call );
        }

        QAST::Op.new(
            :op('locallifetime'),
            QAST::Stmt.new(
                # Stash original $_.
                QAST::Op.new( :op('bind'),
                    QAST::Var.new( :name($old_topic_var), :scope('local'), :decl('var') ),
                    QAST::Var.new( :name('$_'), :scope('lexical') )
                ),

                # Evaluate LHS and bind it to $_.
                QAST::Op.new( :op('bind'),
                    QAST::Var.new( :name('$_'), :scope('lexical') ),
                    $lhs
                ),

                # Evaluate RHS and call ACCEPTS on it, passing in $_. Bind the
                # return value to a result variable.
                QAST::Op.new( :op('bind'),
                    QAST::Var.new( :name($result_var), :scope('local'), :decl('var') ),
                    $sm_call
                ),

                # Re-instate original $_.
                QAST::Op.new( :op('bind'),
                    QAST::Var.new( :name('$_'), :scope('lexical') ),
                    QAST::Var.new( :name($old_topic_var), :scope('local') )
                ),

                # And finally evaluate to the smart-match result.
                QAST::Var.new( :name($result_var), :scope('local') )
            ),
            $old_topic_var,
            $result_var,
        );
    }

    sub bind_op($/, $target, $source, $sigish) {
        # Check we know how to bind to the thing on the LHS.
        if $target.isa(QAST::Var) {
            # Check it's not a native type; we can't bind to those.
            if nqp::objprimspec($target.returns) {
                $*W.throw($/, ['X', 'Bind', 'NativeType'],
                        name => ($target.name // ''),
                );
            }
            
            # We may need to decontainerize the right, depending on sigil.
            my $sigil := nqp::substr($target.name(), 0, 1);
            if $sigil eq '@' || $sigil eq '%' {
                $source := QAST::Op.new( :op('decont'), $source );
            }

            # Now go by scope.
            if $target.scope eq 'attribute' {
                # Source needs type check.
                my $type;
                try {
                    $type := $*PACKAGE.HOW.get_attribute_for_usage(
                        $*PACKAGE, $target.name
                    ).type;
                }
                unless $type =:= $*W.find_symbol(['Mu']) {
                    $source := QAST::Op.new(
                        :op('p6bindassert'),
                        $source, QAST::WVal.new( :value($type) ))
                }
            }
            else {
                # Probably a lexical.
                my int $was_lexical := 0;
                try {
                    my $type := $*W.find_lexical_container_type($target.name);
                    unless $type =:= $*W.find_symbol(['Mu']) {
                        $source := QAST::Op.new(
                            :op('p6bindassert'),
                            $source, QAST::WVal.new( :value($type) ));
                    }
                    $was_lexical := 1;
                }
                unless $was_lexical {
                    $*W.throw($/, ['X', 'Bind']);
                }
            }

            # Finally, just need to make a bind.
            make QAST::Op.new( :op('bind'), $target, $source );
        }
        elsif $target.isa(QAST::Op) && $target.op eq 'hllize' &&
                $target[0].isa(QAST::Op) && $target[0].op eq 'call' &&
                ($target[0].name eq '&postcircumfix:<[ ]>' || $target[0].name eq '&postcircumfix:<{ }>') {
            $source.named('BIND');
            $target[0].push($source);
            make $target;
        }
        elsif $target.isa(QAST::Op) && $target.op eq 'call' &&
              ($target.name eq '&postcircumfix:<[ ]>' || $target.name eq '&postcircumfix:<{ }>') {
            $source.named('BIND');
            $target.push($source);
            make $target;
        }
        elsif $target.isa(QAST::WVal) && nqp::istype($target.value, $*W.find_symbol(['Signature'])) {
            make QAST::Op.new(
                :op('p6bindcaptosig'),
                $target,
                QAST::Op.new(
                    :op('callmethod'), :name('Capture'),
                    $source
                ));
        }
        # XXX Several more cases to do...
        else {
            $*W.throw($/, ['X', 'Bind']);
        }
    }

    sub assign_op($/, $lhs_ast, $rhs_ast) {
        my $past;
        my $var_sigil;
        if $lhs_ast.isa(QAST::Var) {
            $var_sigil := nqp::substr($lhs_ast.name, 0, 1);
        }
        if nqp::istype($lhs_ast, QAST::Var)
                && nqp::objprimspec($lhs_ast.returns) {
            # Native assignment is actually really a bind at low level.
            $past := QAST::Op.new(
                :op('bind'), :returns($lhs_ast.returns),
                $lhs_ast, $rhs_ast);
        }
        elsif $var_sigil eq '@' || $var_sigil eq '%' {
            # While the scalar container store op would end up calling .STORE,
            # it does it in a nested runloop, which gets pricey. This is a
            # simple heuristic check to try and avoid that by calling .STORE.
            $past := QAST::Op.new(
                :op('callmethod'), :name('STORE'),
                $lhs_ast, $rhs_ast);
            $past<nosink> := 1;
        }
        elsif $lhs_ast.isa(QAST::Op) && $lhs_ast.op eq 'call' &&
              ($lhs_ast.name eq '&postcircumfix:<[ ]>' || $lhs_ast.name eq '&postcircumfix:<{ }>') &&
                +@($lhs_ast) == 2 { # no adverbs
            $lhs_ast.push($rhs_ast);
            $past := $lhs_ast;
            $past<nosink> := 1;
        }
        elsif $lhs_ast.isa(QAST::Op) && $lhs_ast.op eq 'hllize' &&
                $lhs_ast[0].isa(QAST::Op) && $lhs_ast[0].op eq 'call' &&
                ($lhs_ast[0].name eq '&postcircumfix:<[ ]>' || $lhs_ast[0].name eq '&postcircumfix:<{ }>') &&
                +@($lhs_ast[0]) == 2 { # no adverbs
            $lhs_ast[0].push($rhs_ast);
            $past := $lhs_ast;
            $past<nosink> := 1;
        }
        else {
            $past := QAST::Op.new( :node($/), :op('p6store'),
                $lhs_ast, $rhs_ast);
        }
        $past
    }
    
    sub mixin_op($/, $sym) {
        my $rhs  := $/[1].ast;
        my $past := QAST::Op.new(
            :op('call'), :name('&infix:<' ~ $sym ~ '>'),
            $/[0].ast);
        if $rhs.isa(QAST::Op) && $rhs.op eq 'call' {
            if $rhs.name && +@($rhs) == 1 {
                try {
                    $past.push(QAST::WVal.new( :value($*W.find_symbol([nqp::substr($rhs.name, 1)])) ));
                    $rhs[0].named('value');
                    $past.push($rhs[0]);
                    CATCH { $past.push($rhs); }
                }
            }
            else {
                if +@($rhs) == 2 && $rhs[0].has_compile_time_value {
                    $past.push($rhs[0]); $rhs[1].named('value');
                    $past.push($rhs[1]);
                }
                else {
                    $past.push($rhs);
                }
            }
        }
        else {
            $past.push($rhs);
        }
        $past
    }
    
    sub xx_op($/, $lhs, $rhs) {
        QAST::Op.new(
            :op('call'), :name('&infix:<xx>'), :node($/),
            block_closure(make_thunk_ref($lhs, $/)),
            $rhs,
            QAST::Op.new( :op('p6bool'), QAST::IVal.new( :value(1) ), :named('thunked') ))
    }

    sub andthen_op($/) {
        my $past := QAST::Op.new(
            :op('call'),
            :name('&infix:<andthen>'),
            # don't need to thunk the first operand
            $/[0].ast,
        );
        my int $i := 1;
        my int $e := +@($/);
        while ($i < $e) {
            my $ast := $/[$i].ast;
            if $ast<past_block> {
                $past.push($ast);
            }
            else {
                $past.push(block_closure(make_thunk_ref($ast, $/[$i]))),
            }
            $i++;
        }
        $past;
    }
    
    sub flipflop($lhs, $rhs, $min_excl, $max_excl, $one_only) {
        # Need various constants.
        my $zero  := $*W.add_numeric_constant(NQPMu, 'Int', 0);
        my $one   := $*W.add_numeric_constant(NQPMu, 'Int', 1);
        my $nil   := QAST::WVal.new( :value($*W.find_symbol(['Nil'])) );
        my $false := QAST::WVal.new( :value($*W.find_symbol(['Bool', 'False'])) );
        my $true  := QAST::WVal.new( :value($*W.find_symbol(['Bool', 'True'])) );
        
        # Need a state variable to track the state.
        my %cont;
        my $id    := $lhs.unique('FLIPFLOP_STATE_');
        my $state := '!' ~ $id;
        %cont{'bind_constraint'} := $*W.find_symbol(['Mu']);
        %cont{'container_type'}  := $*W.find_symbol(['Scalar']);
        %cont{'container_base'}  := %cont{'container_type'};
        %cont{'default_value'}   := $zero.compile_time_value;
        %cont{'scalar_value'}    := $zero.compile_time_value;
        $*W.install_lexical_container($*W.cur_lexpad(), $state, %cont,
            $*W.create_container_descriptor(%cont{'bind_constraint'}, 1, $state),
            :scope('state'));
            
        # Twiddle to make special-case RHS * work.
        if istype($rhs.returns, $*W.find_symbol(['Whatever'])) {
            $rhs := $false;
        }
        
        # Evaluate LHS and RHS. Note that in one-only mode, we use
        # the state bit to decide which side to evaluate.
        my $ff_code := QAST::Stmts.new(
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name($id ~ '_lhs'), :scope('local'), :decl('var') ),
                ($one_only ??
                    QAST::Op.new(
                        :op('if'),
                        QAST::Var.new( :name($state), :scope('lexical') ),
                        $false,
                        QAST::Op.new( :op('callmethod'), :name('Bool'), $lhs )
                    ) !!
                    QAST::Op.new( :op('callmethod'), :name('Bool'), $lhs ))
            ),
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name($id ~ '_rhs'), :scope('local'), :decl('var') ),
                ($one_only ??
                    QAST::Op.new(
                        :op('if'),
                        QAST::Var.new( :name($state), :scope('lexical') ),
                        QAST::Op.new( :op('callmethod'), :name('Bool'), $rhs ),
                        $false
                    ) !!
                    QAST::Op.new( :op('callmethod'), :name('Bool'), $rhs ))
            )
        );
        
        # Now decide what to do based on current state and current
        # results.
        $ff_code.push(QAST::Op.new(
            :op('if'),
            QAST::Var.new( :name($state), :scope('lexical') ),
            
            # State is currently true. Check RHS. If it's false, then we
            # increment the sequence count. If it's true, then we reset,
            # the state to zero and and what we return depends on $max_excl.
            QAST::Op.new(
                :op('if'),
                QAST::Var.new( :name($id ~ '_rhs'), :scope('local') ),
                ($max_excl ??
                    QAST::Stmts.new(
                        QAST::Op.new(
                            :op('p6store'),
                            QAST::Var.new( :name($state), :scope('lexical') ),
                            $zero
                        ),
                        $nil
                    ) !!
                    QAST::Stmts.new(
                        QAST::Op.new(
                            :op('bind'),
                            QAST::Var.new( :name($id ~ '_orig'), :scope('local'), :decl('var') ),
                            QAST::Op.new(
                                :op('call'), :name('&prefix:<++>'),
                                QAST::Var.new( :name($state), :scope('lexical') )
                            )
                        ),
                        QAST::Op.new(
                            :op('p6store'),
                            QAST::Var.new( :name($state), :scope('lexical') ),
                            $zero
                        ),
                        QAST::Op.new(
                            :op('decont'),
                            QAST::Var.new( :name($id ~ '_orig'), :scope('local') )
                        )
                    )),
                QAST::Stmts.new(
                    QAST::Op.new(
                        :op('call'), :name('&prefix:<++>'),
                        QAST::Var.new( :name($state), :scope('lexical') )
                    )
                )
            ),
            
            # State is currently false. Check LHS. If it's false, then we
            # stay in a false state. If it's true, then we flip the bit,
            # but only if the RHS is not also true. We return a result
            # based on $min_excl.
            QAST::Op.new(
                :op('if'),
                QAST::Var.new( :name($id ~ '_lhs'), :scope('local') ),
                QAST::Op.new(
                    :op('if'),
                    QAST::Var.new( :name($id ~ '_rhs'), :scope('local') ),
                    $min_excl || $max_excl ?? $nil !! $one,
                    QAST::Stmts.new(
                        QAST::Op.new(
                            :op('p6store'),
                            QAST::Var.new( :name($state), :scope('lexical') ),
                            $one
                        ),
                        $min_excl ?? $nil !! $one
                    )
                ),
                $nil
            )
        ));
        
        QAST::Op.new( :op('locallifetime'), $ff_code, $id ~ '_lhs', $id ~ '_rhs' );
    }

    method prefixish($/) {
        if $<prefix_postfix_meta_operator> {
            make QAST::Op.new( :node($/),
                     :name<&METAOP_HYPER_PREFIX>,
                     :op<call>,
                     QAST::Var.new( :name('&prefix:<' ~ $<OPER>.Str ~ '>'),
                                    :scope<lexical> ));
        }
    }

    sub baseop_reduce($/) {
        my str $reduce := 'LEFT';
        if $<prec> eq 'f='         { $reduce := 'LISTINFIX'; }
        elsif $<assoc> eq 'right'  
           || $<assoc> eq 'list'   { $reduce := nqp::uc($<assoc>); }
        elsif $<prec> eq 'm='      { $reduce := 'CHAIN'; }
        elsif $<pasttype> eq 'xor' { $reduce := 'XOR'; }
        '&METAOP_REDUCE_' ~ $reduce;
    }

    method infixish($/) {
        if $<infix_postfix_meta_operator> {
            my $base     := $<infix>;
            my $basesym  := ~$base<sym>;
            my $basepast := $base.ast
                              ?? $base.ast[0]
                              !! QAST::Var.new(:name("&infix:<$basesym>"),
                                               :scope<lexical>);
            if $basesym eq '||' || $basesym eq '&&' || $basesym eq '//' {
                make QAST::Op.new( :op<call>,
                        :name('&METAOP_TEST_ASSIGN:<' ~ $basesym ~ '>') );
            }
            else {
                make QAST::Op.new( :node($/), :op<call>,
                        QAST::Op.new( :op<call>,
                            :name<&METAOP_ASSIGN>, $basepast ));
            }
        }

        if $<infix_prefix_meta_operator> {
            my $metasym  := ~$<infix_prefix_meta_operator><sym>;
            my $base     := $<infix_prefix_meta_operator><infixish>;
            my $basesym  := ~$base<OPER>;
            my $basepast := $base.ast
                              ?? $base.ast[0]
                              !! QAST::Var.new(:name("&infix:<$basesym>"),
                                               :scope<lexical>);
            my $helper   := '';
            if    $metasym eq '!' { $helper := '&METAOP_NEGATE'; }
            if    $metasym eq 'R' { $helper := '&METAOP_REVERSE'; }
            elsif $metasym eq 'X' { $helper := '&METAOP_CROSS'; }
            elsif $metasym eq 'Z' { $helper := '&METAOP_ZIP'; }

            my $metapast := QAST::Op.new( :op<call>, :name($helper), $basepast );
            $metapast.push(QAST::Var.new(:name(baseop_reduce($base<OPER><O>)),
                                         :scope<lexical>))
                if $metasym eq 'X' || $metasym eq 'Z';
            make QAST::Op.new( :node($/), :op<call>, $metapast );
        }

        if $<infixish> {
            make $<infixish>.ast;
        }
    }

    method term:sym<reduce>($/) {
        my $base     := $<op>;
        my $basepast := $base.ast
                          ?? $base.ast[0]
                          !! QAST::Var.new(:name("&infix:<" ~ $base<OPER><sym> ~ ">"),
                                           :scope<lexical>);
        my $metaop   := baseop_reduce($base<OPER><O>);
        my $metapast := QAST::Op.new( :op<call>, :name($metaop), $basepast);
        if $<triangle> {
            my $tri := $*W.add_constant('Int', 'int', 1);
            $tri.named('triangle');
            $metapast.push($tri);
        }
        my $args := $<args>.ast;
        $args.name('&infix:<,>');
        make QAST::Op.new(:node($/), :op<call>, $metapast, $args);
    }

    method infix_circumfix_meta_operator:sym«<< >>»($/) {
        make make_hyperop($/);
    }

    method infix_circumfix_meta_operator:sym<« »>($/) {
        make make_hyperop($/);
    }

    sub make_hyperop($/) {
        my $base     := $<infixish>;
        my $basesym  := ~ $base<OPER>;
        my $basepast := $base.ast
                          ?? $base.ast[0]
                          !! QAST::Var.new(:name("&infix:<$basesym>"),
                                           :scope<lexical>);
        my $hpast    := QAST::Op.new(:op<call>, :name<&METAOP_HYPER>, $basepast);
        if $<opening> eq '<<' || $<opening> eq '«' {
            my $dwim := $*W.add_constant('Int', 'int', 1);
            $dwim.named('dwim-left');
            $hpast.push($dwim);
        }
        if $<closing> eq '>>' || $<closing> eq '»' {
            my $dwim := $*W.add_constant('Int', 'int', 1);
            $dwim.named('dwim-right');
            $hpast.push($dwim);
        }
        QAST::Op.new( :node($/), :op<call>, $hpast )
    }

    method postfixish($/) {
        if $<postfix_prefix_meta_operator> {
            my $past := $<OPER>.ast || QAST::Op.new( :name('&postfix:<' ~ $<OPER>.Str ~ '>'),
                                                     :op<call> );
            if $past.isa(QAST::Op) && $past.op() eq 'callmethod' {
                if $past.name -> $name {
                    $past.unshift(QAST::SVal.new( :value($name) ));
                }
                $past.name('dispatch:<hyper>');
            }
            elsif $past.isa(QAST::Op) && $past.op() eq 'call' {
                if $<dotty> {
                    $past.name('&METAOP_HYPER_CALL');
                }
                else {
                    my $basepast := $past.name 
                                    ?? QAST::Var.new( :name($past.name), :scope<lexical>)
                                    !! $past[0];
                    $past.push($basepast);
                    $past.name('&METAOP_HYPER_POSTFIX');
                }
            }
            make $past;
        }
    }

    method postcircumfix:sym<[ ]>($/) {
        my $past := QAST::Op.new( :name('&postcircumfix:<[ ]>'), :op('call'), :node($/) );
        if $<semilist><statement> {
            my $slast := $<semilist>.ast;
            $past.push($slast);
        }
        make $past;
    }

    method postcircumfix:sym<{ }>($/) {
        my $past := QAST::Op.new( :name('&postcircumfix:<{ }>'), :op('call'), :node($/) );
        if $<semilist><statement> {
            if +$<semilist><statement> > 1 {
                $*W.throw($/, 'X::Comp::NYI', feature => 'multi-dimensional indexes');
            }
            $past.push($<semilist>.ast);
        }
        make $past;
    }

    method postcircumfix:sym<ang>($/) {
        my $past := QAST::Op.new( :name('&postcircumfix:<{ }>'), :op('call'), :node($/) );
        my $nib  := $<nibble>.ast;
        $past.push($nib)
            unless nqp::istype($nib, QAST::Stmts) && nqp::istype($nib[0], QAST::Op) &&
            $nib[0].name eq '&infix:<,>' && +@($nib[0]) == 0;
        make $past;
    }

    method postcircumfix:sym«<< >>»($/) {
        my $past := QAST::Op.new( :name('&postcircumfix:<{ }>'), :op('call'), :node($/) );
        my $nib  := $<nibble>.ast;
        $past.push($nib)
            unless nqp::istype($nib, QAST::Stmts) && nqp::istype($nib[0], QAST::Op) &&
            $nib[0].name eq '&infix:<,>' && +@($nib[0]) == 0;
        make $past;
    }

    method postcircumfix:sym<« »>($/) {
        my $past := QAST::Op.new( :name('&postcircumfix:<{ }>'), :op('call'), :node($/) );
        my $nib  := $<nibble>.ast;
        $past.push($nib)
            unless nqp::istype($nib, QAST::Stmts) && nqp::istype($nib[0], QAST::Op) &&
            $nib[0].name eq '&infix:<,>' && +@($nib[0]) == 0;
        make $past;
    }

    method postcircumfix:sym<( )>($/) {
        make $<arglist>.ast;
    }

    method value:sym<quote>($/) {
        make $<quote>.ast;
    }

    method value:sym<number>($/) {
        make $<number>.ast;
    }
    method value:sym<version>($/) {
        make $<version>.ast;
    }

    method version($/) {
        my $v := $*W.find_symbol(['Version']).new(~$<vstr>);
        $*W.add_object($v);
        make QAST::WVal.new( :value($v) );
    }

    method decint($/) { make string_to_bigint( $/, 10); }
    method hexint($/) { make string_to_bigint( $/, 16); }
    method octint($/) { make string_to_bigint( $/, 8 ); }
    method binint($/) { make string_to_bigint( $/, 2 ); }


    method number:sym<complex>($/) {
        my $re := $*W.add_constant('Num', 'num', 0e0);
        my $im := $*W.add_constant('Num', 'num', +~$<im>);
        make $*W.add_constant('Complex', 'type_new', $re.compile_time_value, $im.compile_time_value);
    }

    method number:sym<numish>($/) {
        make $<numish>.ast;
    }

    method numish($/) {
        if $<integer> {
            make $*W.add_numeric_constant($/, 'Int', $<integer>.ast);
        }
        elsif $<dec_number> { make $<dec_number>.ast; }
        elsif $<rad_number> { make $<rad_number>.ast; }
        else {
            make $*W.add_numeric_constant($/, 'Num', +$/);
        }
    }

    method escale($/) {
        make $<sign> eq '-'
            ??  nqp::neg_I($<decint>.ast, $<decint>.ast)
            !! $<decint>.ast;
    }

    method dec_number($/) {
#        say("dec_number: $/");
        if $<escale> {
#            say('dec_number exponent: ' ~ ~$e.ast);
            make radcalc($/, 10, $<coeff>, 10, nqp::unbox_i($<escale>.ast), :num);
        } else {
            make radcalc($/, 10, $<coeff>);
        }
    }

    method rad_number($/) {
        my $radix    := +($<radix>.Str);
        if $<bracket>   {
            make QAST::Op.new(:name('&unbase_bracket'), :op('call'),
                $*W.add_numeric_constant($/, 'Int', $radix), $<bracket>.ast);
        }
        elsif $<circumfix> {
            make QAST::Op.new(:name('&unbase'), :op('call'),
                $*W.add_numeric_constant($/, 'Int', $radix), $<circumfix>.ast);
        } else {
            my $intpart  := $<intpart>.Str;
            my $fracpart := $<fracpart> ?? $<fracpart>.Str !! "0";
            my $intfrac  := $intpart ~ $fracpart; #the dot is a part of $fracpart, so no need for ~ "." ~

            my $base;
            my $exp;
            $base   := +($<base>[0].Str) if $<base>;
            $exp    := +($<exp>[0].Str)  if $<exp>;

            my $error;
            make radcalc($/, $radix, $intfrac, $base, $exp);
        }
    }

    method typename($/) {
        # Locate the type object and make that. Anything that wants a PAST
        # reference to it can obtain one, but many things really want the
        # actual type object to build up some data structure or make a trait
        # dispatch with. Note that for '::T' style things we need to make a
        # GenericHOW, though whether/how it's used depends on context.
        if $<longname> {
            if nqp::substr(~$<longname>, 0, 2) ne '::' {
                my $longname := $*W.dissect_longname($<longname>);
                my $type := $*W.find_symbol($longname.type_name_parts('type name'));
                if $<arglist> {
                    $type := $*W.parameterize_type($type, $<arglist>[0].ast, $/);
                }
                if $<typename> {
                    $type := $*W.parameterize_type_with_args($type,
                        [$<typename>.ast], hash());
                }
                make $type;
            }
            else {
                if $<arglist> || $<typename> {
                    $/.CURSOR.panic("Cannot put type parameters on a type capture");
                }
                if ~$<longname> eq '::' {
                    $/.CURSOR.panic("Cannot use :: as a type name");
                }
                make $*W.pkg_create_mo($/, %*HOW<generic>, :name(nqp::substr(~$<longname>, 2)));
            }
        }
        else {
            make $*W.find_symbol(['::?' ~ ~$<identifier>]);
        }
    }

    my %SUBST_ALLOWED_ADVERBS;
    my %SHARED_ALLOWED_ADVERBS;
    my %MATCH_ALLOWED_ADVERBS;
    my %MATCH_ADVERBS_MULTIPLE := hash(
        x       => 1,
        g       => 1,
        global  => 1,
        ov      => 1,
        overlap => 1,
        ex      => 1,
        exhaustive => 1,
    );
    my %REGEX_ADVERBS_CANONICAL := hash(
        ignorecase  => 'i',
        ratchet     => 'r',
        sigspace    => 's',
        continue    => 'c',
        pos         => 'p',
        th          => 'nth',
        st          => 'nth',
        nd          => 'nth',
        rd          => 'nth',
        global      => 'g',
        overlap     => 'ov',
        exhaustive  => 'ex',
        Perl5       => 'P5',
        samecase    => 'ii',
        samespace   => 'ss',
    );
    my %REGEX_ADVERB_IMPLIES := hash(
        ii        => 'i',
        ss        => 's',
    );
    INIT {
        my str $mods := 'i ignorecase s sigspace r ratchet Perl5 P5';
        for nqp::split(' ', $mods) {
            %SHARED_ALLOWED_ADVERBS{$_} := 1;
        }

        $mods := 'g global ii samecase ss samespace x c continue p pos nth th st nd rd';
        for nqp::split(' ', $mods) {
            %SUBST_ALLOWED_ADVERBS{$_} := 1;
        }

        $mods := 'x c continue p pos nth th st nd rd g global ov overlap ex exhaustive';
        for nqp::split(' ', $mods) {
            %MATCH_ALLOWED_ADVERBS{$_} := 1;
        }
    }


    method quotepair($/) {
        unless $*value ~~ QAST::Node {
            if $*purpose eq 'rxadverb' && ($*key eq 'c' || $*key eq 'continue'
            || $*key eq 'p' || $*key eq 'pos') && $*value == 1 {
                $*value := QAST::Op.new(
                    :node($/),
                    :op<if>,
                    QAST::Var.new(:name('$/'), :scope('lexical')),
                    QAST::Op.new(:op('callmethod'),
                        QAST::Var.new(:name('$/'), :scope<lexical>),
                        :name<to>
                    ),
                    QAST::IVal.new(:value(0)),
                );
            } else {
                $*value := QAST::IVal.new( :value($*value) );
            }
        }
        $*value.named(~$*key);
        make $*value;
    }

    method rx_adverbs($/) {
        my @pairs;
        for $<quotepair> {
            nqp::push(@pairs, $_.ast);
        }
        make @pairs;
    }

    method setup_quotepair($/) {
        my %h;
        my $key := $*ADVERB.ast.named;
        my $value := $*ADVERB.ast;
        if $value ~~ QAST::IVal || $value ~~ QAST::SVal {
            $value := $value.value;
        }
        elsif $value.has_compile_time_value {
            $value := $value.compile_time_value;
        }
        else {
            if %SHARED_ALLOWED_ADVERBS{$key} {
                $*W.throw($/, ['X', 'Value', 'Dynamic'], what => "Adverb $key");
            }
        }
        $key := %REGEX_ADVERBS_CANONICAL{$key} // $key;
        %*RX{$key} := $value;
        if %REGEX_ADVERB_IMPLIES{$key} {
            %*RX{%REGEX_ADVERB_IMPLIES{$key}} := $value
        }
        $value;
    }

    method quote:sym<apos>($/) { make $<nibble>.ast; }
    method quote:sym<dblq>($/) { make $<nibble>.ast; }
    method quote:sym<crnr>($/) { make $<nibble>.ast; }
    method quote:sym<qq>($/)   { make $<quibble>.ast; }
    method quote:sym<q>($/)    { make $<quibble>.ast; }
    method quote:sym<Q>($/)    { make $<quibble>.ast; }
    method quote:sym<Q:PIR>($/) {
        if $FORBID_PIR {
            nqp::die("Q:PIR forbidden in safe mode\n");
        }
        my $pir := compile_time_value_str($<quibble>.ast, "Q:PIR", $/);
        make QAST::VM.new( :pir($pir), :node($/) );
    }
    method quote:sym</ />($/) {
        my %sig_info := hash(parameters => []);
        my $block := QAST::Block.new(QAST::Stmts.new, QAST::Stmts.new, :node($/));
        my $coderef := regex_coderef($/, $*W.stub_code_object('Regex'),
            $<nibble>.ast, 'anon', '', %sig_info, $block, :use_outer_match(1)) if $<nibble>.ast;
        # Return closure if not in sink context.
        my $closure := block_closure($coderef);
        $closure<sink_past> := QAST::Op.new( :op<callmethod>, :name<Bool>, $closure);
        make $closure;
    }

    method quote:sym<rx>($/) {
        my $block := QAST::Block.new(QAST::Stmts.new, QAST::Stmts.new, :node($/));
        self.handle_and_check_adverbs($/, %SHARED_ALLOWED_ADVERBS, 'rx', $block);
        my %sig_info := hash(parameters => []);
        my $coderef := regex_coderef($/, $*W.stub_code_object('Regex'),
            $<quibble>.ast, 'anon', '', %sig_info, $block, :use_outer_match(1)) if $<quibble>.ast;
        my $past := block_closure($coderef);
        $past<sink_past> := QAST::Op.new(:op<callmethod>, :name<Bool>, $past);
        make $past;
    }
    method quote:sym<m>($/) {
        my $block := QAST::Block.new(QAST::Stmts.new, QAST::Stmts.new, :node($/));
        my %sig_info := hash(parameters => []);
        my $coderef := regex_coderef($/, $*W.stub_code_object('Regex'),
            $<quibble>.ast, 'anon', '', %sig_info, $block, :use_outer_match(1)) if $<quibble>.ast;

        my $past := QAST::Op.new(
            :node($/),
            :op('callmethod'), :name('match'),
            QAST::Var.new( :name('$_'), :scope('lexical') ),
            block_closure($coderef)
        );
        if self.handle_and_check_adverbs($/, %MATCH_ALLOWED_ADVERBS, 'm', $past) {
            # if this match returns a list of matches instead of a single
            # match, don't assing to $/ (which imposes item context)
            make $past;
        } else {
            make QAST::Op.new( :op('p6store'),
                QAST::Var.new(:name('$/'), :scope('lexical')),
                $past
            );
        }
    }

    # returns 1 if the adverbs indicate that the return value of the
    # match will be a List of matches rather than a single match
    method handle_and_check_adverbs($/, %adverbs, $what, $past?) {
        my int $multiple := 0;
        for $<rx_adverbs>.ast {
            $multiple := 1 if %MATCH_ADVERBS_MULTIPLE{$_.named};
            unless %SHARED_ALLOWED_ADVERBS{$_.named} || %adverbs{$_.named} {
                $*W.throw($/, 'X::Syntax::Regex::Adverb',
                    adverb    => $_.named,
                    construct => $what,
                );
            }
            if $past {
                $past.push($_);
            }
        }
        $multiple;
    }

    method quote:sym<tr>($/) {
        if nqp::elems($<rx_adverbs><quotepair>) {
            $*W.throw($/, 'X::Comp::NYI', feature => 'tr/// adverbs');
        }
        my $left  := ~$<tribble><left>;
        my $right := ~$<tribble><right>;
        make QAST::Op.new(:op<p6store>,
            QAST::Var.new(:name('$_'), :scope<lexical>),
            QAST::Op.new(:op<callmethod>, :name<trans>,
                QAST::Var.new(:name('$_'), :scope<lexical>),
                make_pair($left, QAST::SVal.new(:value($right))),
            )
        );
    }

    method quote:sym<s>($/) {
        # Build the regex.
        my $rx_block := QAST::Block.new(QAST::Stmts.new, QAST::Stmts.new, :node($/));
        my %sig_info := hash(parameters => []);
        my $rx_coderef := regex_coderef($/, $*W.stub_code_object('Regex'),
            $<sibble><left>.ast, 'anon', '', %sig_info, $rx_block, :use_outer_match(1)) if $<sibble><left>.ast;

        # Quote needs to be closure-i-fied.
        my $closure := block_closure(make_thunk_ref($<sibble><right>.ast, $<sibble><right>));

        # make $_ = $_.subst(...)
        my $past := QAST::Op.new(
            :node($/),
            :op('callmethod'), :name('subst'),
            QAST::Var.new( :name('$_'), :scope('lexical') ),
            $rx_coderef, $closure
        );
        self.handle_and_check_adverbs($/, %SUBST_ALLOWED_ADVERBS, 'substitution', $past);
        if $/[0] {
            $past.push(QAST::IVal.new(:named('samespace'), :value(1)));
        }
        $past.push(QAST::IVal.new(:named('SET_CALLER_DOLLAR_SLASH'), :value(1)));

        $past := make QAST::Op.new(
            :node($/),
            :op('call'),
            :name('&infix:<=>'),
            QAST::Var.new(:name('$_'), :scope('lexical')),
            $past
        );

        $past<is_subst> := 1;
        $past
}

    method quote:sym<quasi>($/) {
        my $ast_class := $*W.find_symbol(['AST']);
        my $quasi_ast := $ast_class.new();
        my $past := $<block>.ast<past_block>.pop;
        nqp::bindattr($quasi_ast, $ast_class, '$!past', $past);
        $*W.add_object($quasi_ast);
        my $throwaway_block := QAST::Block.new();
        my $quasi_context := block_closure(
            reference_to_code_object(
                $*W.create_simple_code_object($throwaway_block, 'Block'),
                $throwaway_block
            ));
        make QAST::Op.new(:op<callmethod>, :name<incarnate>,
                          QAST::WVal.new( :value($quasi_ast) ),
                          $quasi_context,
                          QAST::Op.new( :op('list'), |@*UNQUOTE_ASTS ));
    }

    # Adds code to do the signature binding.
    my $use_vm_binder;
    sub add_signature_binding_code($block, $sig_obj, @params) {
        # Set arity.
        my int $arity := 0;
        for @params {
            last if $_<optional> || $_<named_names> ||
               $_<pos_slurpy> || $_<named_slurpy>;
            $arity := $arity + 1;
        }
        $block.arity($arity);

        # Consider using the VM binder on backends where it will work out
        # (e.g. we can get the same errors).
        my $need_full_binder := 1;
        unless nqp::defined($use_vm_binder) {
            $use_vm_binder := nqp::getcomp('perl6').backend.name eq 'moar';
        }
        if $use_vm_binder {
            # If there are zero parameters, then we can trvially leave it to
            # the VM, with no extra work.
            if nqp::elems(@params) == 0 {
                $need_full_binder := 0;
            }

            # If there is one anonymous capture parameter, as is common with
            # protos, then we've nothing to check or store; flag as custom
            # bind but don't invoke the binder.
            elsif is_anon_capture(@params) {
                $block.custom_args(1);
                $need_full_binder := 0;
            }

            # If there is a single $_ and it takes its value from outer, then
            # this is easily handled too. Very common case.
            elsif is_default_topic(@params) {
                my $var := find_var_decl($block, '$_');
                $var.decl('param');
                $var.default(QAST::Op.new(
                    :op('getlexouter'),
                    QAST::SVal.new( :value('$_') )
                ));
                $need_full_binder := 0;
            }

            # Otherwise, need to go through and do a fuller analysis.
            else {
                if lower_signature($block, $sig_obj, @params) -> @todo {
                    for @todo {
                        $block[0].push($_);
                    }
                    $need_full_binder := 0;
                }
            }
        }

        # If we need the full binder, invoke it; mark we do custom args
        # handling.
        if $need_full_binder {
            $block.custom_args(1);
            $block[0].push(QAST::Op.new( :op('p6bindsig') ));
        }

        $block;
    }
    sub is_anon_capture(@params) {
        if nqp::elems(@params) == 1 {
            my $only := @params[0];
            if $only<is_capture> && !nqp::existskey($only, 'variable_name') {
                if !nqp::istype($*W.find_symbol(['Capture']), $only<nominal_type>) {
                    $only<node>.CURSOR.panic("Capture parameter must have a type accepting a Capture");
                }
                else {
                    return 1;
                }
            }
        }
        0
    }
    sub is_default_topic(@params) {
        if nqp::elems(@params) == 1 {
            my $only := @params[0];
            if $only<default_from_outer> && $only<is_parcel> && $only<variable_name> eq '$_' {
                if $only<nominal_type> =:= $*W.find_symbol(['Mu']) {
                    return 1;
                }
            }
        }
        0
    }
    my $SIG_ELEM_IS_RW       := 256;
    my $SIG_ELEM_IS_COPY     := 512;
    my $SIG_ELEM_IS_PARCEL   := 1024;
    my $SIG_ELEM_IS_OPTIONAL := 2048;
    sub lower_signature($block, $sig, @params) {
        my @result;
        my $clear_topic_bind;
        my $saw_slurpy;
        my $Sig      := $*W.find_symbol(['Signature']);
        my $Param    := $*W.find_symbol(['Parameter']);
        my $Iterable := $*W.find_symbol(['Iterable']);
        my @p_objs := nqp::getattr($sig, $Sig, '$!params');
        my int $i  := 0;
        my int $n  := nqp::elems(@params);
        while $i < $n {
            # Some things need the full binder to do.
            my %info      := @params[$i];
            my $param_obj := @p_objs[$i];
            my int $flags := nqp::getattr_i($param_obj, $Param, '$!flags');
            return 0 if nqp::existskey(%info, 'sub_signature');
            return 0 if nqp::existskey(%info, 'type_captures'); # XXX Support later
            return 0 if %info<bind_attr>;                       # XXX Support later
            return 0 if %info<bind_accessor>;                   # XXX Support later
            return 0 if %info<nominal_generic>;                 # XXX Support later
            return 0 if %info<default_from_outer>;

            # Generate a var to bind into.
            my $name := "__lowered_param_$i";
            my $var  := QAST::Var.new( :$name, :scope('local'), :decl('param') );
            if %info<is_capture> {
                # If this is a final and anonymous capture, then we're good.
                # Otherwise, bail out for now.
                return 0 if $saw_slurpy;
                return 0 unless $i + 1 == $n ||
                                $i + 2 == $n && @params[$i + 1]<named_slurpy>;
                if !nqp::istype($*W.find_symbol(['Capture']), %info<nominal_type>) {
                    %info<node>.CURSOR.panic("Capture parameter must have a type accepting a Capture");
                }
                $var.slurpy(1);
                my $hash_name := $name ~ '_hash';
                @result.push(QAST::Var.new(
                    :name($hash_name), :scope('local'), :decl('param'),
                    :slurpy(1), :named(1)
                ));
                if nqp::existskey(%info, 'variable_name') {
                    # Build a capture object.
                    my $Capture := QAST::WVal.new( :value($*W.find_symbol(['Capture'])) );
                    $var.push(QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name($name), :scope('local') ),
                        QAST::Op.new(
                            :op('p6bindattrinvres'),
                            QAST::Op.new(
                                :op('p6bindattrinvres'),
                                QAST::Op.new( :op('create'), $Capture ),
                                $Capture,
                                QAST::SVal.new( :value('$!list') ),
                                QAST::Var.new( :name($name), :scope('local') )
                            ),
                            $Capture,
                            QAST::SVal.new( :value('$!hash') ),
                            QAST::Var.new( :name($hash_name), :scope('local') )
                        )));
                }
            }
            elsif nqp::existskey(%info, 'named_names') {
                my @names := %info<named_names>;
                return 0 if nqp::elems(@names) != 1;
                $var.named(@names[0]);
            }
            elsif %info<pos_slurpy> || %info<pos_lol> {
                $var.slurpy(1);
                my $type := $*W.find_symbol([ %info<pos_lol> ?? 'LoL' !!
                    $flags +& $SIG_ELEM_IS_RW ?? 'List' !! 'Array' ]);
                my $flat := %info<pos_lol> ?? 'False' !! 'True';
                $var.push(QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :name($name), :scope('local') ),
                    QAST::Op.new(
                        :op('p6list'),
                        QAST::Var.new( :name($name), :scope('local') ),
                        QAST::WVal.new( :value($type) ),
                        QAST::WVal.new( :value($*W.find_symbol([$flat])) )
                    )));
                $saw_slurpy := 1;
            }
            elsif %info<named_slurpy> {
                $var.slurpy(1);
                $var.named(1);
                my $slurpy_setup := QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :name($name), :scope('local') ),
                    QAST::Op.new(
                        :op('p6bindattrinvres'),
                        QAST::Op.new(
                            :op('create'),
                            QAST::WVal.new( :value($*W.find_symbol(['Hash'])) )
                        ),
                        QAST::WVal.new( :value($*W.find_symbol(['EnumMap'])) ),
                        QAST::SVal.new( :value('$!storage') ),
                        QAST::Var.new( :name($name), :scope('local') )
                    ));
                if nqp::existskey(%info, 'variable_name') && %info<variable_name> eq '%_' {
                    $slurpy_setup<autoslurpy> := 1;
                }
                $var.push($slurpy_setup);
                $saw_slurpy := 1;
            }

            # Add type checks.
            my $nomtype := %info<nominal_type>;
            if nqp::objprimspec($nomtype) {
                $var.returns($nomtype);
            }
            elsif !$saw_slurpy {
                # Must hll-ize before we go on.
                $var.push(QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :name($name), :scope('local') ),
                    QAST::Op.new(
                        :op('hllize'),
                        QAST::Var.new( :name($name), :scope('local') )
                    )));

                # Type-check, unless it's Mu, in which case skip it.
                unless $nomtype =:= $*W.find_symbol(['Mu']) {
                    if $nomtype.HOW.archetypes.generic {
                        return 0 unless %info<is_invocant>;
                    }
                    else {
                        $var.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                            :op('istype'),
                            QAST::Var.new( :name($name), :scope('local') ),
                            QAST::WVal.new( :value($nomtype) )
                        )));
                    }
                }
                if %info<undefined_only> {
                    $var.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                        :op('not_i'),
                        QAST::Op.new(
                            :op('isconcrete'),
                            QAST::Var.new( :name($name), :scope('local') )
                        ))));
                }
                if %info<defined_only> {
                    $var.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                        :op('isconcrete'),
                        QAST::Var.new( :name($name), :scope('local') )
                    )));
                }
            }

            # Handle coercion.
            my $coerce_to := nqp::getattr($param_obj, $Param, '$!coerce_type');
            unless nqp::isnull($coerce_to) {
                $var.push(QAST::Op.new(
                    :op('unless'),
                    QAST::Op.new(
                        :op('istype'),
                        QAST::Var.new( :name($name), :scope('local') ),
                        QAST::WVal.new( :value($coerce_to) )
                    ),
                    QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name($name), :scope('local') ),
                        QAST::Op.new(
                            :op('callmethod'),
                            :name(nqp::getattr_s($param_obj, $Param, '$!coerce_method')),
                            QAST::Var.new( :name($name), :scope('local') )
                        ))));
            }

            # If it's optional, do any default handling.
            if $flags +& $SIG_ELEM_IS_OPTIONAL {
                if nqp::existskey(%info, 'default_value') {
                    my $wval := QAST::WVal.new( :value(%info<default_value>) );
                    if %info<default_is_literal> {
                        $var.default($wval);
                    }
                    else {
                        $var.default(QAST::Op.new(
                            :op('call'),
                            QAST::Op.new(
                                :op('p6capturelex'),
                                QAST::Op.new( :op('callmethod'), :name('clone'), $wval )
                            )));
                    }
                }
                else {
                    if %info<sigil> eq '@' {
                        return 0;
                    }
                    elsif %info<sigil> eq '%' {
                        return 0;
                    }
                    else {
                        my int $ps := nqp::objprimspec($nomtype);
                        if $ps == 1 {
                            $var.default(QAST::IVal.new( :value(0) ));
                        }
                        elsif $ps == 2 {
                            $var.default(QAST::NVal.new( :value(0.0) ));
                        }
                        elsif $ps == 3 {
                            $var.default(QAST::SVal.new( :value('') ));
                        }
                        else {
                            $var.default(QAST::WVal.new( :value($nomtype) ));
                        }
                    }
                }
            }

            # If it's the invocant, needs to go into self also.
            if %info<is_invocant> {
                $var.push(QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :name('self'), :scope('lexical') ),
                    QAST::Op.new(
                        :op('decont'),
                        QAST::Var.new( :name($name), :scope('local') )
                    )));
            }

            # Bind to lexical if needed.
            if nqp::existskey(%info, 'variable_name') {
                if nqp::objprimspec($nomtype) || $flags +& $SIG_ELEM_IS_RW || $flags +& $SIG_ELEM_IS_PARCEL {
                    $var.push(QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name(%info<variable_name>), :scope('lexical'), :returns($nomtype) ),
                        QAST::Var.new( :name($name), :scope('local') )
                    ));
                }
                elsif %info<sigil> eq '@' {
                    if $flags +& $SIG_ELEM_IS_COPY {
                        return 0;
                    }
                    else {
                        $var.push(QAST::Op.new(
                            :op('bind'),
                            QAST::Var.new( :name(%info<variable_name>), :scope('lexical') ),
                            QAST::Op.new(
                                :op('decont'),
                                QAST::Var.new( :name($name), :scope('local') )
                            )));
                    }
                }
                elsif %info<sigil> eq '%' {
                    if $flags +& $SIG_ELEM_IS_COPY {
                        return 0;
                    }
                    else {
                        $var.push(QAST::Op.new(
                            :op('bind'),
                            QAST::Var.new( :name(%info<variable_name>), :scope('lexical') ),
                            QAST::Op.new(
                                :op('decont'),
                                QAST::Var.new( :name($name), :scope('local') )
                            )));
                    }
                }
                else {
                    # If the type means there's no way this could possible be
                    # Iterable, we know it can't flatten. This in turn means
                    # we need not wrap it in a read-only scalar.
                    my $wrap := $flags +& $SIG_ELEM_IS_COPY;
                    unless $wrap {
                        $wrap := nqp::istype($nomtype, $Iterable) || nqp::istype($Iterable, $nomtype);
                    }
                    if $wrap {
                        $var.push(QAST::Op.new(
                            :op('bind'),
                            QAST::Var.new( :name(%info<variable_name>), :scope('lexical') ),
                            QAST::Op.new(
                                :op('assignunchecked'),
                                QAST::Op.new(
                                    :op('p6scalarfromdesc'),
                                    QAST::WVal.new( :value(%info<container_descriptor>) )
                                ),
                                QAST::Var.new( :name($name), :scope('local') )
                            )));
                    }
                    else {
                        $var.push(QAST::Op.new(
                            :op('bind'),
                            QAST::Var.new( :name(%info<variable_name>), :scope('lexical') ),
                            QAST::Op.new(
                                :op('decont'),
                                QAST::Var.new( :name($name), :scope('local') )
                            )));
                    }

                    # Take care we don't undo explicit $_ bindings.
                    if %info<variable_name> eq '$_' && $*IMPLICIT {
                        for $block[0].list {
                            if nqp::istype($_, QAST::Op) && $_.op eq 'bind' &&
                               nqp::istype($_[0], QAST::Var) && $_[0].name eq '$_' {
                                $clear_topic_bind := $_;
                                last;
                            }
                        }
                    }
                }
            }

            # Finally, apply post-constraints (must come after variable bind,
            # as constraints can refer to the var).
            if %info<post_constraints> {
                for %info<post_constraints> {
                    my $wval := QAST::WVal.new( :value($_) );
                    $var.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                        :op('istrue'),
                        QAST::Op.new(
                            :op('callmethod'), :name('ACCEPTS'),
                            nqp::istype($_, $*W.find_symbol(['Code']))
                                ?? QAST::Op.new( :op('p6capturelex'),
                                      QAST::Op.new( :op('callmethod'), :name('clone'), $wval ) )
                                !! $wval,
                            QAST::Var.new( :name($name), :scope('local') )
                        ))));
                }
            }

            # Add the generated var.
            nqp::push(@result, $var);

            $i++;
        }
        if $clear_topic_bind {
            $clear_topic_bind.shift(); $clear_topic_bind.shift();
            $clear_topic_bind.op('null');
        }
        @result
    }
    sub find_var_decl($block, $name) {
        for $block[0].list {
            if nqp::istype($_, QAST::Var) && $_.name eq $name && $_.decl {
                return $_;
            }
        }
        nqp::die("Internal error: find_var_decl could not find $name");
    }

    # Adds a placeholder parameter to this block's signature.
    sub add_placeholder_parameter($/, $sigil, $ident, :$named, :$pos_slurpy, :$named_slurpy, :$full_name) {
        my $block := $*W.cur_lexpad();

        # don't allow $^A..Z as placeholders, as per spec
        if nqp::chars($full_name) == 3 && nqp::substr($full_name,2,1) ~~ /^<[A..Z]>$/ {
            $*W.throw($/, ['X', 'Syntax', 'Perl5Var'], name => $full_name );
        }

        # ensure we're not trying to put a placeholder in the mainline.
        elsif $block<IN_DECL> eq 'mainline' || $block<IN_DECL> eq 'eval' {
            $*W.throw($/, ['X', 'Placeholder', 'Mainline'],
                placeholder => $full_name,
            );
        }
        
        # Obtain/create placeholder parameter list.
        my @params := $block<placeholder_sig> || ($block<placeholder_sig> := []);

        # If we already declared this as a placeholder, we're done.
        my $name := ~$sigil ~ ~$ident;
        for @params {
            if $_<variable_name> eq $name {
                return QAST::Var.new( :name($name), :scope('lexical') );
            }
        }

        # Make descriptor.
        my %param_info := hash(
            variable_name => $name,
            pos_slurpy    => $pos_slurpy,
            named_slurpy  => $named_slurpy,
            placeholder   => $full_name,
            sigil         => ~$sigil);

        # If it's slurpy, just goes on the end.
        if $pos_slurpy || $named_slurpy {
            @params.push(%param_info);
        }

        # If it's named, just shove it on the end, but before any slurpies.
        elsif $named {
            %param_info<named_names> := [$ident];
            my @popped;
            while @params
                    && (@params[+@params - 1]<pos_slurpy> || @params[+@params - 1]<named_slurpy>) {
                @popped.push(@params.pop);
            }
            @params.push(%param_info);
            while @popped { @params.push(@popped.pop) }
        }

        # Otherwise, put it in correct lexicographic position.
        else {
            my int $insert_at := 0;
            for @params {
                last if $_<pos_slurpy> || $_<named_slurpy> ||
                        $_<named_names> ||
                        nqp::substr($_<variable_name>, 1) gt $ident;
                $insert_at := $insert_at + 1;
            }
            nqp::splice(@params, [%param_info], $insert_at, 0);
        }

        # Add variable declaration, and evaluate to a lookup of it.
        my %existing := $block.symbol($name);
        if +%existing && !%existing<placeholder_parameter> {
            $*W.throw($/, ['X', 'Redeclaration'],
                symbol  => ~$/,
                postfix => ' as a placeholder parameter',
            );
        }
        $block[0].push(QAST::Var.new( :name($name), :scope('lexical'), :decl('var') ));
        $block.symbol($name, :scope('lexical'), :placeholder_parameter(1));
        QAST::Var.new( :name($name), :scope('lexical') )
    }

    sub reference_to_code_object($code_obj, $past_block) {
        my $ref := QAST::WVal.new( :value($code_obj) );
        $ref<past_block> := $past_block;
        $ref<code_object> := $code_obj;
        $ref
    }

    sub block_closure($code) {
        my $closure := QAST::Op.new(
            :op('callmethod'), :name('clone'),
            $code
        );
        $closure := QAST::Op.new( :op('p6capturelex'), $closure);
        $closure<past_block> := $code<past_block>;
        $closure<code_object> := $code<code_object>;
        $closure
    }

    sub make_thunk_ref($to_thunk, $/) {
        my $block := $*W.push_lexpad($/);
        $block.push(QAST::Stmts.new(autosink($to_thunk)));
        $*W.pop_lexpad();
        reference_to_code_object(
            $*W.create_simple_code_object($block, 'Code'),
            $block);
    }

    sub make_topic_block_ref($past, :$copy) {
        my $block := QAST::Block.new(
            QAST::Stmts.new(
                QAST::Var.new( :name('$_'), :scope('lexical'), :decl('var') )
            ),
            $past);
        ($*W.cur_lexpad())[0].push($block);
        my $param := hash( :variable_name('$_'), :nominal_type($*W.find_symbol(['Mu'])));
        if $copy {
            $param<container_descriptor> := $*W.create_container_descriptor(
                    $*W.find_symbol(['Mu']), 0, '$_'
            );
        }
        my $param_obj := $*W.create_parameter($param);
        if $copy { $param_obj.set_copy() } else { $param_obj.set_rw() }
        my $sig := $*W.create_signature(nqp::hash('parameters', [$param_obj]));
        add_signature_binding_code($block, $sig, [$param]);
        reference_to_code_object(
            $*W.create_code_object($block, 'Block', $sig),
            $block)
    }

    sub make_where_block($expr) {
        # If it's already a block, nothing to do at all.
        if $expr<past_block> {
            return $expr<code_object>;
        }

        # Build a block that'll smartmatch the topic against the
        # expression.
        my $past := QAST::Block.new(
            QAST::Stmts.new(
                QAST::Var.new( :name('$_'), :scope('lexical'), :decl('var') )
            ),
            QAST::Stmts.new(
                QAST::Op.new(
                    :op('callmethod'), :name('ACCEPTS'),
                    $expr,
                    QAST::Var.new( :name('$_'), :scope('lexical') )
                )));
        ($*W.cur_lexpad())[0].push($past);

        # Give it a signature and create code object.
        my $param := hash(
            variable_name => '$_',
            nominal_type => $*W.find_symbol(['Mu']));
        my $sig := $*W.create_signature(nqp::hash('parameters', [$*W.create_parameter($param)]));
        add_signature_binding_code($past, $sig, [$param]);
        $*W.create_code_object($past, 'Block', $sig)
    }

    sub when_handler_helper($when_block) {
        unless nqp::existskey(%*HANDLERS, 'SUCCEED') {
            %*HANDLERS<SUCCEED> := QAST::Op.new(
                :op('p6return'),
                wrap_return_type_check(
                    QAST::Op.new(
                        :op('getpayload'),
                        QAST::Op.new( :op('exception') )
                    ),
                    $*DECLARAND) );
        }

        # if this is not an immediate block create a call
        if ($when_block<past_block>) {
            $when_block := QAST::Op.new( :op('call'), $when_block);
        }

        # call succeed with the block return value, succeed will throw
        # a BREAK exception to be caught by the above handler
        my $result := QAST::Op.new(
            :op('call'),
            :name('&succeed'),
            $when_block,
        );
        
        # wrap it in a handle op so that we can use a PROCEED exception
        # to skip the succeed call
        QAST::Op.new(
            :op('handle'),
            $result,
            'PROCEED',
            QAST::Op.new(
                :op('getpayload'),
                QAST::Op.new( :op('exception') )
            )
        )
    }

    sub make_dot_equals($target, $call) {
        $call.unshift($*W.add_string_constant($call.name));
        $call.unshift($target);
        $call.name('dispatch:<.=>');
        $call.op('callmethod');
        $call;
    }

    # XXX This isn't quite right yet... need to evaluate these semantics
    sub set_block_handler($/, $handler, $type) {
        # Handler needs its own $/ and $!.
        $*W.install_lexical_magical($handler<past_block>, '$!');
        $*W.install_lexical_magical($handler<past_block>, '$/');
        
        # unshift handler preamble: create exception object and store it into $_
        my $exceptionreg := $handler.unique('exception_');
        $handler<past_block>[0].unshift(QAST::Var.new(
            :scope('local'), :name($exceptionreg), :decl('param')
        ));
        $handler<past_block>[0].push(QAST::Stmts.new(
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :scope('lexical'), :name('$_') ),
                QAST::Op.new(
                    :op('call'), :name('&EXCEPTION'),
                    QAST::Var.new( :scope('local'), :name($exceptionreg) )
                )
            ),
            QAST::Op.new( :op('p6store'),
                QAST::Op.new( :op('getlexouter'), QAST::SVal.new( :value('$!') ) ),
                QAST::Var.new( :scope('lexical'), :name('$_') ),
            )
        ));
        
        # If the handler has a succeed handler, then make sure we sink
        # the exception it will produce.
        if $handler<past_block><handlers> && nqp::existskey($handler<past_block><handlers>, 'SUCCEED') {
            my $suc := $handler<past_block><handlers><SUCCEED>;
            $suc[0] := QAST::Stmts.new(
                sink(QAST::Op.new(
                    :op('getpayload'),
                    QAST::Op.new( :op('exception') )
                )),
                QAST::WVal.new( :value($*W.find_symbol(['Nil'])) )
            );
        }

        # set up a generic exception rethrow, so that exception
        # handlers from unwanted frames will get skipped if the
        # code in our handler throws an exception.
        my $ex := QAST::Op.new( :op('exception') );
        if $handler<past_block><handlers> && nqp::existskey($handler<past_block><handlers>, $type) {
            $ex := QAST::VM.new(
                :parrot(QAST::VM.new(
                    :pirop('perl6_skip_handlers_in_rethrow__0Pi'),
                    $ex,
                    QAST::IVal.new( :value(1) ))),
                :jvm($ex),
                :moar($ex));
        }
        else {
            my $prev_content := QAST::Stmts.new();
            $prev_content.push($handler<past_block>.shift()) while +@($handler<past_block>);
            $prev_content.push(QAST::WVal.new( :value($*W.find_symbol(['Nil'])) ));
            $handler<past_block>.push(QAST::Op.new(
                :op('handle'),
                $prev_content,
                'CATCH',
                QAST::VM.new(
                    :parrot(QAST::VM.new(
                        :pirop('perl6_based_rethrow 1PP'),
                        QAST::Op.new( :op('exception') ),
                        QAST::Var.new( :name($exceptionreg), :scope('local') ),
                    )),
                    :jvm(QAST::Op.new(
                        :op('rethrow'),
                        QAST::Op.new( :op('exception') )
                    )),
                    :moar(QAST::Op.new(
                        :op('rethrow'),
                        QAST::Op.new( :op('exception') )
                    )))));
                
            # rethrow the exception if we reach the end of the handler
            # (if a when {} clause matches this will get skipped due
            # to the BREAK exception)
            $handler<past_block>.push(QAST::Op.new(
                :op('rethrow'),
                QAST::Var.new( :name($exceptionreg), :scope('local') )));
        }

        # Install the handler, taking care over scoping.
        my $handler_lex_name := $handler.unique('__HANDLER_');
        $*W.cur_lexpad()[0].push(QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name($handler_lex_name), :scope('lexical'), :decl('var') ),
            block_closure($handler)
        ));
        %*HANDLERS{$type} := QAST::Stmts.new(
            :node($/),
            QAST::Op.new(
                :op('p6invokehandler'),
                QAST::Var.new( :name($handler_lex_name), :scope('lexical') ),
                $ex
            ),
            QAST::WVal.new( :value($*W.find_symbol(['Nil'])) )
        );
    }

    # Handles the case where we have a default value closure for an
    # attribute.
    method install_attr_init($/, $attr, $initializer, $block) {
        # Construct signature and anonymous method.
        my @params := [
            hash( is_invocant => 1, nominal_type => $*PACKAGE),
            hash( variable_name => '$_', nominal_type => $*W.find_symbol(['Mu']))
        ];
        my $sig := $*W.create_signature(nqp::hash('parameters', [
            $*W.create_parameter(@params[0]),
            $*W.create_parameter(@params[1])
        ]));
        $block[0].push(QAST::Var.new( :name('self'), :scope('lexical'), :decl('var') ));
        $block[0].push(QAST::Var.new( :name('$_'), :scope('lexical'), :decl('var') ));
        $block.push(QAST::Stmts.new( $initializer ));
        $block.symbol('self', :scope('lexical'));
        add_signature_binding_code($block, $sig, @params);
        $block.blocktype('declaration_static');
        my $code := $*W.create_code_object($block, 'Method', $sig);

        # Block should go in current lexpad, in correct lexical context.
        ($*W.cur_lexpad())[0].push($block);

        # Dispatch trait. XXX Should really be Bool::True, not Int here...
        my $true := ($*W.add_constant('Int', 'int', 1)).compile_time_value;
        $*W.apply_trait($/, '&trait_mod:<will>', $attr, :build($code));
    }

    # This is the hook where, in the future, we'll use this as the hook to check
    # if we have a proto or other declaration in scope that states that this sub
    # has a signature of the form :(\|$parcel), in which case we don't promote
    # the Parcel to a Capture when calling it. For now, we just worry about the
    # special case, return.
    sub capture_or_parcel($args, $name) {
        if $name eq 'return' {
            # Need to demote pairs again.
            my $parcel := QAST::Op.new( :op('call') );
            for @($args) {
                $parcel.push($_<before_promotion> ?? $_<before_promotion> !! $_);
            }
            $parcel
        }
        else {
            $args
        }
    }

    # This checks if we have something of the form * op *, * op <thing> or
    # <thing> op * and if so, and if it's not one of the ops we do not
    # auto-curry for, emits a closure instead. We hard-code the things not
    # to curry for now; in the future, we will inspect the multi signatures
    # of the op to decide, or likely store things in this hash from that
    # introspection and keep it as a quick cache.

    # %curried == 0 means do not curry
    # %curried == 1 means curry WhateverCode only
    # %curried == 2 means curry both WhateverCode and Whatever (default)

    my %curried;
    INIT {
        %curried{'&infix:<...>'}  := 0;
        %curried{'&infix:<...^>'} := 0;
        %curried{'&infix:<~~>'}   := 0;
        %curried{'&infix:<=>'}    := 0;
        %curried{'&infix:<:=>'}   := 0;
        %curried{'&infix:<..>'}   := 1;
        %curried{'&infix:<..^>'}  := 1;
        %curried{'&infix:<^..>'}  := 1;
        %curried{'&infix:<^..^>'} := 1;
        %curried{'&infix:<xx>'}   := 1;
        %curried{'callmethod'}    := 2;
        %curried{'&postcircumfix:<[ ]>'} := 2;
        %curried{'&postcircumfix:<{ }>'} := 2;
    }
    sub whatever_curry($/, $past, $upto_arity) {
        my $Whatever := $*W.find_symbol(['Whatever']);
        my $WhateverCode := $*W.find_symbol(['WhateverCode']);
        my $curried :=
            # It must be an op and...
            nqp::istype($past, QAST::Op) && (

            # Either a call that we're allowed to curry...
                (($past.op eq 'call' || $past.op eq 'chain') &&
                    (nqp::index($past.name, '&infix:') == 0 ||
                     nqp::index($past.name, '&prefix:') == 0 ||
                     nqp::index($past.name, '&postfix:') == 0 ||
                     (nqp::istype($past[0], QAST::Op) &&
                        nqp::index($past[0].name, '&METAOP') == 0)) &&
                    %curried{$past.name} // 2)

            # Or not a call and an op in the list of alloweds.
                || ($past.op ne 'call' && %curried{$past.op} // 0)

            # or one of our new postcircumfix subs that used to be methods
                || ($past.op eq 'call' && nqp::index($past.name, '&postcircumfix:') == 0 &&
                    %curried{$past.name} // 0)
            );
        my int $i := 0;
        my int $whatevers := 0;
        while $curried && $i < $upto_arity {
            my $check := $past[$i];
            $check := $check[0] if (nqp::istype($check, QAST::Stmts) || 
                                    nqp::istype($check, QAST::Stmt)) &&
                                   +@($check) == 1;
            $whatevers++ if istype($check.returns, $WhateverCode)
                            || $curried > 1 && istype($check.returns, $Whatever);
            $i++;
        }
        if $whatevers {
            my $was_chain := $past.op eq 'chain' ?? $past.name !! NQPMu;
            my int $i := 0;
            my @params;
            my @old_args;
            my $block := QAST::Block.new(QAST::Stmts.new(), $past);
            $*W.cur_lexpad()[0].push($block);
            while $i < $upto_arity {
                my $old := $past[$i];
                $old := $old[0] if (nqp::istype($old, QAST::Stmts) ||
                                    nqp::istype($old, QAST::Stmt)) &&
                                   +@($old) == 1;
                if istype($old.returns, $WhateverCode) {
                    my $new;
                    if $was_chain && nqp::existskey($old, "chain_args") {
                        $new := QAST::Op.new( :op<chain>, :name($old<chain_name>), :node($/) );
                        $old<chain_block>[1] := QAST::Op.new( :op<die>, QAST::SVal.new( :value('This WhateverCode has been inlined into another WhateverCode and should not have been called!') ) );
                        for $old<chain_past> {
                            $new.push($_);
                        }
                        for $old<chain_args> -> %arg {
                            @params.push(%arg);
                            $block[0].push(QAST::Var.new(:name(%arg<variable_name>), :scope<lexical>, :decl<var>));
                        }
                        nqp::push(@old_args, $new);
                    } else {
                        # Have to move the nested thunk inside this one, to get the
                        # correct lexical scoping.
                        my $old_ast := $old<past_block>;
                        remove_block($*W.cur_lexpad(), $old_ast);
                        $block[0].push($old_ast);
                        $new := QAST::Op.new( :op<call>, :node($/), $old );
                        my $acount := 0;
                        while $acount < $old.arity {
                            my $pname := $*W.cur_lexpad()[0].unique('$whatevercode_arg');
                            @params.push(hash(
                                :variable_name($pname),
                                :nominal_type($*W.find_symbol(['Mu'])),
                                :is_parcel(1),
                            ));
                            $block[0].push(QAST::Var.new(:name($pname), :scope<lexical>, :decl<var>));
                            my $to_push := QAST::Var.new(:name($pname), :scope<lexical>);
                            $new.push($to_push);
                            nqp::push(@old_args, $to_push) if $was_chain;
                            $acount++;
                        }
                    }
                    $past[$i] := $new;
                }
                elsif $curried > 1 && istype($old.returns, $Whatever) {
                    my $pname := $*W.cur_lexpad()[0].unique('$whatevercode_arg');
                    @params.push(hash(
                        :variable_name($pname),
                        :nominal_type($*W.find_symbol(['Mu'])),
                        :is_parcel(1),
                    ));
                    $block[0].push(QAST::Var.new(:name($pname), :scope<lexical>, :decl('var')));
                    $past[$i] := QAST::Var.new(:name($pname), :scope<lexical>);
                    nqp::push(@old_args, $past[$i]) if $was_chain;
                } else {
                    nqp::push(@old_args, $past[$i]) if $was_chain;
                }
                $i++;
            }
            my %sig_info := hash(parameters => @params);
            my $signature := create_signature_object($/, %sig_info, $block);
            add_signature_binding_code($block, $signature, @params);
            my $code := $*W.create_code_object($block, 'WhateverCode', $signature);
            $past := block_closure(reference_to_code_object($code, $block));
            $past.returns($WhateverCode);
            $past.arity(+@params);
            if $was_chain {
                $past<chain_past> := @old_args;
                $past<chain_args> := @params;
                $past<chain_name> := $was_chain;
                $past<chain_block> := $block;
            }
        }
        $past
    }
    sub remove_block($from, $block) {
        # Remove the QAST::Block $block from $from[0]; die if not found.
        my @decls := $from[0].list;
        my int $i := 0;
        my int $n := nqp::elems(@decls);
        while $i < $n {
            if @decls[$i] =:= $block {
                @decls[$i] := QAST::Op.new( :op('null') );
                return 1;
            }
            $i++;
        }
        nqp::die('Internal error: failed to remove block');
    }

    sub wrap_return_type_check($wrappee, $code_obj) {
        QAST::Op.new(
            :op('p6typecheckrv'),
            $wrappee,
            QAST::WVal.new( :value($code_obj) ));
    }

    sub wrap_return_handler($past) {
        wrap_return_type_check(
            QAST::Stmts.new(
                :resultchild(0),
                QAST::Op.new(
                    :op<lexotic>, :name<RETURN>,
                    # If we fall off the bottom, decontainerize if
                    # rw not set.
                    QAST::Op.new( :op('p6decontrv'), QAST::WVal.new( :value($*DECLARAND) ), $past )
                ),
                QAST::Op.new(
                    :op<bind>,
                    QAST::Var.new(:name<RETURN>, :scope<lexical>),
                    QAST::Var.new(:name<&EXHAUST>, :scope<lexical>))
            ),
            $*DECLARAND
        )
    }

    # Works out how to look up a type. If it's not generic we statically
    # resolve it. Otherwise, we punt to a runtime lexical lookup.
    sub instantiated_type(@name, $/) {
        my $type := $*W.find_symbol(@name);
        my $is_generic := 0;
        try { $is_generic := $type.HOW.archetypes.generic }
        my $past;
        if $is_generic {
            $past := $*W.symbol_lookup(@name, $/);
            $past.set_compile_time_value($type);
        }
        else {
            $past := QAST::WVal.new( :value($type) );
        }
        $past.returns($type.WHAT);
        $past
    }

    # Ensures that the given PAST node has a value known at compile
    # time and if so obtains it. Otherwise reports an error, involving
    # the $usage parameter to make it more helpful.
    sub compile_time_value_str($past, $usage, $/) {
        if $past.has_compile_time_value {
            nqp::unbox_s($past.compile_time_value);
        }
        else {
            $*W.throw($/, ['X', 'Value', 'Dynamic'], what => $usage);
        }
    }
    
    sub istype($val, $type) {
        try { return nqp::istype($val, $type) }
        0
    }

    sub strip_trailing_zeros(str $n) {
        return $n if nqp::index($n, '.') < 0;
        while nqp::index('_0',nqp::substr($n, -1)) >= 0 {
            $n := nqp::substr($n, 0, nqp::chars($n) - 1);
        }
        $n;
    }

    # radix, $base, $exponent: parrot numbers (Integer or Float)
    # $number: parrot string
    # return value: PAST for Int, Rat or Num
    sub radcalc($/, $radix, $number, $base?, $exponent?, :$num) {
        my int $sign := 1;
        $*W.throw($/, 'X::Syntax::Number::RadixOutOfRange', :$radix)
            if $radix < 2 || $radix > 36;
        nqp::die("You gave us a base for the magnitude, but you forgot the exponent.")
            if nqp::defined($base) && !nqp::defined($exponent);
        nqp::die("You gave us an exponent for the magnitude, but you forgot the base.")
            if !nqp::defined($base) && nqp::defined($exponent);

        if nqp::substr($number, 0, 1) eq '-' {
            $sign := -1;
            $number := nqp::substr($number, 1);
        }
        if nqp::substr($number, 0, 1) eq '0' {
            my $radix_name := nqp::uc(nqp::substr($number, 1, 1));
            if nqp::index('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ', $radix_name) > $radix {
                $number := nqp::substr($number, 2);

                if      $radix_name eq 'B' {
                    $radix := 2;
                } elsif $radix_name eq 'O' {
                    $radix := 8;
                } elsif $radix_name eq 'D' {
                    $radix := 10;
                } elsif $radix_name eq 'X' {
                    $radix := 16;
                } else {
                    nqp::die("Unkonwn radix character '$radix_name' (can be b, o, d, x)");
                }
            }
        }

        $number := strip_trailing_zeros(~$number);

        my $Int := $*W.find_symbol(['Int']);

        my $iresult      := nqp::box_i(0, $Int);
        my $fdivide      := nqp::box_i(1, $Int);
        my $radixInt     := nqp::box_i($radix, $Int);
        my int $idx      := -1;
        my int $seen_dot := 0;
        while $idx < nqp::chars($number) - 1 {
            $idx++;
            my $current := nqp::uc(nqp::substr($number, $idx, 1));
            next if $current eq '_';
            if $current eq '.' {
                $seen_dot := 1;
                next;
            }
            my $i := nqp::index('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ', $current);
            nqp::die("Invalid character '$current' in number literal") if $i < 0 || $i >= $radix;
            $iresult := nqp::add_I(nqp::mul_I($iresult, $radixInt, $Int), nqp::box_i($i, $Int), $Int);
            $fdivide := nqp::mul_I($fdivide, $radixInt, $Int) if $seen_dot;
        }

        $iresult := nqp::mul_I($iresult, nqp::box_i($sign, $Int), $Int);

        if $num {
            if nqp::bool_I($iresult) {
                my num $result := nqp::mul_n(nqp::div_n(nqp::tonum_I($iresult), nqp::tonum_I($fdivide)), nqp::pow_n($base, $exponent));
                return $*W.add_numeric_constant($/, 'Num', $result);
            } else {
                return $*W.add_numeric_constant($/, 'Num', 0e0);
            }
        } else {
            if nqp::defined($exponent) {
                $iresult := nqp::mul_I(
                            $iresult,
                            nqp::pow_I(
                                nqp::box_i($base,     $iresult),
                                nqp::box_i($exponent, $iresult),
                                $*W.find_symbol(['Num']),
                                $Int,
                           ),
                           $Int,
                        );
            }
            if $seen_dot {
                # add_constant special-cases Rat, so there is
                # no need to add $iresult and $fdivide first
                return $*W.add_constant('Rat', 'type_new',
                    $iresult, $fdivide, :nocache(1)
                );
            } else {
                return $*W.add_numeric_constant($/, 'Int', $iresult);
            }
        }
    }
}

class Perl6::QActions is HLL::Actions does STDActions {
    method nibbler($/) {
        my @asts;
        my $lastlit := '';
        
        for @*nibbles {
            if nqp::istype($_, NQPMatch) {
                if nqp::istype($_.ast, QAST::Node) {
                    if $lastlit ne '' {
                        @asts.push($*W.add_string_constant($lastlit));
                        $lastlit := '';
                    }
                    @asts.push($_.ast<ww_atom>
                        ?? $_.ast
                        !! QAST::Op.new( :op('callmethod'), :name('Stringy'),  $_.ast ));
                }
                else {
                    $lastlit := $lastlit ~ $_.ast;
                }
            }
            else {
                $lastlit := $lastlit ~ $_;
            }
        }
        if $lastlit ne '' || !@asts {
            @asts.push($*W.add_string_constant($lastlit));
        }
        
        my $past := @asts.shift();
        for @asts {
            $past := QAST::Op.new( :op('call'), :name('&infix:<~>'), $past, $_ );
        }
        
        if nqp::can($/.CURSOR, 'postprocessor') {
            my $pp := $/.CURSOR.postprocessor;
            $past := self."postprocess_$pp"($/, $past);
        }
        
        $past.node($/);
        make $past;
    }
    
    method postprocess_null($/, $past) {
        $past
    }
    
    method postprocess_run($/, $past) {
        QAST::Op.new( :name('&QX'), :op('call'), :node($/), $past )
    }
    
    method postprocess_words($/, $past) {
        if $past.has_compile_time_value {
            my @words := HLL::Grammar::split_words($/,
                nqp::unbox_s($past.compile_time_value));
            if +@words != 1 {
                $past := QAST::Op.new( :op('call'), :name('&infix:<,>'), :node($/) );
                for @words { $past.push($*W.add_string_constant(~$_)); }
                $past := QAST::Stmts.new($past);
            }
            else {
                $past := $*W.add_string_constant(~@words[0]);
            }
        }
        else {
            $past := QAST::Op.new( :op('callmethod'), :name('words'), :node($/), $past );
        }
        $past
    }
    
    method postprocess_quotewords($/, $past) {
        my $result := QAST::Op.new( :op('call'), :name('&infix:<,>'), :node($/) );
        sub walk($node) {
            if $node<ww_atom> {
                $result.push($node);
            }
            elsif nqp::istype($node, QAST::Op) && $node.name eq '&infix:<~>' {
                walk($node[0]);
                walk($node[1]);
            }
            else {
                my $ppw := self.postprocess_words($/, $node);
                unless nqp::istype($ppw, QAST::Stmts) && +@($ppw[0]) == 0 {
                    $result.push($ppw);
                }
            }
        }
        walk($past);
        +@($result) == 1 ?? $result[0] !! $result
    }

    method postprocess_heredoc($/, $past) {
        QAST::Stmts.new(
            QAST::Op.new( :op<die_s>, QAST::SVal.new( :value("Premature heredoc consumption") ) ),
            $past)
    }

    method escape:sym<\\>($/) { make $<item>.ast; }
    method backslash:sym<qq>($/) { make $<quote>.ast; }
    method backslash:sym<\\>($/) { make $<text>.Str; }
    method backslash:delim ($/) { make $<text>.Str; }
    method backslash:sym<miscq>($/) { make '\\' ~ ~$/; }
    method backslash:sym<misc>($/) { make ~$/; }
    
    method backslash:sym<a>($/) { make nqp::chr(7) }
    method backslash:sym<b>($/) { make "\b" }
    method backslash:sym<c>($/) { make $<charspec>.ast }
    method backslash:sym<e>($/) { make "\c[27]" }
    method backslash:sym<f>($/) { make "\c[12]" }
    method backslash:sym<n>($/) { make "\n" }
    method backslash:sym<o>($/) { make self.ints_to_string( $<octint> ?? $<octint> !! $<octints><octint> ) }
    method backslash:sym<r>($/) { make "\r" }
    method backslash:sym<t>($/) { make "\t" }
    method backslash:sym<x>($/) { make self.ints_to_string( $<hexint> ?? $<hexint> !! $<hexints><hexint> ) }
    method backslash:sym<0>($/) { make "\c[0]" }

    method escape:sym<{ }>($/) {
        make QAST::Op.new(
            :op('callmethod'), :name('Stringy'),
            QAST::Op.new(
                :op('call'),
                QAST::Op.new( :op('p6capturelex'), $<block>.ast ),
                :node($/)));
    }
    
    method escape:sym<$>($/) { make $<EXPR>.ast; }
    method escape:sym<@>($/) { make $<EXPR>.ast; }
    method escape:sym<%>($/) { make $<EXPR>.ast; }
    method escape:sym<&>($/) { make $<EXPR>.ast; }

    method escape:sym<' '>($/) { make mark_ww_atom($<quote>.ast); }
    method escape:sym<" ">($/) { make mark_ww_atom($<quote>.ast); }
    method escape:sym<colonpair>($/) { make mark_ww_atom($<colonpair>.ast); }
    sub mark_ww_atom($ast) {
        $ast<ww_atom> := 1;
        $ast;
    }
}

class Perl6::RegexActions is QRegex::P6Regex::Actions does STDActions {

    method metachar:sym<:my>($/) {
        my $past := $<statement>.ast;
        make QAST::Regex.new( $past, :rxtype('qastnode'), :subtype('declarative') );
    }

    method metachar:sym<{ }>($/) {
        make QAST::Regex.new( $<codeblock>.ast,
                              :rxtype<qastnode>, :node($/) );
    }
    
    method metachar:sym<qw>($/) {
        my $qast := QAST::Regex.new( :rxtype<alt>, :node($/) );
        my $nib  := $<nibble>.ast[0];
        my @nibs := +@($nib) ?? @($nib) !! [$nib];
        for @nibs {
            unless $_.has_compile_time_value {
                $/.CURSOR.panic("Quote words construct too complex to use in a regex");
            }
            $qast.push(%*RX<i>
                ?? QAST::Regex.new( $_.compile_time_value, :rxtype<literal>, :subtype<ignorecase> )
                !! QAST::Regex.new( $_.compile_time_value, :rxtype<literal> ));
        }
        make $qast;
    }
    
    method metachar:sym<'>($/) { self.rxquote($/) }
    method metachar:sym<">($/) { self.rxquote($/) }
    method rxquote($/) {
        my $quote := $<quote>.ast;
        if $quote.has_compile_time_value {
            my $qast := QAST::Regex.new( :rxtype<literal>, nqp::unbox_s($quote.compile_time_value) );
            $qast.subtype('ignorecase') if %*RX<i>;
            make $qast;
        }
        else {
            make QAST::Regex.new( QAST::Node.new(
                                        QAST::SVal.new( :value('!LITERAL') ),
                                        $quote,
                                        QAST::IVal.new( :value(%*RX<i> ?? 1 !! 0) ) ),
                                :rxtype<subrule>, :subtype<method>, :node($/));
        }
    }
    
    method metachar:sym<rakvar>($/) {
        make QAST::Regex.new( QAST::Node.new(
                                    QAST::SVal.new( :value('INTERPOLATE') ),
                                    $<var>.ast,
                                    QAST::IVal.new( :value(%*RX<i> ?? 1 !! 0) ),
                                    QAST::IVal.new( :value($*SEQ ?? 1 !! 0) ) ),
                              :rxtype<subrule>, :subtype<method>, :node($/));
    }

    method assertion:sym<{ }>($/) {
        make QAST::Regex.new( 
                 QAST::Node.new(
                    QAST::SVal.new( :value('INTERPOLATE') ),
                    $<codeblock>.ast,
                    QAST::IVal.new( :value(%*RX<i> ?? 1 !! 0) ),
                    QAST::IVal.new( :value($*SEQ ?? 1 !! 0) ),
                    QAST::IVal.new( :value(1) ) ),
                 :rxtype<subrule>, :subtype<method>, :node($/));
    }

    method assertion:sym<?{ }>($/) {
        make QAST::Regex.new( $<codeblock>.ast,
                              :subtype<zerowidth>, :negate( $<zw> eq '!' ),
                              :rxtype<qastnode>, :node($/) );
    }

    method assertion:sym<var>($/) {
        make QAST::Regex.new( QAST::Node.new(
                                    QAST::SVal.new( :value('INTERPOLATE') ),
                                    $<var>.ast,
                                    QAST::IVal.new( :value(%*RX<i> ?? 1 !! 0) ),
                                    QAST::IVal.new( :value($*SEQ ?? 1 !! 0) ),
                                    QAST::IVal.new( :value(1) ) ),
                              :rxtype<subrule>, :subtype<method>, :node($/));
    }
    
    method assertion:sym<name>($/) {
        my @parts := $*W.dissect_longname($<longname>).components();
        my $name  := @parts.pop();
        my $qast;
        if $<assertion> {
            if +@parts {
                $/.CURSOR.panic("Can only alias to a short name (without '::')");
            }
            $qast := $<assertion>.ast;
            if $qast.rxtype eq 'subrule' {
                self.subrule_alias($qast, $name);
            }
            else {
                $qast := QAST::Regex.new( $qast, :name($name), 
                                          :rxtype<subcapture>, :node($/) );
            }
        }
        elsif !@parts && $name eq 'sym' {
            my str $fullrxname := %*RX<name>;
            my int $loc := nqp::index($fullrxname, ':sym<');
            $loc := nqp::index($fullrxname, ':sym«')
                if $loc < 0;
            my str $rxname := nqp::substr($fullrxname, $loc + 5, nqp::chars($fullrxname) - $loc - 6);
            $qast := QAST::Regex.new(:name('sym'), :rxtype<subcapture>, :node($/),
                QAST::Regex.new(:rxtype<literal>, $rxname, :node($/)));
        }
        else {
            if +@parts {
                my $gref := QAST::WVal.new( :value($*W.find_symbol(@parts)) );
                $qast := QAST::Regex.new(:rxtype<subrule>, :subtype<capture>,
                                         :node($/), QAST::Node.new(
                                            QAST::SVal.new( :value('OTHERGRAMMAR') ), 
                                            $gref, QAST::SVal.new( :value($name) )),
                                         :name(~$<longname>) );
            } elsif $*W.regex_in_scope('&' ~ $name) {
                $qast := QAST::Regex.new(:rxtype<subrule>, :subtype<capture>,
                                         :node($/), QAST::Node.new(
                                            QAST::SVal.new( :value('INDRULE') ),
                                            QAST::Var.new( :name('&' ~ $name), :scope('lexical') ) ), 
                                         :name($name) );
            }
            else {
                $qast := QAST::Regex.new(:rxtype<subrule>, :subtype<capture>,
                                         :node($/), QAST::Node.new(QAST::SVal.new( :value($name) )), 
                                         :name($name) );
            }
            if $<arglist> {
                for $<arglist>.ast.list { $qast[0].push($_) }
            }
            elsif $<nibbler> {
                my $nibbled := $name eq 'after'
                    ?? self.flip_ast($<nibbler>.ast)
                    !! $<nibbler>.ast;
                my $sub := %*LANG<Regex-actions>.qbuildsub($nibbled, :anon(1), :addself(1));
                $qast[0].push($sub);
            }
        }
        make $qast;
    }
    
    method assertion:sym<~~>($/) {
        if $<num> {
            $/.CURSOR.panic('Sorry, ~~ regex assertion with a capture is not yet implemented');
        }
        elsif $<desigilname> {
            $/.CURSOR.panic('Sorry, ~~ regex assertion with a capture is not yet implemented');
        }
        else {
            make QAST::Regex.new( :rxtype<subrule>, :subtype<method>,
                QAST::Node.new(QAST::SVal.new( :value('RECURSE') )), :node($/) );
        }
    }
    
    method codeblock($/) {
        my $blockref := $<block>.ast;
        my $past :=
            QAST::Stmts.new(
                QAST::Op.new(
                    :op('p6store'),
                    QAST::Var.new( :name('$/'), :scope<lexical> ),
                    QAST::Op.new(
                        QAST::Var.new( :name('$¢'), :scope<lexical> ),
                        :name('MATCH'),
                        :op('callmethod')
                    )
                ),
                QAST::Op.new( :op<call>, QAST::Op.new( :op('p6capturelex'), $blockref ) )
            );
        make $past;
    }

    method arglist($/) {
        my $arglist := $<arglist>.ast;
        make $arglist;
    }

    method create_regex_code_object($block) {
        $*W.create_code_object($block, 'Regex',
            $*W.create_signature(nqp::hash('parameters', [])))
    }

    method store_regex_nfa($code_obj, $block, $nfa) {
        $code_obj.SET_NFA($nfa.save);
    }
}

class Perl6::P5RegexActions is QRegex::P5Regex::Actions does STDActions {
    method create_regex_code_object($block) {
        $*W.create_code_object($block, 'Regex',
            $*W.create_signature(nqp::hash('parameters', [])))
    }

    method p5metachar:sym<(?{ })>($/) {
        make QAST::Regex.new( $<codeblock>.ast,
                              :rxtype<qastnode>, :node($/) );
    }

    method p5metachar:sym<(??{ })>($/) {
        make QAST::Regex.new( 
                 QAST::Node.new(
                    QAST::SVal.new( :value('INTERPOLATE') ),
                    $<codeblock>.ast,
                    QAST::IVal.new( :value(%*RX<i> ?? 1 !! 0) ),
                    QAST::IVal.new( :value(1) ),
                    QAST::IVal.new( :value(1) ) ),
                 :rxtype<subrule>, :subtype<method>, :node($/));
    }

    method p5metachar:sym<var>($/) {
        make QAST::Regex.new( QAST::Node.new(
                                    QAST::SVal.new( :value('INTERPOLATE') ),
                                    $<var>.ast,
                                    QAST::IVal.new( :value(%*RX<i> ?? 1 !! 0) ),
                                    QAST::IVal.new( :value($*SEQ ?? 1 !! 0) ),
                                    QAST::IVal.new( :value($*INTERPOLATION) ) ),
                              :rxtype<subrule>, :subtype<method>, :node($/));
    }

    method codeblock($/) {
        my $blockref := $<block>.ast;
        my $past :=
            QAST::Stmts.new(
                QAST::Op.new(
                    :op('p6store'),
                    QAST::Var.new( :name('$/'), :scope<lexical> ),
                    QAST::Op.new(
                        QAST::Var.new( :name('$¢'), :scope<lexical> ),
                        :name('MATCH'),
                        :op('callmethod')
                    )
                ),
                QAST::Op.new(:op<call>, $blockref)
            );
        make $past;
    }

    method store_regex_nfa($code_obj, $block, $nfa) {
        $code_obj.SET_NFA($nfa.save);
    }
}

# vim: ft=perl6
