# This is a list of all spec tests that are expected to pass.
#
# Empty lines and those beginning with a # are ignored
#
# We intend to include *all* tests from roast, even when we
# skip most of the tests or the entire test file. To verify
# we are running all tests, run:
#
# perl tools/update-passing-test-data.pl
#
# If a file appears in the output of the script, it is not run
# by default. It may need to be fudged in order to run successfully.
# Open an RT when necessary as part of the fudge process, using the
# RT in the fudge message, and then add the test file to this file, sorted.
#
# Each file may have one or more markers that deselects the test:
#     long   - run tests unless --quick
#     stress - run tests only if --stress
#     moar   - run tests only for MoarVM backend
# See the "make quicktest" and "make stresstest" targets in
# build/Makefile.in for examples of use.


S01-perl-5-integration/array.t              # perl5
S01-perl-5-integration/basic.t              # perl5
S01-perl-5-integration/class.t              # perl5
S01-perl-5-integration/exception_handling.t # perl5
S01-perl-5-integration/hash.t               # perl5
S01-perl-5-integration/import.t             # perl5
S01-perl-5-integration/method.t             # perl5
S01-perl-5-integration/return.t             # perl5
S01-perl-5-integration/roundtrip.t          # perl5
S01-perl-5-integration/strings.t            # perl5
S01-perl-5-integration/subs.t               # perl5
S02-lexical-conventions/begin_end_pod.t
S02-lexical-conventions/bom.t
S02-lexical-conventions/comments.t
S02-lexical-conventions/end-pod.t
S02-lexical-conventions/minimal-whitespace.t
S02-lexical-conventions/one-pass-parsing.t
S02-lexical-conventions/pod-in-multi-line-exprs.t
S02-lexical-conventions/sub-block-parsing.t
S02-lexical-conventions/unicode.t
S02-lexical-conventions/unicode-whitespace.t
S02-lexical-conventions/unspace.t
S02-lists/indexing.t
S02-lists/tree.t
S02-literals/adverbs.t
S02-literals/allomorphic.t
S02-literals/array-interpolation.t
S02-literals/autoref.t
S02-literals/char-by-name.t
S02-literals/char-by-number.t
S02-literals/fmt-interpolation.t
S02-literals/hash-interpolation.t
S02-literals/heredocs.t
S02-literals/hex_chars.t
S02-literals/listquote.t
S02-literals/listquote-whitespace.t
S02-literals/misc-interpolation.t
S02-literals/numeric.t
S02-literals/pair-boolean.t
S02-literals/pairs.t
S02-literals/pod.t
S02-literals/quoting.t
S02-literals/quoting-unicode.t
S02-literals/radix.t
S02-literals/string-interpolation.t
S02-literals/sub-calls.t
S02-literals/subscript.t
S02-literals/types.t
S02-literals/underscores.t
S02-literals/version.t
S02-magicals/78258.t
S02-magicals/args.t
S02-magicals/block.t
S02-magicals/DISTRO.t
S02-magicals/dollar_bang.t
S02-magicals/dollar-underscore.t
S02-magicals/env.t
S02-magicals/file_line.t
S02-magicals/KERNEL.t
S02-magicals/PERL.t
S02-magicals/pid.t
S02-magicals/progname.t
S02-magicals/sub.t
S02-magicals/subname.t
S02-magicals/VM.t
S02-names/bare-sigil.t
S02-names/caller.t
S02-names/identifier.t
S02-names/indirect.t
S02-names/is_cached.t
S02-names/is_default.t
S02-names/is_dynamic.t
S02-names/name.t
S02-names/our.t
S02-names/pseudo.t
S02-names/strict.t
S02-names/symbolic-deref.t
S02-names-vars/contextual.t
S02-names-vars/fmt.t
S02-names-vars/list_array_perl.t
S02-names-vars/names.t
S02-names-vars/perl.t
S02-names-vars/signature.t
S02-names-vars/variables-and-packages.t
S02-names-vars/varnames.t
S02-one-pass-parsing/less-than.t
S02-packages/package-lookup.t
S02-types/anon_block.t
S02-types/array_extending.t
S02-types/array_ref.t
S02-types/array-shapes.t
S02-types/array.t
S02-types/assigning-refs.t
S02-types/autovivification.t
S02-types/bag.t
S02-types/baghash.t
S02-types/bool.t
S02-types/built-in.t
S02-types/capture.t
S02-types/catch_type_cast_mismatch.t
S02-types/compact.t
S02-types/declare.t
S02-types/fatrat.t
S02-types/flattening.t
S02-types/hash_ref.t
S02-types/hash.t
S02-types/infinity.t
S02-types/instants-and-durations.t
S02-types/int-uint.t
S02-types/is-type.t
S02-types/isDEPRECATED.t
S02-types/keyhash.t
S02-types/lazy-lists.t
S02-types/lists.t
S02-types/mix.t
S02-types/mixhash.t
S02-types/mixed_multi_dimensional.t
S02-types/multi_dimensional_array.t
S02-types/nan.t
S02-types/native.t
S02-types/nested_arrays.t
S02-types/nested_pairs.t
S02-types/nil.t
S02-types/num.t
S02-types/pair.t
S02-types/list.t
S02-types/parsing-bool.t
S02-types/range.t
S02-types/resolved-in-setting.t    # moar
S02-types/set.t
S02-types/sethash.t
S02-types/sigils-and-types.t
S02-types/stash.t
S02-types/subscripts_and_context.t
S02-types/subset.t
S02-types/type.t
S02-types/undefined-types.t
S02-types/unicode.t
S02-types/version.t
S02-types/whatever.t
S02-types/WHICH.t
S03-binding/arrays.t
S03-binding/attributes.t
S03-binding/closure.t
S03-binding/hashes.t
S03-binding/nested.t
S03-binding/ro.t
S03-binding/scalars.t
S03-feeds/basic.t
S03-junctions/associative.t
S03-junctions/autothreading.t
S03-junctions/boolean-context.t
S03-junctions/misc.t
S03-metaops/cross.t
S03-metaops/eager-hyper.t
S03-metaops/hyper.t
S03-metaops/not.t
S03-metaops/reduce.t
S03-metaops/reverse.t
S03-metaops/zip.t
S03-operators/adverbial-modifiers.t
S03-operators/also.t
S03-operators/andthen.t
S03-operators/orelse.t
S03-operators/arith.t
S03-operators/assign-is-not-binding.t
S03-operators/assign.t
S03-operators/autoincrement-range.t
S03-operators/autoincrement.t
S03-operators/autovivification.t
S03-operators/bag.t
S03-operators/basic-types.t
S03-operators/bit.t
S03-operators/boolean-bitwise.t
S03-operators/brainos.t
S03-operators/composition.t
S03-operators/custom.t
S03-operators/buf.t
S03-operators/chained-declarators.t
S03-operators/cmp.t
S03-operators/comparison-simple.t
S03-operators/comparison.t
S03-operators/context-forcers.t
S03-operators/context.t
S03-operators/equality.t
S03-operators/eqv.t
S03-operators/flip-flop.t
S03-operators/gcd.t
S03-operators/div.t
S03-operators/identity.t
S03-operators/increment.t
S03-operators/infixed-function.t
S03-operators/inplace.t
S03-operators/is-divisible-by.t
S03-operators/lcm.t
S03-operators/list-quote-junction.t
S03-operators/minmax.t
S03-operators/misc.t
S03-operators/mix.t
S03-operators/names.t
S03-operators/nesting.t
S03-operators/not.t
S03-operators/numeric-shift.t
S03-operators/overflow.t
S03-operators/precedence.t
S03-operators/range-basic.t
S03-operators/range-int.t
S03-operators/range.t
S03-operators/reduce-le1arg.t
S03-operators/relational.t
S03-operators/repeat.t
S03-operators/scalar-assign.t
S03-operators/set.t
S03-operators/short-circuit.t
S03-operators/so.t
S03-operators/spaceship-and-containers.t
S03-operators/spaceship.t
S03-operators/subscript-adverbs.t
S03-operators/subscript-vs-lt.t
S03-operators/ternary.t
S03-operators/value_equivalence.t
S03-sequence/arity0.t
S03-sequence/arity-2-or-more.t
S03-sequence/basic.t
S03-sequence/limit-arity-2-or-more.t
S03-sequence/misc.t
S03-sequence/nonnumeric.t
S03-smartmatch/any-any.t
S03-smartmatch/any-bool.t
S03-smartmatch/any-callable.t
S03-smartmatch/any-complex.t
S03-smartmatch/any-hash-pair.t
S03-smartmatch/any-method.t
S03-smartmatch/any-num.t
S03-smartmatch/any-pair.t
S03-smartmatch/any-str.t
S03-smartmatch/any-sub.t
S03-smartmatch/any-type.t
S03-smartmatch/array-array.t
S03-smartmatch/array-hash.t
S03-smartmatch/capture-signature.t
S03-smartmatch/disorganized.t
S03-smartmatch/hash-hash.t
S03-smartmatch/range-range.t
S03-smartmatch/regex-hash.t
S03-smartmatch/scalar-hash.t
S03-smartmatch/signature-signature.t
S04-blocks-and-statements/let.t
S04-blocks-and-statements/pointy-rw.t
S04-blocks-and-statements/pointy.t
S04-blocks-and-statements/temp.t
S04-declarations/constant.t
S04-declarations/implicit-parameter.t
S04-declarations/multiple.t
S04-declarations/my.t
S04-declarations/our.t
S04-declarations/smiley.t
S04-declarations/state.t
S04-declarations/will.t
S04-exception-handlers/catch.t
S04-exception-handlers/control.t
S04-exception-handlers/top-level.t
S04-exceptions/control_across_runloop.t
S04-exceptions/fail.t
S04-exceptions/pending.t
S04-phasers/ascending-order.t
S04-phasers/begin.t
S04-phasers/check.t
S04-phasers/descending-order.t
S04-phasers/end.t
S04-phasers/enter-leave.t
S04-phasers/eval-in-begin.t
S04-phasers/first.t
S04-phasers/init.t
S04-phasers/in-eval.t
S04-phasers/in-loop.t
S04-phasers/keep-undo.t
S04-phasers/multiple.t
S04-phasers/next.t
S04-phasers/pre-post.t
S04-phasers/rvalue.t
S04-statement-modifiers/for.t
S04-statement-modifiers/given.t
S04-statement-modifiers/if.t
S04-statement-modifiers/unless.t
S04-statement-modifiers/until.t
S04-statement-modifiers/values_in_bool_context.t
S04-statement-modifiers/while.t
S04-statement-modifiers/with.t
S04-statement-modifiers/without.t
S04-statement-parsing/hash.t
S04-statements/do.t
S04-statements/for-scope.t
S04-statements/for.t
S04-statements/for_with_only_one_item.t
S04-statements/gather.t
S04-statements/given.t
S04-statements/if.t
S04-statements/label.t
S04-statements/last.t
S04-statements/loop.t
S04-statements/map-and-sort-in-for.t
S04-statements/next.t
S04-statements/no-implicit-block.t
S04-statements/once.t
S04-statements/quietly.t
S04-statements/redo.t
S04-statements/repeat.t
S04-statements/return.t
S04-statements/sink.t
S04-statements/terminator.t
S04-statements/try.t
S04-statements/unless.t
S04-statements/until.t
S04-statements/when.t
S04-statements/while.t
S04-statements/with.t
S05-capture/alias.t
S05-capture/array-alias.t
S05-capture/caps.t
S05-capture/dot.t
S05-capture/match-object.t
S05-capture/named.t
S05-capture/subrule.t
S05-grammar/action-stubs.t
S05-grammar/example.t
S05-grammar/inheritance.t
S05-grammar/methods.t
S05-grammar/namespace.t
S05-grammar/parse_and_parsefile.t
S05-grammar/polymorphism.t
S05-grammar/protoregex.t
S05-grammar/protos.t
S05-grammar/signatures.t
S05-grammar/ws.t
S05-interpolation/lexicals.t
S05-interpolation/regex-in-variable.t
S05-mass/charsets.t
S05-mass/named-chars.t
S05-mass/properties-block.t
S05-mass/properties-derived.t
S05-mass/properties-general.t
S05-mass/properties-script.t
S05-mass/recursive.t
S05-mass/rx.t
S05-mass/stdrules.t
S05-match/arrayhash.t
S05-match/blocks.t
S05-match/capturing-contexts.t
S05-match/make.t
S05-match/non-capturing.t
S05-match/perl.t
S05-match/positions.t
S05-metachars/closure.t
S05-metachars/line-anchors.t
S05-metachars/newline.t
S05-metachars/tilde.t
S05-metasyntax/angle-brackets.t
S05-metasyntax/assertions.t
S05-metasyntax/changed.t
S05-metasyntax/charset.t
S05-metasyntax/delimiters.t
S05-metasyntax/interpolating-closure.t
S05-metasyntax/litvar.t
S05-metasyntax/longest-alternative.t
S05-metasyntax/lookaround.t
S05-metasyntax/null.t
S05-metasyntax/proto-token-ltm.t
S05-metasyntax/regex.t
S05-metasyntax/repeat.t
S05-metasyntax/sequential-alternation.t
S05-metasyntax/single-quotes.t
S05-metasyntax/unknown.t
S05-metasyntax/unicode-property-pair.t
S05-modifier/continue.t
S05-modifier/counted-match.t
S05-modifier/counted.t
S05-modifier/global.t
S05-modifier/ignorecase.t
S05-modifier/ignorecase-and-ignoremark.t    # moar
S05-modifier/ignoremark.t                   # moar
S05-modifier/ii.t
S05-modifier/my.t
S05-modifier/overlapping.t
S05-modifier/perl5_0.t
S05-modifier/perl5_1.t
S05-modifier/perl5_2.t
S05-modifier/perl5_3.t
S05-modifier/perl5_4.t
S05-modifier/perl5_5.t
S05-modifier/perl5_6.t
S05-modifier/perl5_7.t
S05-modifier/perl5_8.t
S05-modifier/perl5_9.t
S05-modifier/pos.t
S05-modifier/repetition-exhaustive.t
S05-modifier/repetition.t
S05-modifier/sigspace.t
S05-substitution/67222.t
S05-substitution/match.t
S05-substitution/subst.t
S05-transliteration/79778.t
S05-transliteration/trans.t
S05-transliteration/with-closure.t
S06-advanced/callframe.t
S06-advanced/callsame.t
S06-advanced/lexical-subs.t
S06-advanced/recurse.t
S06-advanced/return.t
S06-advanced/stub.t
S06-advanced/wrap.t
S06-currying/assuming-and-mmd.t
S06-currying/misc.t
S06-currying/named.t
S06-currying/positional.t
S06-currying/slurpy.t
S06-macros/errors.t
S06-macros/quasi-blocks.t
S06-macros/unquoting.t
S06-macros/opaque-ast.t
S06-multi/by-trait.t
S06-multi/lexical-multis.t
S06-multi/positional-vs-named.t
S06-multi/proto.t
S06-multi/redispatch.t
S06-multi/subsignature.t
S06-multi/syntax.t
S06-multi/type-based.t
S06-multi/unpackability.t
S06-multi/value-based.t
S06-operator-overloading/imported-subs.t
S06-operator-overloading/methods.t
S06-operator-overloading/semicolon.t
S06-operator-overloading/sub.t
S06-operator-overloading/term.t
S06-operator-overloading/workout.t
S06-other/anon-hashes-vs-blocks.t
S06-other/introspection.t
S06-other/main-eval.t
S06-other/main.t
S06-other/main-usage.t
S06-other/main-semicolon.t
S06-other/misc.t
S06-other/pairs-as-lvalues.t
S06-parameters/smiley.t
S06-routine-modifiers/lvalue-subroutines.t
S06-routine-modifiers/native-lvalue-subroutines.t
S06-routine-modifiers/proxy.t
S06-routine-modifiers/scoped-named-subs.t
S06-signature/arity.t
S06-signature/caller-param.t
S06-signature/closure-over-parameters.t
S06-signature/closure-parameters.t
S06-signature/code.t
S06-signature/defaults.t
S06-signature/definite-return.t
S06-signature/errors.t
S06-signature/introspection.t
S06-signature/mixed-placeholders.t
S06-signature/multidimensional.t
S06-signature/multi-invocant.t
S06-signature/named-parameters.t
S06-signature/named-placeholders.t
S06-signature/named-renaming.t
S06-signature/optional.t
S06-signature/outside-subroutine.t
S06-signature/passing-arrays.t
S06-signature/passing-hashes.t
S06-signature/positional-placeholders.t
S06-signature/positional.t
S06-signature/scalar-type.t
S06-signature/shape.t
S06-signature/sigilless.t
S06-signature/slurpy-and-interpolation.t
S06-signature/slurpy-params.t
S06-signature/slurpy-placeholders.t
S06-signature/sub-ref.t
S06-signature/tree-node-parameters.t
S06-signature/type-capture.t
S06-signature/types.t
S06-signature/unpack-array.t
S06-signature/unpack-object.t
S06-signature/unspecified.t
S06-traits/as.t
S06-traits/is-assoc.t
S06-traits/is-copy.t
S06-traits/is-readonly.t
S06-traits/is-rw.t
S06-traits/misc.t
S06-traits/native-is-copy.t
S06-traits/native-is-rw.t
S06-traits/precedence.t
S07-slip/slip.t
S07-hyperrace/hyper.t
S07-hyperrace/race.t
S06-traits/slurpy-is-rw.t
S09-autovivification/autoincrement.t
S09-autovivification/autovivification.t
S09-hashes/objecthash.t
S09-subscript/slice.t
S09-subscript/multidim-assignment.t
S09-multidim/XX-POS-on-dimensioned.t
S09-multidim/XX-POS-on-undimensioned.t
S09-multidim/assign.t
S09-multidim/decl.t
S09-multidim/indexing.t
S09-multidim/methods.t
S09-multidim/subs.t
S09-typed-arrays/arrays.t
S09-typed-arrays/hashes.t
S09-typed-arrays/native.t
S09-typed-arrays/native-decl.t
S09-typed-arrays/native-int.t
S09-typed-arrays/native-num.t
S10-packages/basic.t
S10-packages/joined-namespaces.t
S10-packages/precompilation.t # slow
S10-packages/use-with-class.t
S11-compunit/compunit-dependencyspecification.t
S11-compunit/compunit-repository.t
S11-compunit/rt126904.t
S11-modules/export.t
S11-modules/importing.t
S11-modules/import-multi.t
S11-modules/import-tag.t
S11-modules/import.t
S11-modules/lexical.t
S11-modules/need.t
S11-modules/nested.t
S11-modules/require.t
S11-repository/curli-install.t
S11-repository/cur-candidates.t
S11-repository/cur-current-distribution.t
S12-attributes/class.t
S12-attributes/clone.t
S12-attributes/defaults.t
S12-attributes/delegation.t
S12-attributes/inheritance.t
S12-attributes/instance.t
S12-attributes/mutators.t
S12-attributes/native.t
S12-attributes/recursive.t
S12-attributes/smiley.t
S12-attributes/undeclared.t
S12-class/anonymous.t
S12-class/attributes.t
S12-class/attributes-required.t
S12-class/augment-supersede.t
S12-class/basic.t
S12-class/declaration-order.t
S12-class/extending-arrays.t
S12-class/inheritance-class-methods.t
S12-class/inheritance.t
S12-class/instantiate.t
S12-class/interface-consistency.t
S12-class/lexical.t
S12-class/literal.t
S12-class/magical-vars.t
S12-class/mro.t
S12-class/namespaced.t
S12-class/open.t
S12-class/parent_attributes.t
S12-class/rw.t
S12-class/self-inheritance.t
S12-class/stubs.t
S12-class/type-object.t
S12-coercion/coercion-types.t
S12-construction/autopairs.t
S12-construction/BUILD.t
S12-construction/construction.t
S12-construction/named-params-in-BUILD.t
S12-construction/new.t
S12-enums/anonymous.t
S12-enums/as-role.t
S12-enums/basic.t
S12-enums/misc.t
S12-enums/non-int.t
S12-enums/pseudo-functional.t
S12-enums/thorough.t
S12-introspection/attributes.t
S12-introspection/can.t
S12-introspection/definite.t
S12-introspection/meta-class.t
S12-introspection/methods.t
S12-introspection/parents.t
S12-introspection/roles.t
S12-introspection/walk.t
S12-introspection/WHAT.t
S12-meta/exporthow.t
S12-meta/grammarhow.t
S12-meta/primitives.t
S12-methods/accessors.t
S12-methods/attribute-params.t
S12-methods/calling_sets.t
S12-methods/calling_syntax.t
S12-methods/chaining.t
S12-methods/class-and-instance.t
S12-methods/delegation.t
S12-methods/default-trait.t
S12-methods/defer-call.t
S12-methods/defer-next.t
S12-methods/fallback.t
S12-methods/how.t
S12-methods/indirect_notation.t
S12-methods/instance.t
S12-methods/lastcall.t
S12-methods/lvalue.t
S12-methods/method-vs-sub.t
S12-methods/multi.t
S12-methods/parallel-dispatch.t
S12-methods/private.t
S12-methods/qualified.t
S12-methods/submethods.t
S12-methods/syntax.t
S12-methods/topic.t
S12-methods/trusts.t
S12-methods/typed-attributes.t
S12-methods/what.t
S12-subset/multi-dispatch.t
S12-subset/subtypes.t
S13-overloading/metaoperators.t
S13-overloading/operators.t
S13-overloading/typecasting-long.t
S13-type-casting/methods.t
S14-roles/anonymous.t
S14-roles/attributes.t
S14-roles/basic.t
S14-roles/bool.t
S14-roles/composition.t
S14-roles/conflicts.t
S14-roles/crony.t
S14-roles/instantiation.t
S14-roles/lexical.t
S14-roles/mixin.t
S14-roles/namespaced.t
S14-roles/parameterized-basic.t
S14-roles/parameterized-mixin.t
S14-roles/parameterized-type.t
S14-roles/parameter-subtyping.t
S14-roles/stubs.t
S14-roles/submethods.t
S14-traits/attributes.t
S14-traits/routines.t
S15-literals/identifiers.t
S15-literals/numbers.t
S15-nfg/case-change.t           # moar
S15-nfg/cgj.t                   # moar
S15-nfg/concatenation.t         # moar
S15-nfg/crlf-encoding.t         # moar
S15-nfg/from-buf.t              # moar
S15-nfg/from-file.t             # moar
S15-nfg/grapheme-break.t        # moar
S15-nfg/long-uni.t              # moar
S15-nfg/mass-chars.t            # moar
S15-nfg/many-combiners.t        # moar
S15-nfg/many-threads.t          # moar
S15-nfg/mass-equality.t         # moar
S15-nfg/mass-roundtrip-nfc.t    # moar
S15-nfg/mass-roundtrip-nfd.t    # moar
S15-nfg/mass-roundtrip-nfkc.t   # moar
S15-nfg/mass-roundtrip-nfkd.t   # moar
S15-nfg/regex.t                 # moar
S15-normalization/nfc-0.t       # moar stress
S15-normalization/nfc-1.t       # moar stress
S15-normalization/nfc-2.t       # moar stress
S15-normalization/nfc-3.t       # moar stress
S15-normalization/nfc-4.t       # moar stress
S15-normalization/nfc-5.t       # moar stress
S15-normalization/nfc-6.t       # moar stress
S15-normalization/nfc-7.t       # moar stress
S15-normalization/nfc-8.t       # moar stress
S15-normalization/nfc-9.t       # moar stress
S15-normalization/nfc-sanity.t  # moar
S15-normalization/nfd-0.t       # moar stress
S15-normalization/nfd-1.t       # moar stress
S15-normalization/nfd-2.t       # moar stress
S15-normalization/nfd-3.t       # moar stress
S15-normalization/nfd-4.t       # moar stress
S15-normalization/nfd-5.t       # moar stress
S15-normalization/nfd-6.t       # moar stress
S15-normalization/nfd-7.t       # moar stress
S15-normalization/nfd-8.t       # moar stress
S15-normalization/nfd-9.t       # moar stress
S15-normalization/nfd-sanity.t  # moar
S15-normalization/nfkc-0.t      # moar stress
S15-normalization/nfkc-1.t      # moar stress
S15-normalization/nfkc-2.t      # moar stress
S15-normalization/nfkc-3.t      # moar stress
S15-normalization/nfkc-4.t      # moar stress
S15-normalization/nfkc-5.t      # moar stress
S15-normalization/nfkc-6.t      # moar stress
S15-normalization/nfkc-7.t      # moar stress
S15-normalization/nfkc-8.t      # moar stress
S15-normalization/nfkc-9.t      # moar stress
S15-normalization/nfkc-sanity.t # moar
S15-normalization/nfkd-0.t      # moar stress
S15-normalization/nfkd-1.t      # moar stress
S15-normalization/nfkd-2.t      # moar stress
S15-normalization/nfkd-3.t      # moar stress
S15-normalization/nfkd-4.t      # moar stress
S15-normalization/nfkd-5.t      # moar stress
S15-normalization/nfkd-6.t      # moar stress
S15-normalization/nfkd-7.t      # moar stress
S15-normalization/nfkd-8.t      # moar stress
S15-normalization/nfkd-9.t      # moar stress
S15-normalization/nfkd-sanity.t # moar
S15-string-types/Uni.t          # moar
S15-string-types/Str.t
S15-unicode-information/uniname.t
S15-unicode-information/unimatch-general.t	# moar
S15-unicode-information/uniprop.t           # moar
S15-unicode-information/unival.t	        # moar
S16-filehandles/argfiles.t
S16-filehandles/chmod.t
S16-filehandles/filestat.t
S16-filehandles/filetest.t
S16-filehandles/io_in_for_loops.t
S16-filehandles/io_in_while_loops.t
S16-filehandles/io.t
S16-filehandles/mkdir_rmdir.t
S16-filehandles/open.t
S16-filehandles/unlink.t
S16-io/bare-say.t
S16-io/basic-open.t
S16-io/bom.t
S16-io/comb.t
S16-io/cwd.t
S16-io/getc.t
S16-io/lines.t
S16-io/newline.t
S16-io/note.t
S16-io/print.t
S16-io/say-and-ref.t
S16-io/say.t
S16-io/split.t
S16-io/supply.t
S16-io/tmpdir.t
S16-io/words.t
S16-unfiled/rebindstdhandles.t
S17-channel/basic.t
S17-lowlevel/lock.t      # slow
S17-lowlevel/thread.t
S17-lowlevel/thread-start-join-stress.t # stress
S17-procasync/basic.t    # moar
S17-procasync/print.t    # moar
S17-procasync/kill.t     # moar stress slow
S17-procasync/no-runaway-file-limit.t # moar slow
S17-procasync/many-processes-no-close-stdin.t # moar slow
S17-promise/allof.t      # slow
S17-promise/at.t
S17-promise/anyof.t
S17-promise/basic.t
S17-promise/in.t         # slow
S17-promise/start.t      # slow
S17-promise/stress.t     # stress
S17-promise/then.t
S17-scheduler/at.t       # slow
S17-scheduler/basic.t
S17-scheduler/every.t    # slow
S17-scheduler/in.t       # slow
S17-scheduler/times.t    # slow
S17-supply/act.t         # slow
S17-supply/basic.t
S17-supply/batch.t       # slow
S17-supply/categorize.t
S17-supply/Channel.t
S17-supply/classify.t
S17-supply/delayed.t     # slow
S17-supply/do.t
S17-supply/elems.t       # slow
S17-supply/flat.t
S17-supply/from-list.t
S17-supply/grab.t
S17-supply/grep.t
S17-supply/head.t
S17-supply/interval.t
S17-supply/lines.t
S17-supply/list.t
S17-supply/map.t
S17-supply/max.t
S17-supply/merge.t
S17-supply/migrate.t
S17-supply/min.t
S17-supply/minmax.t
S17-supply/on-demand.t
S17-supply/Promise.t
S17-supply/produce.t
S17-supply/reduce.t
S17-supply/reverse.t
S17-supply/rotor.t
S17-supply/schedule-on.t
S17-supply/sort.t
S17-supply/squish.t
S17-supply/stable.t       # slow
S17-supply/start.t        # slow
S17-supply/syntax.t       # slow
S17-supply/tail.t
S17-supply/throttle.t     # slow
S17-supply/unique.t       # slow
S17-supply/wait.t         # slow
S17-supply/watch-path.t   # slow
S17-supply/words.t
S17-supply/zip.t
S17-supply/zip-latest.t
S19-command-line/arguments.t
S19-command-line/dash-e.t
S19-command-line/help.t
S19-command-line/repl.t    # moar
S19-command-line-options/02-dash-n.t
S19-command-line-options/03-dash-p.t
S22-package-format/local.t
S24-testing/0-compile.t
S24-testing/3-output.t
S24-testing/line-numbers.t
S26-documentation/01-delimited.t
S26-documentation/02-paragraph.t
S26-documentation/03-abbreviated.t
S26-documentation/04-code.t
S26-documentation/05-comment.t
S26-documentation/06-lists.t
S26-documentation/07-tables.t
S26-documentation/08-formattingcodes.t
S26-documentation/09-configuration.t
S26-documentation/10-doc-cli.t
S26-documentation/block-leading.t
S26-documentation/block-trailing.t
S26-documentation/module-comment.t
S26-documentation/multiline-leading.t
S26-documentation/multiline-trailing.t
S26-documentation/wacky.t
S26-documentation/why-both.t
S26-documentation/why-trailing.t
S26-documentation/why-leading.t
S28-named-variables/cwd.t
S28-named-variables/slangs.t    # moar
S29-any/are.t
S29-any/cmp.t
S29-any/isa.t
S29-context/die.t
S29-context/eval.t
S29-context/evalfile.t
S29-context/exit-in-if.t
S29-context/exit.t
S29-context/sleep.t  # slow
S29-conversions/hash.t
S29-conversions/ord_and_chr.t
S29-os/system.t
S32-array/adverbs.t
S32-array/bool.t
S32-array/create.t
S32-array/delete.t
S32-array/delete-adverb.t
S32-array/delete-adverb-native.t
S32-array/elems.t
S32-array/end.t
S32-array/exists-adverb.t
S32-array/keys_values.t
S32-array/kv.t
S32-array/pairs.t
S32-array/perl.t
S32-array/pop.t
S32-array/push.t
S32-array/rotate.t
S32-array/shift.t
S32-array/splice.t
S32-array/unshift.t
S32-basics/warn.t
S32-basics/xxKEY.t
S32-basics/xxPOS.t
S32-basics/xxPOS-native.t    # moar
S32-container/cat.t
S32-container/roundrobin.t
S32-container/stringify.t
S32-container/zip.t
S32-exceptions/misc.t
S32-hash/adverbs.t
S32-hash/antipairs.t
S32-hash/delete.t
S32-hash/delete-adverb.t
S32-hash/exists.t
S32-hash/exists-adverb.t
S32-hash/invert.t
S32-hash/keys_values.t
S32-hash/kv.t
S32-hash/pairs.t
S32-hash/perl.t
S32-hash/push.t
S32-hash/slice.t
S32-io/IO-Socket-Async.t
S32-io/IO-Socket-Async-UDP.t    # moar
S32-io/chdir.t
S32-io/copy.t
S32-io/dir.t
S32-io/file-tests.t
S32-io/io-handle.t
S32-io/io-spec-unix.t
S32-io/io-spec-win.t
S32-io/io-spec-cygwin.t
S32-io/io-path-unix.t
S32-io/io-path-win.t
S32-io/io-path-cygwin.t
S32-io/io-path.t
S32-io/IO-Socket-INET.t
S32-io/move.t
S32-io/native-descriptor.t  # moar
S32-io/note.t
S32-io/open.t
S32-io/other.t
S32-io/pipe.t
S32-io/rename.t
S32-io/socket-recv-vs-read.t
S32-io/slurp.t
S32-io/spurt.t
S32-list/categorize.t
S32-list/classify.t
S32-list/create.t
S32-list/combinations.t
S32-list/end.t
S32-list/first.t
S32-list/first-end.t
S32-list/first-end-k.t
S32-list/first-end-p.t
S32-list/first-end-v.t
S32-list/first-k.t
S32-list/first-p.t
S32-list/first-v.t
S32-list/grep.t
S32-list/grep-k.t
S32-list/grep-kv.t
S32-list/grep-p.t
S32-list/grep-v.t
S32-list/head.t
S32-list/join.t
S32-list/map_function_return_values.t
S32-list/map.t
S32-list/minmax.t
S32-list/permutations.t
S32-list/pick.t
S32-list/produce.t
S32-list/reduce.t
S32-list/repeated.t
S32-list/reverse.t
S32-list/roll.t
S32-list/rotor.t
S32-list/seq.t
S32-list/sort.t
S32-list/tail.t
S32-list/unique.t
S32-list/squish.t
S32-num/abs.t
S32-num/base.t
S32-num/complex.t
S32-num/cool-num.t
S32-num/exp.t
S32-num/expmod.t
S32-num/fatrat.t
S32-num/int.t
S32-num/is-prime.t
S32-num/log.t
S32-num/narrow.t
S32-num/pi.t
S32-num/polar.t
S32-num/polymod.t
S32-num/power.t
S32-num/rand.t
S32-num/rat.t
S32-num/real-bridge.t
S32-num/roots.t
S32-num/rounders.t
S32-num/rshift_pos_amount.t
S32-num/sign.t
S32-num/sqrt.t
S32-num/stringify.t
S32-num/unpolar.t
S32-scalar/defined.t
S32-scalar/perl.t
S32-scalar/undef.t
S32-str/append.t
S32-str/bool.t
S32-str/capitalize.t
S32-str/chomp.t
S32-str/chop.t
S32-str/comb.t
S32-str/contains.t
S32-str/encode.t
S32-str/ends-with.t
S32-str/fc.t            # moar
S32-str/flip.t
S32-str/indent.t
S32-str/index.t
S32-str/indices.t
S32-str/lc.t
S32-str/length.t
S32-str/lines.t
S32-str/numeric.t
S32-str/ords.t
S32-str/pack.t
S32-str/pos.t
S32-str/rindex.t
S32-str/samemark.t
S32-str/samecase.t
S32-str/split-simple.t
S32-str/split.t
S32-str/sprintf.t
S32-str/sprintf-b.t
S32-str/starts-with.t
S32-str/substr.t
S32-str/substr-eq.t
S32-str/substr-rw.t
S32-str/tc.t
S32-str/tclc.t
S32-str/trim.t
S32-str/uc.t
S32-str/unpack.t
S32-str/utf8-c8.t       # moar
S32-str/val.t
S32-str/words.t
S32-temporal/calendar.t
S32-temporal/Date.t
S32-temporal/DateTime-Instant-Duration.t
S32-temporal/DateTime.t   # slow
S32-temporal/local.t
S32-trig/atan2.t
S32-trig/cosech.t
S32-trig/cosec.t
S32-trig/cosh.t
S32-trig/cos.t
S32-trig/cotanh.t
S32-trig/cotan.t
S32-trig/e.t
S32-trig/pi.t
S32-trig/sech.t
S32-trig/sec.t
S32-trig/simple.t
S32-trig/sinh.t
S32-trig/sin.t
S32-trig/tanh.t
S32-trig/tan.t
integration/99problems-01-to-10.t
integration/99problems-11-to-20.t
integration/99problems-21-to-30.t
integration/99problems-31-to-40.t
integration/99problems-41-to-50.t
integration/99problems-51-to-60.t
integration/99problems-61-to-70.t
integration/advent2009-day01.t
integration/advent2009-day02.t
integration/advent2009-day03.t
integration/advent2009-day04.t
integration/advent2009-day05.t
integration/advent2009-day06.t
integration/advent2009-day07.t
integration/advent2009-day08.t
integration/advent2009-day09.t
integration/advent2009-day10.t
integration/advent2009-day11.t
integration/advent2009-day12.t
integration/advent2009-day13.t
integration/advent2009-day14.t
integration/advent2009-day15.t
integration/advent2009-day16.t
integration/advent2009-day17.t
integration/advent2009-day18.t
integration/advent2009-day19.t
integration/advent2009-day20.t
integration/advent2009-day21.t
integration/advent2009-day22.t
integration/advent2009-day23.t
integration/advent2009-day24.t
integration/advent2010-day03.t
integration/advent2010-day04.t
integration/advent2010-day06.t
integration/advent2010-day07.t
integration/advent2010-day08.t
integration/advent2010-day10.t
integration/advent2010-day11.t
integration/advent2010-day12.t
integration/advent2010-day14.t
integration/advent2010-day16.t
integration/advent2010-day19.t
integration/advent2010-day21.t
integration/advent2010-day22.t
integration/advent2010-day23.t
integration/advent2011-day03.t
integration/advent2011-day04.t
integration/advent2011-day05.t
integration/advent2011-day07.t
integration/advent2011-day10.t
integration/advent2011-day11.t
integration/advent2011-day14.t
integration/advent2011-day15.t
integration/advent2011-day16.t
integration/advent2011-day20.t
integration/advent2011-day22.t
integration/advent2011-day23.t
integration/advent2011-day24.t
integration/advent2012-day02.t
integration/advent2012-day03.t
integration/advent2012-day04.t #stress
integration/advent2012-day06.t
integration/advent2012-day09.t
integration/advent2012-day10.t
integration/advent2012-day12.t
integration/advent2012-day13.t
integration/advent2012-day14.t
integration/advent2012-day15.t
integration/advent2012-day16.t
integration/advent2012-day19.t # slow
integration/advent2012-day20.t
integration/advent2012-day21.t #stress
integration/advent2012-day22.t
integration/advent2012-day23.t
integration/advent2012-day24.t
integration/advent2013-day02.t
integration/advent2013-day04.t
integration/advent2013-day06.t
integration/advent2013-day07.t
integration/advent2013-day08.t
integration/advent2013-day09.t
integration/advent2013-day10.t
integration/advent2013-day12.t
integration/advent2013-day14.t # slow
integration/advent2013-day15.t
integration/advent2013-day18.t
integration/advent2013-day19.t
integration/advent2013-day20.t
integration/advent2013-day21.t
integration/advent2013-day22.t
integration/advent2013-day23.t
integration/advent2014-day05.t #stress
integration/advent2014-day13.t
integration/advent2014-day16.t
integration/code-blocks-as-sub-args.t
integration/error-reporting.t  # slow
integration/lazy-bentley-generator.t
integration/lexical-array-in-inner-block.t
integration/lexicals-and-attributes.t
integration/man-or-boy.t
integration/method-calls-and-instantiation.t
integration/no-indirect-new.t
integration/packages.t
integration/pair-in-array.t
integration/passing-pair-class-to-sub.t
integration/precompiled.t      # moar slow
integration/real-strings.t
integration/role-composition-vs-attribute.t
integration/rule-in-class-Str.t
integration/say-crash.t
integration/substr-after-match-in-gather-in-for.t
integration/topic_in_double_loop.t
integration/variables-in-do.t
integration/weird-errors.t     # slow
rosettacode/greatest_element_of_a_list.t
rosettacode/sierpinski_triangle.t
