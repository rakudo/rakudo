# a helper class to abstract support for unicodey functions
my class Rakudo::Unicodey is implementation-detail {

#?if jvm
    method unival() is hidden-from-backtrace {
        X::NYI.new(:feature<unival>).throw
    }

    method ords(str $str) {  # strtocodes NYI on JVM
        my @ords;
        my int $chars = nqp::chars($str);
        my int $i     = -1;

        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$chars),
          @ords.push(nqp::ord($str,$i))
        );

        @ords
    }

    method NFC(str) is hidden-from-backtrace {
        X::NYI.new(:feature<NFC>).throw
    }
    method NFD(str)  is hidden-from-backtrace {
        X::NYI.new(:feature<NFD>).throw
    }
    method NFKC(str) is hidden-from-backtrace {
        X::NYI.new(:feature<NFKC>).throw
    }
    method NFKD(str) is hidden-from-backtrace {
        X::NYI.new(:feature<NFKD>).throw
    }
#?endif

#?if !jvm
    my constant $nuprop = nqp::unipropcode("Numeric_Value_Numerator");
    my constant $deprop = nqp::unipropcode("Numeric_Value_Denominator");

    method unival(int $ord) {
        nqp::chars(my str $de = nqp::getuniprop_str($ord,$deprop))
          ?? nqp::iseq_s($de,"NaN")                 # some string to work with
            ?? NaN                                   # no value found
            !! nqp::iseq_s($de,"1")                  # some value
              ?? nqp::coerce_si(nqp::getuniprop_str($ord,$nuprop))
              !! Rat.new(
                   nqp::coerce_si(nqp::getuniprop_str($ord,$nuprop)),
                   nqp::coerce_si($de)
                 )
          !! Nil                                    # not valid, so no value
    }

    method ords(str $str) {
        nqp::strtocodes(
          $str,
          nqp::const::NORMALIZE_NFC,
          nqp::create(array[uint32])
        )
    }

    method NFC(str $str) {
        nqp::strtocodes($str,nqp::const::NORMALIZE_NFC,nqp::create(NFC))
    }
    method NFD(str $str) {
        nqp::strtocodes($str,nqp::const::NORMALIZE_NFD,nqp::create(NFD))
    }
    method NFKC(str $str) {
        nqp::strtocodes($str,nqp::const::NORMALIZE_NFKC,nqp::create(NFKC))
    }
    method NFKD(str $str) {
        nqp::strtocodes($str,nqp::const::NORMALIZE_NFKD,nqp::create(NFKD))
    }
#?endif

    my role UnicodeyIterator does PredictiveIterator {
        has $!codes;
        method new(str $str) {
            nqp::p6bindattrinvres(
              nqp::create(self),
              self,
              '$!codes',
              Rakudo::Unicodey.ords($str)
            )
        }
        method skip-one() {
            nqp::if(
              nqp::elems($!codes),
              nqp::shift_i($!codes)
            )
        }
        method count-only(--> Int:D) { nqp::elems($!codes) }
        method bool-only(--> Bool:D) { nqp::hllbool(nqp::elems($!codes)) }
    }

    my class UninamesIterator does UnicodeyIterator {
        method pull-one() {
            nqp::elems($!codes)
              ?? nqp::getuniname(nqp::shift_i($!codes))
              !! IterationEnd
        }
        method push-all(\target --> IterationEnd) {
            my $codes := $!codes;
            nqp::while(
              nqp::elems($codes),
              target.push(nqp::getuniname(nqp::shift_i($codes)))
            );
        }
    }
    method uninames(str $str) { UninamesIterator.new($str) }

    my class UnivalsIterator does UnicodeyIterator {
        method pull-one() {
            nqp::elems($!codes)
              ?? Rakudo::Unicodey.unival(nqp::shift_i($!codes))
              !! IterationEnd
        }
        method push-all(\target --> IterationEnd) {
            my $codes := $!codes;
            nqp::while(
              nqp::elems($codes),
              target.push(Rakudo::Unicodey.unival(nqp::shift_i($codes)))
            );
        }
    }
    method univals(str $str) { UnivalsIterator.new($str) }
}

augment class Cool {
    proto method chr(*%) is pure {*}
    multi method chr(Cool:D:) { self.Int.chr }

#    proto method chrs(*%) is pure {*}  # lives in Any-iterable
    multi method chrs(Cool:D:) { self.list.chrs }

    proto method ord(*%) is pure {*}
    multi method ord(Cool:D: --> Int:D) { self.Str.ord }

    proto method ords(*%) is pure {*}
    multi method ords(Cool:D:) { self.Str.ords }

    proto method uniname(*%) is pure {*}
    multi method uniname(Cool:D: --> Str:D) { self.Int.uniname }

    proto method uninames(*%) is pure {*}
    multi method uninames(Cool:D: --> Str:D) { self.Str.uninames }

    proto method unival(*%) is pure {*}
    multi method unival(Cool:D:) { self.Int.unival }

    proto method univals(*%) is pure {*}
    multi method univals(Cool:D:) { self.Str.univals }

    method uniprop(|c)      { uniprop(self, |c) }
    method uniprop-int(|c)  { uniprop-int(self, |c) }
    method uniprop-bool(|c) { uniprop-bool(self, |c) }
    method uniprop-str(|c)  { uniprop-str(self, |c) }
    method uniprops(|c)     { uniprops(self, |c) }
    method unimatch(|c)     { unimatch(self, |c) }

    proto method NFC(*%) {*}
    multi method NFC(Cool:D:) { self.Str.NFC }

    proto method NFD(*%) {*}
    multi method NFD(Cool:D:) { self.Str.NFD }

    proto method NFKC(*%) {*}
    multi method NFKC(Cool:D:) { self.Str.NFKC }

    proto method NFKD(*%) {*}
    multi method NFKD(Cool:D:) { self.Str.NFKD }
}

augment class Int {

    method !codepoint-out-of-bounds(str $action) {
        die "Codepoint %i (0x%X) is out of bounds in '$action'".sprintf(self,self)
    }

    multi method chr(Int:D: --> Str:D) {
        nqp::isbig_I(self) || nqp::islt_I(self,0)
          ?? self!codepoint-out-of-bounds('chr')
          !! nqp::chr(self)
    }

    multi method uniname(Int:D: --> Str:D) {
        nqp::islt_I(self,0)       # (bigint) negative number?
          ?? '<illegal>'
          !! nqp::isbig_I(self)   # bigint positive number?
            ?? '<unassigned>'
            !! nqp::getuniname(self)
    }

    multi method unival(Int:D:) {
        nqp::isbig_I(self) || nqp::islt_I(self,0)
          ?? self!codepoint-out-of-bounds('unival')
          !! Rakudo::Unicodey.unival(self)
    }
}

augment class Str {
    multi method ord(Str:D: --> Int:D) {
        nqp::chars($!value) ?? nqp::p6box_i(nqp::ord($!value)) !! Nil
    }

    multi method ords(Str:D: --> Seq:D) {
        Seq.new(Rakudo::Unicodey.ords($!value).iterator)
    }

    multi method uniname(Str:D: --> Str:D) {
        nqp::iseq_i((my int $ord = nqp::ord($!value)),-1)
          ?? Nil
          !! nqp::getuniname($ord)
    }

    multi method uninames(Str:D:) {
        Seq.new(Rakudo::Unicodey.uninames(self))
    }

    multi method unival(Str:D:) {
        nqp::iseq_i((my int $ord = nqp::ord($!value)),-1)
          ?? Nil
          !! Rakudo::Unicodey.unival($ord)
    }

    multi method univals(Str:D:) {
        Seq.new(Rakudo::Unicodey.univals(self))
    }

    method uniparse(Str:D: --> Str:D) {
        my $names := nqp::split(',', self);
        my $parts := nqp::list_s;

        nqp::while(
          nqp::elems($names),
          nqp::push_s(
            $parts,
            nqp::unless(
              nqp::strfromname(my $name := nqp::shift($names).trim),
              X::Str::InvalidCharName.new(:$name).fail
            )
          )
        );

        nqp::join("",$parts)
    }

#?if jvm
    multi method NFC(Str:D:)  { Rakudo::Unicodey.NFC($!value)  }
    multi method NFD(Str:D:)  { Rakudo::Unicodey.NFD($!value)  }
    multi method NFKC(Str:D:) { Rakudo::Unicodey.NFKC($!value) }
    multi method NFKD(Str:D:) { Rakudo::Unicodey.NFKD($!value) }
#?endif

#?if !jvm
    multi method NFC(Str:D:  --> NFC:D)  { Rakudo::Unicodey.NFC($!value)  }
    multi method NFD(Str:D:  --> NFD:D)  { Rakudo::Unicodey.NFD($!value)  }
    multi method NFKC(Str:D: --> NFKC:D) { Rakudo::Unicodey.NFKC($!value) }
    multi method NFKD(Str:D: --> NFKD:D) { Rakudo::Unicodey.NFKD($!value) }
#?endif
}

augment class List {
    multi method chrs(List:D: --> Str:D) {
        nqp::if(
          self.is-lazy,
          self.fail-iterator-cannot-be-lazy('.chrs'),
          nqp::stmts(
            (my int $i     = -1),
            (my int $elems = self.elems),    # reifies
            (my $result   := nqp::setelems(nqp::list_s,$elems)),
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::if(
                nqp::istype((my $value := nqp::atpos($!reified,$i)),Int),
                nqp::bindpos_s($result,$i,nqp::chr($value)),
                nqp::if(
                  nqp::istype($value,Str),
                  nqp::if(
                    nqp::istype(($value := +$value),Failure),
                    (return $value),
                    nqp::bindpos_s($result,$i,nqp::chr($value))
                  ),
                  (return Failure.new(X::TypeCheck.new(
                    operation => "converting element #$i to .chr",
                    got       => $value,
                    expected  => Int
                  )))
                )
              )
            ),
            nqp::join("",$result)
          )
        )
    }
}

augment class Nil {
    # These suggest using Nil.new if they fall through, which is LTA
    multi method ords(Nil:) { self.Str.ords }
    multi method chrs(Nil:) { self.Int.chrs }
}

# Make sure all affected subclasses are aware of additions to their parents
BEGIN .^compose for
  Array, Match, Range, Seq,
  Int, Num, Rat, Complex,
  IntStr, NumStr, RatStr, ComplexStr
;

# all proto's in one place so they're available on all (conditional) backends
#-------------------------------------------------------------------------------
proto sub chr($, *%) is pure {*}
proto sub chrs(|)    is pure {*}

proto sub ord($, *%)  is pure {*}
proto sub ords($, *%) is pure {*}

proto sub uniname($, *%)  is pure {*}
proto sub uninames($, *%) is pure {*}

proto sub uniprop($, |)       is pure {*}
proto sub uniprops($, $?, *%) is pure {*}

proto sub uniprop-bool($, $, *%) is pure {*}
proto sub uniprop-int($, $, *%)  is pure {*}
proto sub uniprop-str($, $, *%)  is pure {*}

proto sub unimatch($, |) is pure {*}

proto sub unival($, *%)  is pure {*}
proto sub univals($, *%) is pure {*}
#-------------------------------------------------------------------------------

multi sub chr(\what) { what.chr }

multi sub chrs(*@c --> Str:D) { @c.chrs }

multi sub ord(\what) { what.ord }
multi sub ords($s) { $s.ords }

multi sub uniname(\what) { what.uniname }
multi sub uninames(\what) { what.uninames }

multi sub unival(\what) { what.unival }

#?if jvm
multi sub univals(|)      { die 'univals NYI on jvm backend' }
multi sub uniprop(|)      { die 'uniprop NYI on jvm backend' }
multi sub uniprop-int(|)  { die 'uniprop-int NYI on jvm backend' }
multi sub uniprop-bool(|) { die 'uniprop-bool NYI on jvm backend' }
multi sub uniprop-str(|)  { die 'uniprop-str NYI on jvm backend' }
multi sub uniprops(|)     { die 'uniprops NYI on jvm backend' }
multi sub unimatch(|)     { die 'unimatch NYI on jvm backend' }
#?endif

#?if js
multi sub uniprop(|)      { die 'uniprop NYI on js backend' }
multi sub uniprop-int(|)  { die 'uniprop-int NYI on js backend' }
multi sub uniprop-bool(|) { die 'uniprop-bool NYI on js backend' }
multi sub uniprop-str(Int:D $code, Stringy:D $propname) {
    nqp::getuniprop_str($code,nqp::unipropcode($propname));
}
multi sub uniprops(|)     { die 'uniprops NYI on jvm backend' }
multi sub unimatch(|)     { die 'unimatch NYI on js backend' }
#?endif

#?if !jvm
multi sub univals(Str:D $str) { $str.univals }
#?endif

#?if moar
multi sub uniprop(Str:D $str, |c) { $str ?? uniprop($str.ord, |c) !! Nil }
multi sub uniprop(Int:D $code) {
    nqp::getuniprop_str($code,nqp::unipropcode('General_Category'));
}
multi sub uniprop(Int:D $code, Stringy:D $propname) {
    ## The code below was generated by tools/build/makeUNIPROP.raku
    my constant $prefs = nqp::hash(
      'AHex','B','ASCII_Hex_Digit','B','Age','S','Alpha','B','Alphabetic','B',
      'Bidi_C','B','Bidi_Class','S','Bidi_Control','B','Bidi_M','B',
      'Bidi_Mirrored','B','Bidi_Mirroring_Glyph','bmg',
      'Bidi_Paired_Bracket_Type','S','Block','S','CE','B','CI','B','CWCF','B',
      'CWCM','B','CWKCF','B','CWL','B','CWT','B','CWU','B',
      'Canonical_Combining_Class','S','Case_Folding','S','Case_Ignorable','B',
      'Cased','B','Changes_When_Casefolded','B','Changes_When_Casemapped','B',
      'Changes_When_Lowercased','B','Changes_When_NFKC_Casefolded','B',
      'Changes_When_Titlecased','B','Changes_When_Uppercased','B','Comp_Ex','B',
      'Composition_Exclusion','B','DI','B','Dash','B',
      'Decomposition_Mapping','S','Decomposition_Type','S',
      'Default_Ignorable_Code_Point','B','Dep','B','Deprecated','B','Dia','B',
      'Diacritic','B','East_Asian_Width','S','Emoji','B','Emoji_Modifier','B',
      'Emoji_Modifier_Base','B','Emoji_Presentation','B','Expands_On_NFC','B',
      'Expands_On_NFD','B','Expands_On_NFKC','B','Expands_On_NFKD','B',
      'Ext','B','Extender','B','FC_NFKC','S','FC_NFKC_Closure','S',
      'Full_Composition_Exclusion','B','GCB','S','General_Category','S',
      'Gr_Base','B','Gr_Ext','B','Gr_Link','B','Grapheme_Base','B',
      'Grapheme_Cluster_Break','S','Grapheme_Extend','B','Grapheme_Link','B',
      'Hangul_Syllable_Type','S','Hex','B','Hex_Digit','B','Hyphen','B',
      'IDC','B','IDS','B','IDSB','B','IDST','B','IDS_Binary_Operator','B',
      'IDS_Trinary_Operator','B','ID_Continue','B','ID_Start','B',
      'ISO_Comment','S','Ideo','B','Ideographic','B','InPC','S','InSC','S',
      'Indic_Positional_Category','S','Indic_Syllabic_Category','S',
      'Join_C','B','Join_Control','B','Joining_Group','S','Joining_Type','S',
      'LOE','B','Line_Break','S','Logical_Order_Exception','B','Lower','B',
      'Lowercase','B','Lowercase_Mapping','lc','Math','B','NChar','B',
      'NFC_QC','S','NFC_Quick_Check','S','NFD_QC','S','NFD_Quick_Check','S',
      'NFKC_CF','S','NFKC_Casefold','S','NFKC_QC','S','NFKC_Quick_Check','S',
      'NFKD_QC','S','NFKD_Quick_Check','S','Name','na',
      'Noncharacter_Code_Point','B','Numeric_Type','S','Numeric_Value','nv',
      'OAlpha','B','ODI','B','OGr_Ext','B','OIDC','B','OIDS','B','OLower','B',
      'OMath','B','OUpper','B','Other_Alphabetic','B',
      'Other_Default_Ignorable_Code_Point','B','Other_Grapheme_Extend','B',
      'Other_ID_Continue','B','Other_ID_Start','B','Other_Lowercase','B',
      'Other_Math','B','Other_Uppercase','B','PCM','B','Pat_Syn','B',
      'Pat_WS','B','Pattern_Syntax','B','Pattern_White_Space','B',
      'Prepended_Concatenation_Mark','B','QMark','B','Quotation_Mark','B',
      'RI','B','Radical','B','Regional_Indicator','B','SB','S','SD','B',
      'STerm','B','Script','S','Sentence_Break','S','Sentence_Terminal','B',
      'Simple_Case_Folding','S','Simple_Lowercase_Mapping','S',
      'Simple_Titlecase_Mapping','S','Simple_Uppercase_Mapping','S',
      'Soft_Dotted','B','Term','B','Terminal_Punctuation','B',
      'Titlecase_Mapping','tc','UIdeo','B','Unified_Ideograph','B','Upper','B',
      'Uppercase','B','Uppercase_Mapping','uc','VS','B',
      'Variation_Selector','B','Vertical_Orientation','S','WB','S','WSpace','B',
      'White_Space','B','Word_Break','S','XIDC','B','XIDS','B',
      'XID_Continue','B','XID_Start','B','XO_NFC','B','XO_NFD','B',
      'XO_NFKC','B','XO_NFKD','B','age','S','bc','S','blk','S','bmg','bmg',
      'bpt','S','ccc','S','cf','S','cjkCompatibilityVariant','S','dm','S',
      'dt','S','ea','S','gc','S','hst','S','isc','S','jg','S','jt','S',
      'kCompatibilityVariant','S','lb','S','lc','lc','na','na','nt','S',
      'nv','nv','sc','S','scf','S','sfc','S','slc','S','space','B','stc','S',
      'suc','S','tc','tc','uc','uc','vo','S',
    );
    ## End generated code
    my int $prop = nqp::unipropcode($propname);
    my str $pref = nqp::ifnull(nqp::atkey($prefs, $propname),'');
    nqp::if(
      nqp::iseq_s($pref, 'S'),
      nqp::getuniprop_str($code,$prop),
      nqp::if(
        nqp::iseq_s($pref, 'I'),
        nqp::getuniprop_int($code,$prop),
        nqp::if(
          nqp::iseq_s($pref, 'B'),
          nqp::hllbool(nqp::getuniprop_bool($code,$prop)),
          nqp::if(
            nqp::iseq_s($pref, 'lc'),
            nqp::lc(nqp::chr(nqp::unbox_i($code))),
            nqp::if(
              nqp::iseq_s($pref, 'tc'),
              nqp::tc(nqp::chr(nqp::unbox_i($code))),
              nqp::if(
                nqp::iseq_s($pref, 'uc'),
                nqp::uc(nqp::chr(nqp::unbox_i($code))),
                nqp::if(
                  nqp::iseq_s($pref, 'na'),
                  nqp::getuniname($code),
                  nqp::if(
                    nqp::iseq_s($pref, 'nv'),
                    $code.unival,
                    nqp::if(
                      nqp::iseq_s($pref, 'bmg'),
                      nqp::stmts(
                        (my int $bmg-ord = nqp::getuniprop_int($code, $prop)),
                        $bmg-ord ?? nqp::chr($bmg-ord) !! ''),
                      nqp::stmts(
                        (my $result := nqp::getuniprop_str($code,$prop)),
                        nqp::if(
                          nqp::istrue($result),
                          nqp::stmts(
                            nqp::bindkey($prefs, $propname, 'S'),
                            $result),
                          nqp::stmts(
                            nqp::bindkey($prefs, $propname, 'I'),
                            nqp::getuniprop_int($code,$prop)))))))))))))
}
# Unicode functions
multi sub uniprop-int(Str:D $str, Stringy:D $propname) {
    $str ?? uniprop-int($str.ord, $propname) !! Nil }
multi sub uniprop-int(Int:D $code, Stringy:D $propname) {
    nqp::getuniprop_int($code,nqp::unipropcode($propname));
}

multi sub uniprop-bool(Str:D $str, Stringy:D $propname) {
    $str ?? uniprop-bool($str.ord, $propname) !! Nil
}
multi sub uniprop-bool(Int:D $code, Stringy:D $propname) {
    nqp::hllbool(nqp::getuniprop_bool($code,nqp::unipropcode($propname)));
}

multi sub uniprop-str(Str:D $str, Stringy:D $propname) {
    $str ?? uniprop-str($str.ord, $propname) !! Nil
}
multi sub uniprop-str(Int:D $code, Stringy:D $propname) {
    nqp::getuniprop_str($code,nqp::unipropcode($propname));
}
multi sub uniprops(Str:D $str, Stringy:D $propname = "General_Category") {
    $str.ords.map: { uniprop($_, $propname) }
}

multi sub unimatch(Str:D $str, |c) { $str ?? unimatch($str.ord, |c) !! Nil }
multi sub unimatch(Int:D $code, Stringy:D $pvalname, Stringy:D $propname = $pvalname) {
    my $prop := nqp::unipropcode($propname);
    nqp::hllbool(nqp::matchuniprop($code,$prop,nqp::unipvalcode($prop,$pvalname)));
}
#?endif

#?if !jvm
multi sub infix:<unicmp>(Str:D \a, Str:D \b) {
    ORDER(
      nqp::unicmp_s(nqp::unbox_s(a), nqp::unbox_s(b), 85,0,0)
    )
}
multi sub infix:<unicmp>(Cool:D \a, Cool:D \b) {
    ORDER(
      nqp::unicmp_s(nqp::unbox_s(a.Str), nqp::unbox_s(b.Str), 85,0,0)
    )
}
multi sub infix:<unicmp>(Pair:D \a, Pair:D \b) {
    nqp::eqaddr((my $cmp := (a.key unicmp b.key)),Order::Same)
      ?? (a.value unicmp b.value)
      !! $cmp
}

multi sub infix:<coll>(Str:D \a, Str:D \b) {
    ORDER(
      nqp::unicmp_s(
        nqp::unbox_s(a),nqp::unbox_s(b),$*COLLATION.collation-level,0,0
      )
    )
}
multi sub infix:<coll>(Cool:D \a, Cool:D \b) {
    ORDER(
      nqp::unicmp_s(
        nqp::unbox_s(a.Str),nqp::unbox_s(b.Str),$*COLLATION.collation-level,0,0
      )
    )
}
multi sub infix:<coll>(Pair:D \a, Pair:D \b) {
    nqp::eqaddr((my $cmp := (a.key coll b.key)),Order::Same)
      ?? (a.value coll b.value)
      !! $cmp
}
#?endif

#?if jvm
multi sub infix:<unicmp>($, $) {
    X::NYI.new(feature => "infix unicmp on JVM").throw
}   
multi sub infix:<coll>($, $) {
    X::NYI.new(feature => "infix coll on JVM").throw
}   
#?endif

proto sub uniparse($, *%) {*}
multi sub uniparse(Str:D \names --> Str:D) { names.uniparse }

# vim: expandtab shiftwidth=4
