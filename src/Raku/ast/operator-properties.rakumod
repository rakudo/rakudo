#-------------------------------------------------------------------------------
# The OperatorProperties class attempts to encapsulate all possible information
# about all types of operators in the Raku Programming Language.

# Historically, this information was represented by a number of internal
# hashes that lived in the grammar.

# The OperatorProperties object is a value type: this means that it cannot
# be mutated.  Creating a derived OperatorProperties object from an existing
# one *is* possible by calling .new on an OperatorProperties instance, and
# only specifying the arguments that you want to have changed.

# The external interface allows looking up the operator properties of infix,
# prefix, postfix and postcircumfix operators.

class OperatorProperties {
    has str $.precedence;
    has str $.sub-precedence;
    has str $.associative;
    has str $.thunky;
    has str $.dba;
    has str $.next-term;

    has int $.iffy;
    has int $.diffy;
    has int $.fiddly;
    has int $.adverb;
    has int $.ternary;
    has int $.commutative;

    # Basic interface
    method new(
      str :$precedence,
      str :$sub-precedence,
      str :$associative,
      str :$thunky,
      str :$dba,
      str :$next-term,
      int :$iffy,
      int :$diffy,
      int :$fiddly,
      int :$adverb,
      int :$ternary,
      int :$commutative
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj,OperatorProperties,'$!precedence',
          $precedence // (nqp::isconcrete(self) ?? $!precedence !! ""));
        nqp::bindattr_s($obj,OperatorProperties,'$!sub-precedence',
          $sub-precedence // (nqp::isconcrete(self) ?? $!sub-precedence !! ""));
        nqp::bindattr_s($obj,OperatorProperties,'$!associative',
          $associative // (nqp::isconcrete(self) ?? $!associative !! ""));
        nqp::bindattr_s($obj,OperatorProperties,'$!thunky',
          $thunky // (nqp::isconcrete(self) ?? $!thunky !! ""));
        nqp::bindattr_s($obj,OperatorProperties,'$!dba',
          $dba // (nqp::isconcrete(self) ?? $!dba !! ""));
        nqp::bindattr_s($obj,OperatorProperties,'$!next-term',
          $next-term // (nqp::isconcrete(self) ?? $!next-term !! ""));

        nqp::bindattr_i($obj,OperatorProperties,'$!iffy',
          $iffy // (nqp::isconcrete(self) ?? $!iffy !! 0));
        nqp::bindattr_i($obj,OperatorProperties,'$!diffy',
          $diffy // (nqp::isconcrete(self) ?? $!diffy !! 0));
        nqp::bindattr_i($obj,OperatorProperties,'$!fiddly',
          $fiddly // (nqp::isconcrete(self) ?? $!fiddly !! 0));
        nqp::bindattr_i($obj,OperatorProperties,'$!adverb',
          $adverb // (nqp::isconcrete(self) ?? $!adverb !! 0));
        nqp::bindattr_i($obj,OperatorProperties,'$!ternary',
          $ternary // (nqp::isconcrete(self) ?? $!ternary !! 0));
        nqp::bindattr_i($obj,OperatorProperties,'$!commutative',
          $commutative // (nqp::isconcrete(self) ?? $!commutative !! 0));

        $obj
    }

    # Instantiate using an old-style %prec hash
    method new-compat(
      str :$prec,
      str :$sub,
      str :$assoc,
      str :$thunky,
      str :$dba,
      str :$nextterm,
      int :$iffy,
      int :$diffy,
      int :$fiddly,
      int :$adverb,
      int :$ternary,
      int :$commutative,
      *%_
    ) {
        self.new(
          :precedence($prec),
          :sub-precedence($sub),
          :associative($assoc),
          :$thunky,
          :$dba,
          :next-term($nextterm),
          :$iffy,
          :$diffy,
          :$fiddly,
          :$adverb,
          :$ternary,
          :$commutative
        )
    }

    # An OperatorProperties object with associative reversed
    method associative-reversed() {
        nqp::isconcrete(self)
          ?? $!associative eq 'left'
            ?? self.new(:associative<right>, :dba($!dba))
            !! $!associative eq 'right'
              ?? self.new(:associative<left>, :dba($!dba))
              !! self
          !! self
    }

    # A readable .raku representation for debugging
    method raku() {
        my str $name := self.HOW.name(self);
        return $name unless nqp::isconcrete(self);

        my $parts := nqp::list_s;

        nqp::push_s($parts,':precedence("' ~ $!precedence ~ '")')
          if $!precedence;
        nqp::push_s($parts,':sub-precedence("' ~ $!sub-precedence ~ '")')
          if $!sub-precedence;
        nqp::push_s($parts,':associative("' ~ $!associative ~ '")')
          if $!associative;
        nqp::push_s($parts,':thunky("' ~ $!thunky ~ '")')
          if $!thunky;
        nqp::push_s($parts,':dba("' ~ $!dba ~ '")')
          if $!dba;
        nqp::push_s($parts,':next-term("' ~ $!next-term ~ '")')
          if $!next-term;

        nqp::push_s($parts,':iffy')    if $!iffy;
        nqp::push_s($parts,':diffy')   if $!diffy;
        nqp::push_s($parts,':fiddly')  if $!fiddly;
        nqp::push_s($parts,':adverb')  if $!adverb;
        nqp::push_s($parts,':ternary') if $!ternary;
        nqp::push_s($parts,':commutative') if $!commutative;

        $name ~ '.new: ' ~ nqp::join(', ',$parts)
    }

#-------------------------------------------------------------------------------
# Allowable brackets in (post)circumfix operators

    # Allowable characters as openers
    method bracket-openers() {
        my constant OPENERS :=
            "\x0028\x003C\x005B\x007B\x00AB\x0F3A\x0F3C\x169B\x2018\x201A"
          ~ "\x201B\x201C\x201E\x201F\x2039\x2045\x207D\x208D\x2208\x2209"
          ~ "\x220A\x2215\x223C\x2243\x2252\x2254\x2264\x2266\x2268\x226A"
          ~ "\x226E\x2270\x2272\x2274\x2276\x2278\x227A\x227C\x227E\x2280"
          ~ "\x2282\x2284\x2286\x2288\x228A\x228F\x2291\x2298\x22A2\x22A6"
          ~ "\x22A8\x22A9\x22AB\x22B0\x22B2\x22B4\x22B6\x22C9\x22CB\x22D0"
          ~ "\x22D6\x22D8\x22DA\x22DC\x22DE\x22E0\x22E2\x22E4\x22E6\x22E8"
          ~ "\x22EA\x22EC\x22F0\x22F2\x22F3\x22F4\x22F6\x22F7\x2308\x230A"
          ~ "\x23B4\x2768\x276A\x276C\x276E\x2770\x2772\x2774\x27C3\x27C5"
          ~ "\x27D5\x27DD\x27E2\x27E4\x27E6\x27E8\x27EA\x27EC\x27EE\x2983"
          ~ "\x2985\x2987\x2989\x298B\x298D\x298F\x2991\x2993\x2995\x2997"
          ~ "\x29C0\x29C4\x29CF\x29D1\x29D4\x29D8\x29DA\x29F8\x29FC\x2A2B"
          ~ "\x2A2D\x2A34\x2A3C\x2A64\x2A79\x2A7D\x2A7F\x2A81\x2A83\x2A8B"
          ~ "\x2A91\x2A93\x2A95\x2A97\x2A99\x2A9B\x2AA1\x2AA6\x2AA8\x2AAA"
          ~ "\x2AAC\x2AAF\x2AB3\x2ABB\x2ABD\x2ABF\x2AC1\x2AC3\x2AC5\x2ACD"
          ~ "\x2ACF\x2AD1\x2AD3\x2AD5\x2AEC\x2AF7\x2AF9\x2E02\x2E04\x2E09"
          ~ "\x2E0C\x2E1C\x2E20\x2E22\x2E24\x2E26\x2E28\x3008\x3008\x300A"
          ~ "\x300C\x300E\x3010\x3014\x3016\x3018\x301A\x301D\xFE17\xFE35"
          ~ "\xFE37\xFE39\xFE3B\xFE3D\xFE3F\xFE41\xFE43\xFE47\xFE59\xFE5B"
          ~ "\xFE5D\xFF08\xFF1C\xFF3B\xFF5B\xFF5F\xFF62"
        ;
    }

    # Allowable characters as closers, in same order as openers
    method bracket-closers() {
        my constant CLOSERS :=
            "\x0029\x003E\x005D\x007D\x00BB\x0F3B\x0F3D\x169C\x2019\x2019"
          ~ "\x2019\x201D\x201D\x201D\x203A\x2046\x207E\x208E\x220B\x220C"
          ~ "\x220D\x29F5\x223D\x22CD\x2253\x2255\x2265\x2267\x2269\x226B"
          ~ "\x226F\x2271\x2273\x2275\x2277\x2279\x227B\x227D\x227F\x2281"
          ~ "\x2283\x2285\x2287\x2289\x228B\x2290\x2292\x29B8\x22A3\x2ADE"
          ~ "\x2AE4\x2AE3\x2AE5\x22B1\x22B3\x22B5\x22B7\x22CA\x22CC\x22D1"
          ~ "\x22D7\x22D9\x22DB\x22DD\x22DF\x22E1\x22E3\x22E5\x22E7\x22E9"
          ~ "\x22EB\x22ED\x22F1\x22FA\x22FB\x22FC\x22FD\x22FE\x2309\x230B"
          ~ "\x23B5\x2769\x276B\x276D\x276F\x2771\x2773\x2775\x27C4\x27C6"
          ~ "\x27D6\x27DE\x27E3\x27E5\x27E7\x27E9\x27EB\x27ED\x27EF\x2984"
          ~ "\x2986\x2988\x298A\x298C\x2990\x298E\x2992\x2994\x2996\x2998"
          ~ "\x29C1\x29C5\x29D0\x29D2\x29D5\x29D9\x29DB\x29F9\x29FD\x2A2C"
          ~ "\x2A2E\x2A35\x2A3D\x2A65\x2A7A\x2A7E\x2A80\x2A82\x2A84\x2A8C"
          ~ "\x2A92\x2A94\x2A96\x2A98\x2A9A\x2A9C\x2AA2\x2AA7\x2AA9\x2AAB"
          ~ "\x2AAD\x2AB0\x2AB4\x2ABC\x2ABE\x2AC0\x2AC2\x2AC4\x2AC6\x2ACE"
          ~ "\x2AD0\x2AD2\x2AD4\x2AD6\x2AED\x2AF8\x2AFA\x2E03\x2E05\x2E0A"
          ~ "\x2E0D\x2E1D\x2E21\x2E23\x2E25\x2E27\x2E29\x3009\x3009\x300B"
          ~ "\x300D\x300F\x3011\x3015\x3017\x3019\x301B\x301E\xFE18\xFE36"
          ~ "\xFE38\xFE3A\xFE3C\xFE3E\xFE40\xFE42\xFE44\xFE48\xFE5A\xFE5C"
          ~ "\xFE5E\xFF09\xFF1E\xFF3D\xFF5D\xFF60\xFF63"
        ;
    }

    # Return closer character for given opener, or Nil if not valid opener
    method bracket-closer-for-opener(str $opener) {
        (my int $index := nqp::index(self.bracket-openers,$opener)) < 0
          ?? Nil
          !! nqp::substr(self.bracket-closers,$index,1)
    }

    # Return opener character for given closer, or Nil if not valid closer.
    # Note that some openers share the same closer: in that case the first
    # opener seen (with the lowest codepoint value) will be returned.
    method bracket-opener-for-closer(str $closer) {
        (my int $index := nqp::index(self.bracket-closers,$closer)) < 0
          ?? Nil
          !! nqp::substr(self.bracket-openers,$index,1)
    }

#-------------------------------------------------------------------------------
# Reserved operators

    # Return hash with reserved operator names and optional hints for errors
    method reserved-operators-lookup() {
        my constant RESERVED := nqp::hash(
          'infix:sym<=>',   "",
          'infix:sym<:=>',  "",
          'infix:sym<::=>', "",
          'infix:sym<~~>',  "(consider implementing an ACCEPTS method)",
          'prefix:sym<|>',  ""
        )
    }

    # Return whether given operator name is reserved
    method is-reserved-operator(str $operator) {
        nqp::existskey(self.reserved-operators-lookup,$operator)
    }

    # Return hint to be added to error message if reserved
    method reserved-operator-hint(str $operator) {
        nqp::atkey(self.reserved-operators-lookup,$operator)
    }

#-------------------------------------------------------------------------------
# Methods that can be run on both an instance and a type object.  Note that
# when called on a type object, an "empty" representation of that function
# will be returned.

    # Accessors
    method precedence()     { nqp::isconcrete(self) ?? $!precedence     !! "" }
    method sub-precedence() { nqp::isconcrete(self) ?? $!sub-precedence !! "" }
    method associative()    { nqp::isconcrete(self) ?? $!associative    !! "" }
    method thunky()         { nqp::isconcrete(self) ?? $!thunky         !! "" }
    method dba()            { nqp::isconcrete(self) ?? $!dba            !! "" }
    method next-term() {
        (nqp::isconcrete(self) ?? $!next-term !! "") || 'termish'
    }

    method iffy()    { nqp::isconcrete(self) ?? $!iffy    !! 0 }
    method diffy()   { nqp::isconcrete(self) ?? $!diffy   !! 0 }
    method fiddly()  { nqp::isconcrete(self) ?? $!fiddly  !! 0 }
    method adverb()  { nqp::isconcrete(self) ?? $!adverb  !! 0 }
    method ternary() { nqp::isconcrete(self) ?? $!ternary !! 0 }
    method commutative() { nqp::isconcrete(self) ?? $!commutative !! 0 }

    # Convenience methods
    method chain() {
        nqp::isconcrete(self) && $!associative eq 'chain'
    }
    method short-circuit() {
        nqp::isconcrete(self) && $!thunky ne ""
    }
    method sub-or-precedence() {
        nqp::isconcrete(self) ?? ($!sub-precedence || $!precedence) !! ""
    }

    # Return a string with the reason why it is not reducible, or empty
    # string
    method not-reducible() {
        nqp::isconcrete(self)
          ?? $!fiddly
            ?? "fiddly"
            !! $!diffy && $!associative ne 'chain'
              ?? "diffy and not chaining"
              !! ""
          !! "" # can't tell on a non-concrete object
    }

    # Return name of handler for reducing with these operator properties
    method reducer-name() {
        if nqp::isconcrete(self) {
            if nqp::iseq_s($!precedence,'f=') {
                '&METAOP_REDUCE_LISTINFIX'
            }
            else {
                my $associative := $!associative;
                nqp::chars($associative)
                  ?? nqp::iseq_s($associative,'chain')
                    ?? '&METAOP_REDUCE_CHAIN'
                    !! nqp::iseq_s($associative,'list')
                      ?? '&METAOP_REDUCE_LIST'
                      !! nqp::iseq_s($associative,'right')
                        ?? '&METAOP_REDUCE_RIGHT'
                        !! '&METAOP_REDUCE_LEFT'    # assume 'left' or 'non'
                  !! '&METAOP_REDUCE_LEFT'
            }
        }
        else {
            '&METAOP_REDUCE_LEFT'
        }
    }

    # Old interface method
    method prec(str $key?) {
        if nqp::isconcrete(self) {
            if $key {
                nqp::atkey(self.prec,$key)
            }
            else {
                my $hash := nqp::hash;
                nqp::bindkey($hash,'prec',$!precedence)    if $!precedence;
                nqp::bindkey($hash,'sub',$!sub-precedence) if $!sub-precedence;
                nqp::bindkey($hash,'assoc',$!associative)  if $!associative;
                nqp::bindkey($hash,'thunky',$!thunky)      if $!thunky;
                nqp::bindkey($hash,'dba',$!dba)            if $!dba;
                nqp::bindkey($hash,'nextterm',$!next-term) if $!next-term;
                nqp::bindkey($hash,'iffy',$!iffy)          if $!iffy;
                nqp::bindkey($hash,'diffy',$!diffy)        if $!diffy;
                nqp::bindkey($hash,'fiddly',$!fiddly)      if $!fiddly;
                nqp::bindkey($hash,'adverb',$!adverb)      if $!adverb;
                nqp::bindkey($hash,'ternary',$!ternary)    if $!ternary;
                nqp::bindkey($hash,'commutative',$!commutative) if $!commutative;
                $hash
            }
        }

        # called on type object
        else {
            if $key {
                $key eq 'prec' || $key eq 'assoc' || $key eq 'thunky'
                  ?? ""
                  !! $key eq 'iffy'
                    ?? 0
                    !! nqp::null
            }
            else {
                nqp::hash
            }
        }
    }

    # Informational method to check whether valid category
    method known-category(str $category) {
        my constant CATEGORIES := nqp::hash(
          'infix',1, 'prefix',1, 'postfix',1, 'postcircumfix',1, 'circumfix',1
        );
        nqp::existskey(CATEGORIES,$category)
    }

#-------------------------------------------------------------------------------
# Methods that return a new OperatorProperties object relative to the instance

    method equiv(str $associative) {
        nqp::isconcrete(self)
          ?? self.new(
               associative => nqp::isnull_s($associative)
                                ?? $!associative
                                !! $associative
             )
          !! self.new
    }
    method tighter(str $associative) {
        nqp::isconcrete(self)
          ?? self.new(
               precedence  => nqp::join('@=',nqp::split('=',$!precedence)),
               associative => nqp::isnull_s($associative)
                                ?? $!associative
                                !! $associative
             )
          !! nqp::die("No precedence found to be tighter for")
    }
    method looser(str $associative) {
        nqp::isconcrete(self)
          ?? self.new(
               precedence  => nqp::join(':=',nqp::split('=',$!precedence)),
               associative => nqp::isnull_s($associative)
                                ?? $!associative
                                !! $associative
             )
          !! nqp::die("No precedence found to be looser for")
    }

#-------------------------------------------------------------------------------
# The "properties-for-group" method is basically a hash of hashes, in which
# the values of the inner hash are named arguments of a call to .new.  The
# hash is initialized at compile time, but its values will change at runtime.
# So despite the "constant", the outer hash is *not* a constant as such.

# The first time the value for a group is requested, it will create an
# OperatorProperties object with the named arguments of the associated inner
# hash and replace the inner hash with that object.  This effectively caches
# the OperatorProperties object for that group.

# If there was no inner hash for the requested group, an empty hash will be
# assumed, and a "bare" instance of an OperatorProperties object will be
# cached for that.

    method properties-for-group(str $group) {
        my constant PROPERTIES := nqp::hash(

          'default-infix', nqp::hash(
            'precedence','t=', 'associative', 'left'
          ),
          'default-prefix', nqp::hash(
            'precedence','v=', 'associative', 'unary'
          ),
          'default-postfix', nqp::hash(
            'precedence','x=', 'associative', 'unary'
          ),
          'default-postcircumfix', nqp::hash(
            'precedence','y='
           ),
          'default-circumfix', nqp::hash(),
          'methodcall', nqp::hash(
            'precedence','y=', 'associative','unary', 'fiddly', 1
          ),
          'autoincrement', nqp::hash(
            'precedence','x=', 'associative','unary'
          ),
          'exponentiation', nqp::hash(
            'precedence','w=', 'associative','right'
          ),
          'symbolic-unary', nqp::hash(
            'precedence','v=', 'associative','unary'
          ),
          'dotty-infix', nqp::hash(
            'precedence','v=', 'associative','left', 'next-term','dottyopish',
            'sub-precedence','z=', 'fiddly',1
          ),
          'multiplicative', nqp::hash(
            'precedence','u=', 'associative','left'
          ),
          'multiplicative-iffy', nqp::hash(
            'precedence','u=', 'associative','left', 'iffy',1
          ),
          'additive', nqp::hash(
            'precedence','t=', 'associative','left'
          ),
          'additive-iffy', nqp::hash(
            'precedence','t=', 'associative','left', 'iffy',1
          ),
          'replication-x', nqp::hash(
            'precedence','s=', 'associative','left'
          ),
          'replication-xx', nqp::hash(
            'precedence','s=', 'associative','left', 'thunky','t.'
          ),
          'concatenation', nqp::hash(
            'precedence','r=', 'associative','left'
          ),
          'junctive-and', nqp::hash(
            'precedence','q=', 'associative','list'
          ),
          'junctive-and-iffy', nqp::hash(
            'precedence','q=', 'associative','list', 'iffy',1
          ),
          'junctive-or', nqp::hash(
            'precedence','p=', 'associative','list'
          ),
          'junctive-or-iffy', nqp::hash(
            'precedence','p=', 'associative','list', 'iffy',1
          ),
          'named-unary', nqp::hash(
            'precedence','o=', 'associative','unary'
          ),
          'structural', nqp::hash(
            'precedence','n=', 'associative','non', 'diffy',1
          ),
          'chaining', nqp::hash(
            'precedence','m=', 'associative','chain', 'iffy',1, 'diffy',1
          ),
          'chaining-commutative', nqp::hash(
            'precedence','m=', 'associative','chain', 'iffy',1, 'diffy',1, 'commutative',1
          ),
          'tight-and', nqp::hash(
            'precedence','l=', 'associative','left', 'thunky','.t', 'iffy',1
          ),
          'tight-or', nqp::hash(
            'precedence','k=', 'associative','left', 'thunky','.t', 'iffy',1
          ),
          'tight-defor', nqp::hash(
            'precedence','k=', 'associative','left', 'thunky','.t'
          ),
          'tight-xor', nqp::hash(
            'precedence','k=', 'associative','list', 'thunky','..t', 'iffy',1
          ),
          'tight-minmax', nqp::hash(
            'precedence','k=', 'associative','list'
          ),
          'ternary', nqp::hash(
            'precedence','j=', 'associative','right', 'thunky','.tt',
            'fiddly',1, 'ternary',1
          ),
          'conditional-ff', nqp::hash(
            'precedence','j=', 'associative','right', 'thunky','tt',
            'iffy',1
          ),
          'item-assignment', nqp::hash(
            'precedence','i=', 'associative','right'
          ),
          'adverb', nqp::hash(
            'precedence','i=', 'associative','unary', 'adverb',1
          ),
          'list assignment', nqp::hash(
            'precedence','i=', 'associative','right', 'fiddly',1,
            'sub-precedence','e='
          ),
          'loose-unary', nqp::hash(
            'precedence','h=', 'associative','unary'
          ),
          'comma', nqp::hash(
            'precedence','g=', 'associative','list', 'next-term','nulltermish'
          ),
          'list-infix', nqp::hash(
            'precedence','f=', 'associative','list'
          ),
          'list-prefix', nqp::hash(
            'precedence','e=', 'associative','right'
          ),
          'loose-and', nqp::hash(
            'precedence','d=', 'associative','left', 'thunky','.t', 'iffy',1
          ),
          'loose-andthen', nqp::hash(
            'precedence','d=', 'associative','list', 'thunky','.b'
          ),
          'loose-or', nqp::hash(
            'precedence','c=', 'associative','left', 'thunky','.t', 'iffy',1
          ),
          'loose-xor', nqp::hash(
            'precedence','c=', 'associative','list', 'thunky','.t', 'iffy',1
          ),
          'loose-orelse', nqp::hash(
            'precedence','c=', 'associative','list', 'thunky','.b'
          ),
          'sequencer', nqp::hash(
            'precedence','b=', 'associative','list'
          )
        );

        # Convert raw properties into an object
        my %value := nqp::ifnull(
          nqp::atkey(PROPERTIES,$group),
          nqp::hash()
        );
        nqp::ishash(%value)
          ?? nqp::bindkey(  # first time
               PROPERTIES,
               $group,
               OperatorProperties.new(|%value, :dba($group))
             )
          !! %value         # already instantiated
    }

#-------------------------------------------------------------------------------
# Each lookup method contains a constant hash with the BuiltinOperatorTypes
# names as keys, and an empty string as value (at compile time).  At runtime,
# when needed, these empty strings will be replaced by instantiated
# OperatorProperties objects that can be returned and inspected.

    # Helper method for producing a OperatorProperties object
    # given a hash with group names keyed to an operator name.
    method produce($hash, str $key) {

        # Get current setting or default
        my $properties := nqp::ifnull(
          nqp::atkey($hash,$key),
          nqp::atkey($hash,$key := '')
        );

        # If not yet an instance, create an instance and update in the
        # hash if the operator is a built-in.
        if nqp::isstr($properties) {
            $properties := self.properties-for-group($properties);
            nqp::bindkey($hash,$key,$properties)
              if nqp::existskey($hash,$key);
        }

        $properties
    }

    # Lookup properties of an infix operator
    method infix(str $operator?) {
        my constant PROPERTIES := nqp::hash(
           '', 'default-infix',

           '.',  'dotty-infix',
           '.=', 'dotty-infix',

          '*',   'multiplicative',
          '×',   'multiplicative',
          '/',   'multiplicative',
          'div', 'multiplicative',
          'gcd', 'multiplicative',
          'lcm', 'multiplicative',
          '%',   'multiplicative',
          'mod', 'multiplicative',
          '+&',  'multiplicative',
          '~&',  'multiplicative',
          '+<',  'multiplicative',
          '+>',  'multiplicative',
          '~<',  'multiplicative',
          '~>',  'multiplicative',

          '%%', 'multiplicative-iffy',
          '?&', 'multiplicative-iffy',

          '+',  'additive',
          '-',  'additive',
          '−',  'additive',
          '+|', 'additive',
          '+^', 'additive',
          '~|', 'additive',
          '~^', 'additive',

          '?|', 'additive-iffy',
          '?^', 'additive-iffy',

          '**', 'exponentiation',

          'x', 'replication-x',

          'xx', 'replication-xx',

          '~', 'concatenation',

          '(&)', 'junctive-and',
          '∩',   'junctive-and',
          '(.)', 'junctive-and',
          '⊍',   'junctive-and',

          '&',   'junctive-and-iffy',

          '(+)', 'junctive-or',
          '⊎',   'junctive-or',
          '(|)', 'junctive-or',
          '∪',   'junctive-or',
          '(-)', 'junctive-or',
          '∖',   'junctive-or',
          '(^)', 'junctive-or',
          '⊖',   'junctive-or',

          '|', 'junctive-or-iffy',
          '^', 'junctive-or-iffy',

          '..',     'structural',
          '^..',    'structural',
          '..^',    'structural',
          '^..^',   'structural',
          '<=>',    'structural',
          'leg',    'structural',
          'cmp',    'structural',
          'unicmp', 'structural',
          'coll',   'structural',
          'but',    'structural',
          'does',   'structural',

          '>',      'chaining',
          '<',      'chaining',
          '>=',     'chaining',
          '<=',     'chaining',
          '(<)',    'chaining',
          '⊂',      'chaining',
          '(>)',    'chaining',
          '⊃',      'chaining',
          '(<=)',   'chaining',
          '⊆',      'chaining',
          '(>=)',   'chaining',
          '⊇',      'chaining',
          '(<+)',   'chaining',
          '≼',      'chaining',
          '(>+)',   'chaining',
          '≽',      'chaining',
          '==',     'chaining-commutative',
          '=~=',    'chaining-commutative',
          '≅',      'chaining-commutative',
          '!=',     'chaining-commutative',
          'eq',     'chaining-commutative',
          'ne',     'chaining-commutative',
          'le',     'chaining',
          'ge',     'chaining',
          'lt',     'chaining',
          'gt',     'chaining',
          '=:=',    'chaining',
          '===',    'chaining-commutative',
          'eqv',    'chaining-commutative',
          'before', 'chaining',
          'after',  'chaining',
          '~~',     'chaining',
          '!~',     'chaining',    # dummy for p5ism
          '!~~',    'chaining',
          '(elem)', 'chaining',
          '∈',      'chaining',
          '(cont)', 'chaining',
          '∋',      'chaining',
          '(==)',   'chaining',
          '≡',      'chaining',
          '∉',      'chaining',
          '∌',      'chaining',
          '⊄',      'chaining',
          '⊅',      'chaining',
          '≢',      'chaining',
          '⊈',      'chaining',
          '⊉',      'chaining',

          '&&', 'tight-and',

          '||', 'tight-or',

          '//', 'tight-defor',

          '^^', 'tight-xor',

          'min', 'tight-minmax',
          'max', 'tight-minmax',

          '?? !!', 'ternary',        # dummy for $a ?? 42 !! 666

          'ff',    'conditional-ff',
          '^ff',   'conditional-ff',
          'ff^',   'conditional-ff',
          '^ff^',  'conditional-ff',
          'fff',   'conditional-ff',
          '^fff',  'conditional-ff',
          'fff^',  'conditional-ff',
          '^fff^', 'conditional-ff',

          '=~',  'item-assignment',  # alt for = ~foo
          '$=',  'item-assignment',  # dummy for $a = 42
          '=>',  'item-assignment',
          '⇒',   'item-assignment',
          '⚛=',  'item-assignment',
          '⚛+=', 'item-assignment',
          '⚛-=', 'item-assignment',
          '⚛−=', 'item-assignment',

          '=',  'list assignment',
          '@=', 'list assignment',   # dummy for @a = 1,2,3
          ':=', 'list assignment',
          '≔',  'list assignment',

          ':', 'comma',              # dummy for chop 42:
          ',', 'comma',

          'X',      'list-infix',
          'Z',      'list-infix',
          '...',    'list-infix',
          '…',      'list-infix',
          '...^',   'list-infix',
          '…^',     'list-infix',
          '^...',   'list-infix',
          '^…',     'list-infix',
          '^...^',  'list-infix',
          '^…^',    'list-infix',
          'minmax', 'list-infix',

          'and', 'loose-and',

          'andthen',    'loose-andthen',
          'notandthen', 'loose-andthen',

          'or',     'loose-or',

          'xor',    'loose-xor',

          'orelse', 'loose-orelse',

          # dummy, for parsing Raku code only
          '==>',  'sequencer',
          '==>>', 'sequencer',
          '<==',  'sequencer',
          '<<==', 'sequencer',
        );

        self.produce(PROPERTIES, $operator // '')
    }

    # Lookup properties of a prefix operator
    method prefix(str $operator?) {
        my constant PROPERTIES := nqp::hash(
           '', 'default-prefix',

          '++',  'autoincrement',
          '--',  'autoincrement',
          '++⚛', 'autoincrement',
          '--⚛', 'autoincrement',

          '+',  'symbolic-unary',
          '~',  'symbolic-unary',
          '-',  'symbolic-unary',
          '?',  'symbolic-unary',
          '!',  'symbolic-unary',
          '|',  'symbolic-unary',
          '+^', 'symbolic-unary',
          '~^', 'symbolic-unary',
          '?^', 'symbolic-unary',
          '^',  'symbolic-unary',
          '⚛',  'symbolic-unary',
          '//', 'symbolic-unary',

          'so',  'loose-unary',
          'not', 'loose-unary',
        );

        self.produce(PROPERTIES, $operator // '')
    }

    # Lookup properties of a postfix operator
    method postfix(str $operator?) {
        my constant PROPERTIES := nqp::hash(
           '', 'default-postfix',

          ':', 'adverb',          # dummy for %h<a>:exists

          '()', 'methodcall',     # term()
          '.',  'methodcall',
          '!',  'methodcall',
          '.^', 'methodcall',
          '.?', 'methodcall',
          '.&', 'methodcall',
          '.=', 'methodcall',
          'i',  'methodcall',

          'ⁿ',   'autoincrement',  # power
          '+',   'autoincrement',  # vulgar

          '++',  'autoincrement',
          '--',  'autoincrement',
          '⚛++', 'autoincrement',
          '⚛--', 'autoincrement',
        );

        self.produce(PROPERTIES, $operator // '')
    }

    # Lookup properties of a postcircumfix operator
    method postcircumfix(str $operator?) {
        my constant PROPERTIES := nqp::hash(
           '', 'default-postcircumfix',

          '< >',   'methodcall',
          '<< >>', 'methodcall',
          '« »',   'methodcall',
          '[ ]',   'methodcall',
          '[; ]',  'methodcall',
          '{ }',   'methodcall',
          '{; }',  'methodcall',
        );

        self.produce(PROPERTIES, $operator // '')
    }

    # Lookup properties of a circumfix operator
    method circumfix(str $operator?) {
        my constant PROPERTIES := nqp::hash(
           '', 'default-circumfix',

          '< >',   'methodcall',
          '<< >>', 'methodcall',
          '« »',   'methodcall',
          '[ ]',   'methodcall',
          '{ }',   'methodcall',
        );

        self.produce(PROPERTIES, $operator // '')
    }
}

# vim: expandtab shiftwidth=4
