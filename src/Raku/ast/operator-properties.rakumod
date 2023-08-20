#-------------------------------------------------------------------------------
# The OperatorProperties class attempts to encapsulate all possible information
# about all types of operators in the Raku Programming Language.

# Historically, this information was represented by a number of internal
# hashes that lived in the grammar.

# The OperatorProperties object is a value type: this means that it cannot
# be mutated.  Creating a derived OperatorProperties object from an existing
# one *is* possible by calling .new on an OperatorProperties instance, and
# only specifying the arguments that you want to have changed.

class OperatorProperties {
    has str $.precedence;
    has str $.associative;
    has str $.thunky;
    has int $.iffy;
    has int $.diffy;
    has int $.fiddly;

    # grammar specific attributes
    has str $.dba;
    has str $.next-term;
    has str $.sub-precedence;

    # Basic interface
    method new(
      str :$precedence,
      str :$associative,
      str :$thunky,
      int :$iffy,
      int :$diffy,
      int :$fiddly,
      str :$dba,
      str :$next-term,
      str :$sub-precedence
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj,OperatorProperties,'$!precedence',
          $precedence // (nqp::isconcrete(self) ?? $!precedence !! ""));
        nqp::bindattr_s($obj,OperatorProperties,'$!associative',
          $associative // (nqp::isconcrete(self) ?? $!associative !! ""));
        nqp::bindattr_s($obj,OperatorProperties,'$!thunky',
          $thunky // (nqp::isconcrete(self) ?? $!thunky !! ""));
        nqp::bindattr_i($obj,OperatorProperties,'$!iffy',
          $iffy // (nqp::isconcrete(self) ?? $!iffy !! 0));
        nqp::bindattr_i($obj,OperatorProperties,'$!diffy',
          $diffy // (nqp::isconcrete(self) ?? $!diffy !! 0));
        nqp::bindattr_i($obj,OperatorProperties,'$!fiddly',
          $fiddly // (nqp::isconcrete(self) ?? $!fiddly !! 0));

        nqp::bindattr_s($obj,OperatorProperties,'$!dba',
          $dba // (nqp::isconcrete(self) ?? $!dba !! ""));
        nqp::bindattr_s($obj,OperatorProperties,'$!next-term',
          $next-term // (nqp::isconcrete(self) ?? $!next-term !! ""));
        nqp::bindattr_s($obj,OperatorProperties,'$!sub-precedence',
          $sub-precedence // (nqp::isconcrete(self) ?? $!sub-precedence !! ""));
        $obj
    }

    # Instantiate using an old-style %prec hash
    method new-compat(
      str :$prec,
      str :$assoc,
      str :$thunky,
      int :$iffy,
      int :$diffy,
      int :$fiddly,
      str :$dba,
      str :$nextterm,
      str :$sub,
      *%_
    ) {
        self.new(
          :precedence($prec),
          :associative($assoc),
          :$thunky,
          :$iffy,
          :$diffy,
          :$fiddly,
          :$dba,
          :next-term($nextterm),
          :sub-precedence($sub)
        )
    }

    # OperatorProperties for an unknown operator
    method default-infix-operator()         { self.new(:precedence<t=>) }
    method default-prefix-operator()        { self.new(:precedence<v=>) }
    method default-postfix-operator()       { self.new(:precedence<x=>) }
    method default-postcircumfix-operator() { self.new(:precedence<y=>) }

    # Accessors
    method precedence()     { nqp::isconcrete(self) ?? $!precedence     !! "" }
    method associative()    { nqp::isconcrete(self) ?? $!associative    !! "" }
    method thunky()         { nqp::isconcrete(self) ?? $!thunky         !! "" }
    method iffy()           { nqp::isconcrete(self) ?? $!iffy           !! 0  }
    method diffy()          { nqp::isconcrete(self) ?? $!diffy          !! 0  }
    method fiddly()         { nqp::isconcrete(self) ?? $!fiddly         !! 0  }
    method dba()            { nqp::isconcrete(self) ?? $!dba            !! "" }
    method next-term()      { nqp::isconcrete(self) ?? $!next-term      !! "" }
    method sub-precedence() { nqp::isconcrete(self) ?? $!sub-precedence !! "" }

    # Convenience methods
    method chaining() {
        nqp::isconcrete(self)
          && ($!associative eq 'chaining' || $!associative eq 'chain')
    }
    method short-circuit() {
        nqp::isconcrete(self) && $!thunky ne ""
    }

    # Return OperatorProperties depending on other properties
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

    # Return name of handler for reducing with these operator properties
    method reducer-name() {
        nqp::isconcrete(self)
          ?? nqp::iseq_s($!precedence,'f=')
            ?? '&METAOP_REDUCE_LISTINFIX'
            !! nqp::chars($!associative)
              ?? nqp::iseq_s($!associative,'chain')
                ?? '&METAOP_REDUCE_CHAIN'
                !! nqp::iseq_s($!associative,'list')
                  ?? '&METAOP_REDUCE_LIST'
                  !! nqp::iseq_s($!associative,'right')
                    ?? '&METAOP_REDUCE_RIGHT'
                    !! '&METAOP_REDUCE_LEFT'    # assume 'left' or 'non'
              !! '&METAOP_REDUCE_LEFT'
          !! '&METAOP_REDUCE_LEFT'
    }

    # Old interface method
    method prec(str $key?) {
        if nqp::isconcrete(self) {
            if $key {
                $key eq 'prec'
                  ?? $!precedence
                  !! $key eq 'assoc'
                    ?? $!associative
                    !! $key eq 'thunky'
                      ?? $!thunky
                      !! $key eq 'iffy'
                        ?? $!iffy
                        !! nqp::null
            }
            else {
                my $hash := nqp::hash;
                nqp::bindkey($hash,'prec',  $!precedence)  if $!precedence;
                nqp::bindkey($hash,'assoc', $!associative) if $!associative;
                nqp::bindkey($hash,'thunky',$!thunky)      if $!thunky;
                nqp::bindkey($hash,'iffy',  $!iffy)        if $!iffy;
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

    # Lookup methods for operator types
    method properties-for-infix(str $op) {
        BuiltinOperatorProperties.infix($op)
    }
    method properties-for-prefix(str $op) {
        BuiltinOperatorProperties.prefix($op)
    }
    method properties-for-postfix(str $op) {
        BuiltinOperatorProperties.postfix($op)
    }
    method properties-for-postcircumfix(str $op) {
        BuiltinOperatorProperties.postcircumfix($op)
    }
}

#-------------------------------------------------------------------------------
# The BuiltinOperatorTypes class is basically a hash of hashes, represented
# by its methods (apart from the lookup method).  The reason for this approach
# is that the information in here will be needed very early during compilation
# of Raku code, specifically during the setting compilation.

# So it is impossible to create a constant hash with instantiated
# OperatorProperties instances at compile time.  But it is possible to create
# methods with the appropriate arguments to create the intended
# OperatorProperties instance.

# The keys of the hash represent a combination of operator properties that
# are shared by one or more operators.  The names of the keys are arbitrary
# and historical, but should represent the types of operators in some way.

# The "lookup" method contains a constant hash with the method names as
# keys, and an empty string as value (at compile time).  At runtime, when
# needed, these empty strings will be replaced by instantiated
# OperatorProperties objects that can be returned and inspected.

class BuiltinOperatorTypes {
    method methodcall() {
        OperatorProperties.new:
          :precedence<y=>, :associative<unary>, :dba<methodcall>, :fiddly
    }
    method autoincrement() {
        OperatorProperties.new:
          :precedence<x=>, :associative<unary>, :dba<autoincrement>
    }
    method exponentiation() {
        OperatorProperties.new:
          :precedence<w=>, :associative<right>, :dba<exponentiation>
    }
    method symbolic-unary() {
        OperatorProperties.new:
          :precedence<v=>, :associative<unary>, :dba('symbolic unary')
    }
    method dotty-infix() {
        OperatorProperties.new:
          :precedence<v=>, :associative<left>, :dba('dotty infix'),
          :fiddly, :next-term<dottyopish>, :sub-precedence<z=>
    }
    method multiplicative() {
        OperatorProperties.new:
          :precedence<u=>, :associative<left>, :dba<multiplicative>
    }
    method multiplicative-iffy() {
        OperatorProperties.new:
          :precedence<u=>, :associative<left>, :dba<multiplicative>, :iffy
    }
    method additive() {
        OperatorProperties.new:
          :precedence<t=>, :associative<left>, :dba<additive>
    }
    method additive-iffy() {
        OperatorProperties.new:
          :precedence<t=>, :associative<left>, :dba<additive>, :iffy
    }
    method replication() {
        OperatorProperties.new:
          :precedence<s=>, :associative<left>, :dba<replication>
    }
    method replication-xx() {
        OperatorProperties.new:
          :precedence<s=>, :associative<left>, :dba<replication>, :thunky<t.>
    }
    method concatenation() {
        OperatorProperties.new:
          :precedence<r=>, :associative<list>, :dba<replication>
    }
    method junctive-and() {
        OperatorProperties.new:
          :precedence<q=>, :associative<list>, :dba('junctive and')
    }
    method junctive-and-iffy() {
        OperatorProperties.new:
          :precedence<q=>, :associative<list>, :dba('junctive and'), :iffy
    }
    method junctive-or() {
        OperatorProperties.new:
          :precedence<p=>, :associative<list>, :dba('junctive or')
    }
    method junctive-or-iffy() {
        OperatorProperties.new:
          :precedence<p=>, :associative<list>, :dba('junctive or'), :iffy
    }
    method named-unary() {
        OperatorProperties.new:
          :precedence<o=>, :associative<unary>, :dba('named unary')
    }
    method structural() {
        OperatorProperties.new:
          :precedence<n=>, :associative<non>, :dba<structural>, :diffy
    }
    method chaining() {
        OperatorProperties.new:
          :precedence<m=>, :associative<chain>, :dba<chaining>,
          :iffy, :diffy
    }
    method tight-and() {
        OperatorProperties.new:
          :precedence<l=>, :associative<list>, :dba('tight and'),
          :thunky<.t>, :iffy
    }
    method tight-or() {
        OperatorProperties.new:
          :precedence<k=>, :associative<list>, :dba('tight or'), :thunky<.t>
    }
    method tight-or-defor() {
        OperatorProperties.new:
          :precedence<k=>, :associative<left>, :dba('tight or'), :thunky<.t>
    }
    method tight-or-xor() {
        OperatorProperties.new:
          :precedence<k=>, :associative<list>, :dba('tight or'),
          :thunky<..t>, :iffy
    }
    method tight-or-minmax() {
        OperatorProperties.new:
          :precedence<k=>, :associative<list>, :dba('tight or')
    }
    method conditional() {
        OperatorProperties.new:
          :precedence<j=>, :associative<right>, :dba<conditional>,
          :thunky<.tt>, :iffy
    }
    method conditional-ff() {
        OperatorProperties.new:
          :precedence<j=>, :associative<right>, :dba<conditional>,
          :thunky<tt>, :iffy
    }
    method item-assignment() {
        OperatorProperties.new:
          :precedence<i=>, :associative<right>, :dba('item assignment')
    }
    method list-assignment() {
        OperatorProperties.new:
          :precedence<i=>, :associative<right>, :dba('list assignment'),
          :fiddly, :sub-precedence<e=>
    }
    method loose-unary() {
        OperatorProperties.new:
          :precedence<h=>, :associative<unary>, :dba('loose unary')
    }
    method comma() {
        OperatorProperties.new:
          :precedence<g=>, :associative<list>, :dba<comma>, :fiddly,
          :next-term<nulltermish>
    }
    method list-infix() {
        OperatorProperties.new:
          :precedence<f=>, :associative<list>, :dba('list infix')
    }
    method list-prefix() {
        OperatorProperties.new:
          :precedence<e=>, :associative<right>, :dba('list prefix')
    }
    method loose-and() {
        OperatorProperties.new:
          :precedence<d=>, :associative<left>, :dba('loose and'), :thunky<.t>
    }
    method loose-andthen() {
        OperatorProperties.new:
          :precedence<d=>, :associative<left>, :dba('loose and'), :thunky<.b>
    }
    method loose-or() {
        OperatorProperties.new:
          :precedence<c=>, :associative<left>, :dba('loose or'), :thunky<.t>,
          :iffy
    }
    method loose-or-xor() {
        OperatorProperties.new:
          :precedence<c=>, :associative<list>, :dba('loose or'), :thunky<.t>,
          :iffy
    }
    method loose-orelse() {
        OperatorProperties.new:
          :precedence<c=>, :associative<list>, :dba('loose or'), :thunky<.b>
    }
    method sequencer() {
        OperatorProperties.new:
          :precedence<b=>, :associative<list>, :dba<sequencer>
    }

    method lookup(str $type) {
        my constant PROPERTIES := nqp::hash(
          'methodcall',          '',
          'autoincrement',       '',
          'exponentiation',      '',
          'symbolic-unary',      '',
          'multiplicative',      '',
          'multiplicative-iffy', '',
          'additive',            '',
          'additive-iffy',       '',
          'replication',         '',
          'replication-xx',      '',
          'concatenation',       '',
          'junctive-and',        '',
          'junctive-and-iffy',   '',
          'junctive-or',         '',
          'junctive-or-iffy',    '',
          'structural',          '',
          'named-unary',         '',
          'chaining',            '',
          'tight-and',           '',
          'tight-or',            '',
          'tight-or-defor',      '',
          'tight-or-xor',        '',
          'tight-or-minmax',     '',
          'conditional',         '',
          'conditional-ff',      '',
          'item-assignment',     '',
          'loose-unary',         '',
          'comma',               '',
          'list-infix',          '',
          'list-prefix',         '',
          'loose-and',           '',
          'loose-andthen',       '',
          'loose-or',            '',
          'loose-or-xor',        '',
          'loose-orelse',        '',
          'sequencer',           '',
        );

        # Return instance for given operator type, or create and bind
        # one if this is the first time this operator type is being
        # used.
        (my $properties := nqp::atkey(PROPERTIES,$type))
          ?? $properties
          !! nqp::bindkey(PROPERTIES,$type,self."$type"())
    }
}

#-------------------------------------------------------------------------------
# The BuiltinOperatorProperties class contains methods for operator lookups,
# one lookup method for each operator type (infix, prefix, postfix, and
# postcircumfix), each of which contains a lookup hash with the operator
# string as the key, and the BuiltinOperatorType name as its value.  The
# reason for this approach is that the information in here will be needed
# very early during compilation of of Raku code, specifically during the
# setting compilation.

# Each lookup method contains a constant hash with the BuiltinOperatorTypes
# names as keys, and an empty string as value (at compile time).  At runtime,
# when needed, these empty strings will be replaced by instantiated
# OperatorProperties objects that can be returned and inspected.

class BuiltinOperatorProperties {

    # Lookup properties of an infix operator
    method infix(str $operator) {
        my constant PROPERTIES := nqp::hash(

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

          'x', 'replication',

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
          '==',     'chaining',
          '=~=',    'chaining',
          '≅',      'chaining',
          '!=',     'chaining',
          'eq',     'chaining',
          'ne',     'chaining',
          'le',     'chaining',
          'ge',     'chaining',
          'lt',     'chaining',
          'gt',     'chaining',
          '=:=',    'chaining',
          '===',    'chaining',
          'eqv',    'chaining',
          'before', 'chaining',
          'after',  'chaining',
          '~~',     'chaining',
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

          '//', 'tight-or-defor',

          '^^', 'tight-or-xor',

          'min', 'tight-or-minmax',
          'max', 'tight-or-minmax',

          'ff',    'conditional-ff',
          '^ff',   'conditional-ff',
          'ff^',   'conditional-ff',
          '^ff^',  'conditional-ff',
          'fff',   'conditional-ff',
          '^fff',  'conditional-ff',
          'fff^',  'conditional-ff',
          '^fff^', 'conditional-ff',

          '=>',  'item-assignment',
          '⇒',   'item-assignment',
          '⚛=',  'item-assignment',
          '⚛+=', 'item-assignment',
          '⚛-=', 'item-assignment',
          '⚛−=', 'item-assignment',

          ':=', 'list-assignment',

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

          '=',   'list-prefix',

          'and', 'loose-and',

          'andthen',    'loose-andthen',
          'notandthen', 'loose-andthen',

          'or',     'loose-or',

          'xor',    'loose-or-xor',

          'orelse', 'loose-orelse',
        );

        # Return instance for the given infix operator, or create and
        # bind one if this is the first time this operator is being used.
        nqp::isstr(my $properties := nqp::atkey(PROPERTIES,$operator))
          ?? nqp::bindkey(PROPERTIES,$operator,
               BuiltinOperatorTypes.lookup($properties))
          !! nqp::ifnull($properties, Nil)
    }

    # Lookup properties of a prefix operator
    method prefix(str $operator) {
        my constant PROPERTIES := nqp::hash(

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

          'so',  'loose-unary',
          'not', 'loose-unary',
        );

        # Return instance for the given prefix operator, or create and
        # bind one if this is the first time this operator is being used.
        nqp::isstr(my $properties := nqp::atkey(PROPERTIES,$operator))
          ?? nqp::bindkey(PROPERTIES,$operator,
               nqp::clone(BuiltinOperatorTypes.lookup($properties)))
          !! nqp::ifnull($properties, Nil)
    }

    # Lookup properties of a postfix operator
    method postfix(str $operator) {
        my constant PROPERTIES := nqp::hash(

          'i', 'methodcall',

          '++',  'autoincrement',
          '--',  'autoincrement',
          '⚛++', 'autoincrement',
          '⚛--', 'autoincrement',
        );

        # Return instance for the given postfix operator, or create and
        # bind one if this is the first time this operator is being used.
        nqp::isstr(my $properties := nqp::atkey(PROPERTIES,$operator))
          ?? nqp::bindkey(PROPERTIES,$operator,
               nqp::clone(BuiltinOperatorTypes.lookup($properties)))
          !! nqp::ifnull($properties, Nil)
    }

    # Lookup properties of a postcircumfix operator
    method postcircumfix(str $operator) {
        my constant PROPERTIES := nqp::hash(

          '[ ]', 'methodcall',
          '{ }', 'methodcall',
        );

        # Return instance for the given circumpostfix operator, or create and
        # bind one if this is the first time this operator is being used.
        nqp::isstr(my $properties := nqp::atkey(PROPERTIES,$operator))
          ?? nqp::bindkey(PROPERTIES,$operator,
               nqp::clone(BuiltinOperatorTypes.lookup($properties)))
          !! nqp::ifnull($properties, Nil)
    }
}

# vim: expandtab shiftwidth=4
