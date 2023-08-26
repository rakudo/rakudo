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

    has int $.iffy;
    has int $.diffy;
    has int $.fiddly;
    has int $.dottyopish;
    has int $.nulltermish;

    # Basic interface
    method new(
      str :$precedence,
      str :$sub-precedence,
      str :$associative,
      str :$thunky,
      str :$dba,
      int :$iffy,
      int :$diffy,
      int :$fiddly,
      int :$dottyopish,
      int :$nulltermish
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

        nqp::bindattr_i($obj,OperatorProperties,'$!iffy',
          $iffy // (nqp::isconcrete(self) ?? $!iffy !! 0));
        nqp::bindattr_i($obj,OperatorProperties,'$!diffy',
          $diffy // (nqp::isconcrete(self) ?? $!diffy !! 0));
        nqp::bindattr_i($obj,OperatorProperties,'$!fiddly',
          $fiddly // (nqp::isconcrete(self) ?? $!fiddly !! 0));
        nqp::bindattr_i($obj,OperatorProperties,'$!dottyopish',
          $dottyopish // (nqp::isconcrete(self) ?? $!dottyopish !! 0));
        nqp::bindattr_i($obj,OperatorProperties,'$!nulltermish',
          $nulltermish // (nqp::isconcrete(self) ?? $!nulltermish !! 0));

        $obj
    }

    # Instantiate using an old-style %prec hash
    method new-compat(
      str :$prec,
      str :$sub,
      str :$assoc,
      str :$thunky,
      str :$dba,
      int :$iffy,
      int :$diffy,
      int :$fiddly,
      str :$nextterm,
      *%_
    ) {
        my int $dottyopish  := $nextterm eq 'dottyopish';
        my int $nulltermish := $nextterm eq 'nulltermish';
        self.new(
          :precedence($prec),
          :associative($assoc),
          :$thunky,
          :$iffy,
          :$diffy,
          :$fiddly,
          :$dba,
          :$dottyopish,
          :$nulltermish,
          :sub-precedence($sub)
        )
    }

#-------------------------------------------------------------------------------
# Methods that can be run on both an instance and a type object.  Note that
# when calle on a type object, an "empty" representation of that function
# will be returned.

    # Accessors
    method precedence()     { nqp::isconcrete(self) ?? $!precedence     !! "" }
    method sub-precedence() { nqp::isconcrete(self) ?? $!sub-precedence !! "" }
    method associative()    { nqp::isconcrete(self) ?? $!associative    !! "" }
    method thunky()         { nqp::isconcrete(self) ?? $!thunky         !! "" }
    method dba()            { nqp::isconcrete(self) ?? $!dba            !! "" }

    method iffy()        { nqp::isconcrete(self) ?? $!iffy        !! 0  }
    method diffy()       { nqp::isconcrete(self) ?? $!diffy       !! 0  }
    method fiddly()      { nqp::isconcrete(self) ?? $!fiddly      !! 0  }
    method dottyopish()  { nqp::isconcrete(self) ?? $!dottyopish  !! 0  }
    method nulltermish() { nqp::isconcrete(self) ?? $!nulltermish !! 0  }

    # Convenience methods
    method chaining() {
        nqp::isconcrete(self)
          && ($!associative eq 'chaining' || $!associative eq 'chain')
    }
    method short-circuit() {
        nqp::isconcrete(self) && $!thunky ne ""
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
                       || nqp::iseq_s($associative,'chaining')
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
                $key eq 'prec'
                  ?? $!precedence
                  !! $key eq 'assoc'
                    ?? $!associative
                    !! $key eq 'thunky'
                      ?? $!thunky
                      !! $key eq 'iffy'
                        ?? $!iffy
                        !! $key eq 'nextterm'
                          ?? $!dottyopish
                            ?? 'dottyopish'
                            !! $!nulltermish
                              ?? 'nulltermish'
                              !! nqp::null
                          !! nqp::null
            }
            else {
                my $hash := nqp::hash;
                nqp::bindkey($hash,'prec',   $!precedence)   if $!precedence;
                nqp::bindkey($hash,'assoc',  $!associative)  if $!associative;
                nqp::bindkey($hash,'thunky', $!thunky)       if $!thunky;
                nqp::bindkey($hash,'iffy',   $!iffy)         if $!iffy;
                nqp::bindkey($hash,'nextterm','dottyopish')  if $!dottyopish;
                nqp::bindkey($hash,'nextterm','nulltermish') if $!nulltermish;
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

          'default-infix',         nqp::hash('precedence','t='),
          'default-prefix',        nqp::hash('precedence','v='),
          'default-postfix',       nqp::hash('precedence','x='),
          'default-postcircumfix', nqp::hash('precedence','y='),
          'default-circumfix',     nqp::hash(),

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
            'precedence','v=', 'associative','left', 'fiddly',1, 'dottyopish',1,
            'sub-precedence','z='
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
          'tight-and', nqp::hash(
            'precedence','l=', 'associative','list', 'thunky','.t', 'iffy',1
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
          'conditional', nqp::hash(
            'precedence','j=', 'associative','right', 'thunky','.tt', 'iffy',1
          ),
          'conditional-ff', nqp::hash(
            'precedence','j=', 'associative','right', 'thunky','tt', 'iffy',1
          ),
          'item-assignment', nqp::hash(
            'precedence','i=', 'associative','right'
          ),
          'list-assignment', nqp::hash(
            'precedence','i=', 'associative','right', 'fiddly',1,
            'sub-precedence','e='
          ),
          'loose-unary', nqp::hash(
            'precedence','h=', 'associative','unary'
          ),
          'comma', nqp::hash(
            'precedence','g=', 'associative','list', 'nulltermish',1
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
    method infix(str $operator) {
        my constant PROPERTIES := nqp::hash(
           '', 'default-infix',

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

          '//', 'tight-defor',

          '^^', 'tight-xor',

          'min', 'tight-minmax',
          'max', 'tight-minmax',

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

          'xor',    'loose-xor',

          'orelse', 'loose-orelse',
        );

        self.produce(PROPERTIES, $operator)
    }

    # Lookup properties of a prefix operator
    method prefix(str $operator) {
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

        self.produce(PROPERTIES, $operator)
    }

    # Lookup properties of a postfix operator
    method postfix(str $operator) {
        my constant PROPERTIES := nqp::hash(
           '', 'default-postfix',

          'i', 'methodcall',

          '++',  'autoincrement',
          '--',  'autoincrement',
          '⚛++', 'autoincrement',
          '⚛--', 'autoincrement',
        );

        self.produce(PROPERTIES, $operator)
    }

    # Lookup properties of a postcircumfix operator
    method postcircumfix(str $operator) {
        my constant PROPERTIES := nqp::hash(
           '', 'default-postcircumfix',

          '[ ]',  'methodcall',
          '[; ]', 'methodcall',
          '{ }',  'methodcall',
          '{; }', 'methodcall',
        );

        self.produce(PROPERTIES, $operator)
    }

    # Lookup properties of a circumfix operator
    method circumfix(str $operator) {
        my constant PROPERTIES := nqp::hash(
           '', 'default-circumfix',
        );

        self.produce(PROPERTIES, $operator)
    }
}

# vim: expandtab shiftwidth=4
