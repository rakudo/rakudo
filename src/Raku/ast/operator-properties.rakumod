class OperatorProperties {
    has str $.precedence;
    has str $.associative;
    has str $.thunky;
    has int $.iffy;

    method new(str :$precedence, str :$associative, str :$thunky, int :$iffy) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj,OperatorProperties,'$!precedence',
          $precedence // (nqp::defined(self) ?? $!precedence !! ""));
        nqp::bindattr_s($obj,OperatorProperties,'$!associative',
          $associative // (nqp::defined(self) ?? $!associative !! ""));
        nqp::bindattr_s($obj,OperatorProperties,'$!thunky',
          $thunky // (nqp::defined(self) ?? $!thunky !! ""));
        nqp::bindattr_i($obj,OperatorProperties,'$!iffy',
          $iffy // (nqp::defined(self) ?? $!iffy !! 0));

        $obj
    }

    # Accessors
    method precedence()  { nqp::defined(self) ?? $!precedence  !! "" }
    method associative() { nqp::defined(self) ?? $!associative !! "" }
    method thunky()      { nqp::defined(self) ?? $!thunky      !! "" }
    method iffy()        { nqp::defined(self) ?? $!iffy        !! 0  }

    # convenience methods
    method chaining() {
        nqp::defined(self) ?? $!associative eq 'chain' !! False
    }
    method short-circuit() {
        nqp::defined(self) ?? $!thunky ne "" !! False
    }

    # Return properties depending on other properties
    method equiv(str $associative) {
        nqp::defined(self)
          ?? self.new(
               precedence  => $!precedence,
               associative => $associative,
               thunky      => $!thunky,
               iffy        => $!iffy
             )
          !! self.new
    }

    method tighter(str $associative) {
        nqp::defined(self)
          ?? self.new(
               precedence  => nqp::join('@=',nqp::split('=',$!precedence)),
               associative => $associative,
               thunky      => $!thunky,
               iffy        => $!iffy
             )
          !! nqp::die("No precedence found to be tighter for")
    }

    method looser(str $associative) {
        nqp::defined(self)
          ?? self.new(
               precedence  => nqp::join(':=',nqp::split('=',$!precedence)),
               associative => $associative,
               thunky      => $!thunky,
               iffy        => $!iffy
             )
          !! nqp::die("No precedence found to be looser for")
    }

    # Return name of handler for reducing with these operator properties
    method reducer-name() {
        nqp::defined(self)
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
        if nqp::defined(self) {
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

    method qast-for-infix(str $op) {
        my constant QAST := nqp::hash(
          '||',  'unless',
          'or',  'unless',
          '&&',  'if',
          'and', 'if',
          '^^',  'xor',
          'xor', 'xor',
          '//',  'defor'
        );
        nqp::atkey(QAST,$op)
    }
}

class BuiltinOperatorTypes {
    method methodcall() {
        OperatorProperties.new(:precedence<y=>)
    }
    method autoincrement() {
        OperatorProperties.new(:precedence<x=>)
    }
    method exponentiation() {
        OperatorProperties.new(:precedence<w=>, :associative<right>)
    }
    method symbolic-unary() {
        OperatorProperties.new(:precedence<v=>)
    }
    method multiplicative() {
        OperatorProperties.new(:precedence<u=>, :associative<left>)
    }
    method iffy() {
        OperatorProperties.new(:precedence<u=>, :associative<left>, :iffy)
    }
    method additive() {
        OperatorProperties.new(:precedence<t=>, :associative<left>)
    }
    method replication() {
        OperatorProperties.new(:precedence<s=>, :associative<left>)
    }
    method replication-xx() {
        OperatorProperties.new(:precedence<s=>, :associative<left>, :thunky<t.>)
    }
    method concatenation() {
        OperatorProperties.new(:precedence<r=>, :associative<list>)
    }
    method junctive-and() {
        OperatorProperties.new(:precedence<q=>, :associative<list>)
    }
    method junctive-or() {
        OperatorProperties.new(:precedence<p=>, :associative<list>)
    }
    method structural() {
        OperatorProperties.new(:precedence<n=>, :associative<non>)
    }
    method chaining() {
        OperatorProperties.new(:precedence<m=>, :associative<chain>, :iffy)
    }
    method tight-and() {
        OperatorProperties.new(:precedence<l=>, :associative<list>, :thunky<.t>)
    }
    method tight-or() {
        OperatorProperties.new(:precedence<k=>, :associative<list>, :thunky<.t>)
    }
    method tight-or-xor() {
        OperatorProperties.new(:precedence<k=>, :associative<list>, :thunky<..t>)
    }
    method tight-or-minmax() {
        OperatorProperties.new(:precedence<k=>, :associative<list>)
    }
    method item-assignment() {
        OperatorProperties.new(:precedence<i=>, :associative<right>)
    }
    method loose-unary() {
        OperatorProperties.new(:precedence<h=>)
    }
    method comma() {
        OperatorProperties.new(:precedence<g=>, :associative<list>)
    }
    method list-infix() {
        OperatorProperties.new(:precedence<f=>, :associative<list>)
    }
    method list-prefix() {
        OperatorProperties.new(:precedence<e=>)
    }
    method loose-and() {
        OperatorProperties.new(:precedence<d=>, :associative<list>, :thunky<.t>)
    }
    method loose-andthen() {
        OperatorProperties.new(:precedence<d=>, :associative<list>, :thunky<.b>)
    }
    method loose-orelse() {
        OperatorProperties.new(:precedence<c=>, :associative<list>, :thunky<.b>)
    }

    method lookup(str $type) {
        my constant PROPERTIES := nqp::hash(
          'methodcall',      '',
          'autoincrement',   '',
          'exponentiation',  '',
          'symbolic-unary',  '',
          'multiplicative',  '',
          'iffy',            '',
          'additive',        '',
          'replication',     '',
          'replication-xx',  '',
          'concatenation',   '',
          'junctive-and',    '',
          'junctive-or',     '',
          'structural',      '',
          'chaining',        '',
          'tight-and',       '',
          'tight-or',        '',
          'tight-or-xor',    '',
          'tight-or-minmax', '',
          'item-assignment', '',
          'loose-unary',     '',
          'comma',           '',
          'list-infix',      '',
          'list-prefix',     '',
          'loose-and',       '',
          'loose-andthen',   '',
          'loose-orelse',    '',
        );

        (my $properties := nqp::atkey(PROPERTIES,$type))
          ?? $properties
          !! nqp::bindkey(PROPERTIES,$type,self."$type"())
    }
}

class BuiltinOperatorProperties {

    method infix(str $op) {
        my constant PROPERTIES := nqp::hash(

          '*',   'multiplicative',
          '/',   'multiplicative',
          'div', 'multiplicative',
          'gcd', 'multiplicative',
          'lcm', 'multiplicative',
          '%',   'multiplicative',
          'mod', 'multiplicative',
          '+&',  'multiplicative',
          '~&',  'multiplicative',
          '?&',  'multiplicative',

          '+',  'additive',
          '-',  'additive',
          '+|', 'additive',
          '+^', 'additive',
          '~|', 'additive',
          '~^', 'additive',
          '?|', 'additive',
          '?^', 'additive',

          '**', 'exponentiation',

          '%%', 'iffy',

          'x', 'replication',

          'xx', 'replication-xx',

          '~', 'concatenation',

          '&',   'junctive-and',
          '(&)', 'junctive-and',
          '∩',   'junctive-and',
          '(.)', 'junctive-and',
          '⊍',   'junctive-and',

          '|',   'junctive-or',
          '^',   'junctive-or',
          '(+)', 'junctive-or',
          '⊎',   'junctive-or',
          '(|)', 'junctive-or',
          '∪',   'junctive-or',
          '(-)', 'junctive-or',
          '∖',   'junctive-or',
          '(^)', 'junctive-or',
          '⊖',   'junctive-or',

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
          '//', 'tight-or',

          '^^', 'tight-or-xor',

          'min', 'tight-or-minmax',
          'max', 'tight-or-minmax',

          '=>', 'item-assignment',

          ',', 'comma',

          'Z',      'list-infix',
          'X',      'list-infix',
          '...',    'list-infix',
          '...^',   'list-infix',
          '^...',   'list-infix',
          '^...^',  'list-infix',
          'minmax', 'list-infix',

          '=',   'list-prefix',
          '⚛=',  'list-prefix',
          '⚛+=', 'list-prefix',
          '⚛-=', 'list-prefix',
          '⚛−=', 'list-prefix',

          'and', 'loose-and',

          'andthen',    'loose-andthen',
          'notandthen', 'loose-andthen',

          'or',     'loose-orelse',
          'xor',    'loose-orelse',
          'orelse', 'loose-orelse',
        );

        nqp::isstr(my $properties := nqp::atkey(PROPERTIES,$op))
          ?? nqp::bindkey(PROPERTIES,$op,BuiltinOperatorTypes.lookup($properties))
          !! $properties
    }

    method prefix(str $op) {
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

        nqp::isstr(my $properties := nqp::atkey(PROPERTIES,$op))
          ?? nqp::bindkey(PROPERTIES,$op,BuiltinOperatorTypes.lookup($properties))
          !! $properties
    }

    method postfix(str $op) {
        my constant PROPERTIES := nqp::hash(

          'i', 'methodcall',

          '++',  'autoincrement',
          '--',  'autoincrement',
          '⚛++', 'autoincrement',
          '⚛--', 'autoincrement',
        );

        nqp::isstr(my $properties := nqp::atkey(PROPERTIES,$op))
          ?? nqp::bindkey(PROPERTIES,$op,BuiltinOperatorTypes.lookup($properties))
          !! $properties
    }

    method postcircumfix(str $op) {
        my constant PROPERTIES := nqp::hash(

          '[ ]', 'methodcall',
          '{ }', 'methodcall',
        );

        nqp::isstr(my $properties := nqp::atkey(PROPERTIES,$op))
          ?? nqp::bindkey(PROPERTIES,$op,BuiltinOperatorTypes.lookup($properties))
          !! $properties
    }
}

# vim: expandtab shiftwidth=4
