class OperatorProperties {
    has str $.precedence;
    has str $.associative;
    has str $.thunky;
    has str $.dba;
    has str $.next-term;
    has str $.sub-precedence;
    has int $.iffy;
    has int $.diffy;
    has int $.fiddly;

    method new(str :$precedence, str :$associative, str :$thunky, str :$dba, str :$next-term, str :$sub-precedence, int :$iffy, int :$diffy, int :$fiddly) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj,OperatorProperties,'$!precedence',
          $precedence // (nqp::defined(self) ?? $!precedence !! ""));
        nqp::bindattr_s($obj,OperatorProperties,'$!associative',
          $associative // (nqp::defined(self) ?? $!associative !! ""));
        nqp::bindattr_s($obj,OperatorProperties,'$!thunky',
          $thunky // (nqp::defined(self) ?? $!thunky !! ""));

        nqp::bindattr_s($obj,OperatorProperties,'$!dba',
          $dba // (nqp::defined(self) ?? $!dba !! ""));
        nqp::bindattr_s($obj,OperatorProperties,'$!next-term',
          $next-term // (nqp::defined(self) ?? $!next-term !! ""));
        nqp::bindattr_s($obj,OperatorProperties,'$!sub-precedence',
          $sub-precedence // (nqp::defined(self) ?? $!sub-precedence !! ""));

        nqp::bindattr_i($obj,OperatorProperties,'$!iffy',
          $iffy // (nqp::defined(self) ?? $!iffy !! 0));
        nqp::bindattr_i($obj,OperatorProperties,'$!diffy',
          $diffy // (nqp::defined(self) ?? $!diffy !! 0));
        nqp::bindattr_i($obj,OperatorProperties,'$!fiddly',
          $fiddly // (nqp::defined(self) ?? $!fiddly !! 0));

        $obj
    }

    # Accessors
    method precedence()     { nqp::defined(self) ?? $!precedence     !! "" }
    method sub-precedence() { nqp::defined(self) ?? $!sub-precedence !! "" }
    method associative()    { nqp::defined(self) ?? $!associative    !! "" }
    method thunky()         { nqp::defined(self) ?? $!thunky         !! "" }
    method dba()            { nqp::defined(self) ?? $!dba            !! "" }
    method next-term()      { nqp::defined(self) ?? $!next-term      !! "" }
    method iffy()           { nqp::defined(self) ?? $!iffy           !! 0  }
    method diffy()          { nqp::defined(self) ?? $!diffy          !! 0  }
    method fiddly()         { nqp::defined(self) ?? $!fiddly         !! 0  }

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
               associative => nqp::isnull_s($associative)
                                ?? $!associative
                                !! $associative
             )
          !! self.new
    }

    method tighter(str $associative) {
        nqp::defined(self)
          ?? self.new(
               precedence  => nqp::join('@=',nqp::split('=',$!precedence)),
               associative => nqp::isnull_s($associative)
                                ?? $!associative
                                !! $associative
             )
          !! nqp::die("No precedence found to be tighter for")
    }

    method looser(str $associative) {
        nqp::defined(self)
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
}

class BuiltinOperatorTypes {
    method methodcall() {
        OperatorProperties.new(:precedence<y=>, :associative<unary>, :dba<methodcall>, :fiddly)
    }
    method autoincrement() {
        OperatorProperties.new(:precedence<x=>, :associative<unary>, :dba<autoincrement>)
    }
    method exponentiation() {
        OperatorProperties.new(:precedence<w=>, :associative<right>, :dba<exponentiation>)
    }
    method symbolic-unary() {
        OperatorProperties.new(:precedence<v=>, :associative<unary>, :dba('symbolic unary'))
    }
    method dotty-infix() {
        OperatorProperties.new(:precedence<v=>, :associative<left>, :dba('dotty infix'), :fiddly,
          :next-term<dottyopish>, :sub-precedence<z=>)
    }
    method multiplicative() {
        OperatorProperties.new(:precedence<u=>, :associative<left>, :dba<multiplicative>)
    }
    method multiplicative-iffy() {
        OperatorProperties.new(:precedence<u=>, :associative<left>, :dba<multiplicative>, :iffy)
    }
    method additive() {
        OperatorProperties.new(:precedence<t=>, :associative<left>, :dba<additive>)
    }
    method additive-iffy() {
        OperatorProperties.new(:precedence<t=>, :associative<left>, :dba<additive>, :iffy)
    }
    method replication() {
        OperatorProperties.new(:precedence<s=>, :associative<left>, :dba<replication>)
    }
    method replication-xx() {
        OperatorProperties.new(:precedence<s=>, :associative<left>, :dba<replication>, :thunky<t.>)
    }
    method concatenation() {
        OperatorProperties.new(:precedence<r=>, :associative<list>, :dba<replication>)
    }
    method junctive-and() {
        OperatorProperties.new(:precedence<q=>, :associative<list>, :dba('junctive and'))
    }
    method junctive-and-iffy() {
        OperatorProperties.new(:precedence<q=>, :associative<list>, :dba('junctive and'), :iffy)
    }
    method junctive-or() {
        OperatorProperties.new(:precedence<p=>, :associative<list>, :dba('junctive or'))
    }
    method junctive-or-iffy() {
        OperatorProperties.new(:precedence<p=>, :associative<list>, :dba('junctive or'), :iffy)
    }
    method named-unary() {
        OperatorProperties.new(:precedence<o=>, :associative<unary>, :dba('named unary'))
    }
    method structural() {
        OperatorProperties.new(:precedence<n=>, :associative<non>, :dba<structural>, :diffy)
    }
    method chaining() {
        OperatorProperties.new(:precedence<m=>, :associative<chain>, :dba<chaining>, :iffy, :diffy)
    }
    method tight-and() {
        OperatorProperties.new(:precedence<l=>, :associative<list>, :dba('tight and'), :thunky<.t>, :iffy)
    }
    method tight-or() {
        OperatorProperties.new(:precedence<k=>, :associative<list>, :dba('tight or'), :thunky<.t>)
    }
    method tight-or-defor() {
        OperatorProperties.new(:precedence<k=>, :associative<left>, :dba('tight or'), :thunky<.t>)
    }
    method tight-or-xor() {
        OperatorProperties.new(:precedence<k=>, :associative<list>, :dba('tight or'), :thunky<..t>, :iffy)
    }
    method tight-or-minmax() {
        OperatorProperties.new(:precedence<k=>, :associative<list>, :dba('tight or'))
    }
    method conditional() {
        OperatorProperties.new(:precedence<j=>, :associative<right>, :dba<conditional>, :thunky<.tt>, :iffy)
    }
    method conditional-ff() {
        OperatorProperties.new(:precedence<j=>, :associative<right>, :dba<conditional>, :thunky<tt>, :iffy)
    }
    method item-assignment() {
        OperatorProperties.new(:precedence<i=>, :associative<right>, :dba('item assignment'))
    }
    method list-assignment() {
        OperatorProperties.new(:precedence<i=>, :associative<right>, :dba('list assignment'), :fiddly,
          :sub-precedence<e=>)
    }
    method loose-unary() {
        OperatorProperties.new(:precedence<h=>, :associative<unary>, :dba('loose unary'))
    }
    method comma() {
        OperatorProperties.new(:precedence<g=>, :associative<list>, :dba<comma>, :fiddly,
          :next-term<nulltermish>)
    }
    method list-infix() {
        OperatorProperties.new(:precedence<f=>, :associative<list>, :dba('list infix'))
    }
    method list-prefix() {
        OperatorProperties.new(:precedence<e=>, :associative<right>, :dba('list prefix'))
    }
    method loose-and() {
        OperatorProperties.new(:precedence<d=>, :associative<left>, :dba('loose and'), :thunky<.t>)
    }
    method loose-andthen() {
        OperatorProperties.new(:precedence<d=>, :associative<left>, :dba('loose and'), :thunky<.b>)
    }
    method loose-or() {
        OperatorProperties.new(:precedence<c=>, :associative<left>, :dba('loose or'), :thunky<.t>, :iffy)
    }
    method loose-or-xor() {
        OperatorProperties.new(:precedence<c=>, :associative<list>, :dba('loose or'), :thunky<.t>, :iffy)
    }
    method loose-orelse() {
        OperatorProperties.new(:precedence<c=>, :associative<list>, :dba('loose or'), :thunky<.b>)
    }
    method sequencer() {
        OperatorProperties.new(:precedence<b=>, :associative<list>, :dba<sequencer>)
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

          '%%', 'multiplicative-iffy',
          '?&', 'multiplicative-iffy',

          '+',  'additive',
          '-',  'additive',
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
          '⚛=',  'item-assignment',
          '⚛+=', 'item-assignment',
          '⚛-=', 'item-assignment',
          '⚛−=', 'item-assignment',

          ',', 'comma',

          'Z',      'list-infix',
          'X',      'list-infix',
          '...',    'list-infix',
          '...^',   'list-infix',
          '^...',   'list-infix',
          '^...^',  'list-infix',
          'minmax', 'list-infix',

          '=',   'list-prefix',

          'and', 'loose-and',

          'andthen',    'loose-andthen',
          'notandthen', 'loose-andthen',

          'or',     'loose-or',

          'xor',    'loose-or-xor',

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
