class RakuAST::Pragma
  is RakuAST::Statement
  is RakuAST::BeginTime
  is RakuAST::ProducesNil
{
    has Str $.name;
    has int $.off;

    method new(Str $name?, :$off) {
        my $obj := nqp::create(self);
        nqp::bindattr(  $obj, RakuAST::Pragma, '$!name', $name // "");
        nqp::bindattr_i($obj, RakuAST::Pragma, '$!off', $off ?? 1 !! 0);
        $obj
    }

    method KNOWN-PRAGMAS() {
        my constant KNOWN-PRAGMAS := nqp::hash(
          'fatal',              0,
          'internals',          1,
          'MONKEY',             0,
          'MONKEY-BARS',        1,
          'MONKEY-BRAINS',      1,
          'MONKEY-BUSINESS',    1,
          'MONKEY-GUTS',        1,
          'MONKEY-SEE-NO-EVAL', 1,
          'MONKEY-SHINE',       1,
          'MONKEY-TRAP',        1,
          'MONKEY-TYPING',      1,
          'MONKEY-WRENCH',      1,
          'nqp',                1,
          'precompilation',     0,
          'trace',              1,
          'worries',            1,
        );
    }

    method IS-PRAGMA(Str $name) {
        nqp::existskey(self.KNOWN-PRAGMAS, $name)
    }

    method categoricals() { () }

    method PERFORM-BEGIN(
      RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        my $name   := $!name;
        my int $on := nqp::not_i($!off);
        if self.KNOWN-PRAGMAS{$name} {
            $*LANG.set_pragma($name, $on)
        }
        elsif $name eq 'MONKEY' {
            $*LANG.set_pragma($_.key, $on)
              if nqp::eqat($_.key,'MONKEY',0) for self.KNOWN-PRAGMAS;
        }
        elsif $name eq 'precompilation' {
            if $!off && $*CU.precompilation-mode {
                nqp::ifnull(
                  nqp::atkey(nqp::getenvhash, 'RAKUDO_PRECOMP_WITH'), 0
                ) ?? nqp::exit(0)
                  !! $resolver.build-exception(
                       'X::Pragma::CannotPrecomp', :what<no>, :$name
                     ).throw;
            }
        }
        elsif $name eq 'fatal' {
            nqp::die("use fatal NYI") if $on;
        }
        else {
            $resolver.build-exception(
              "X::Pragma::Unknown",:name($!name)
            ).throw;
        }
    }
}

class RakuAST::Isms
  is RakuAST::Statement
  is RakuAST::BeginTime
  is RakuAST::ProducesNil
{
    has RakuAST::Expression $.argument;
    has int $.off;

    method new(RakuAST::Expression $argument, :$off) {
        my $obj := nqp::create(self);
        nqp::bindattr(  $obj, RakuAST::Isms, '$!argument', $argument);
        nqp::bindattr_i($obj, RakuAST::Isms, '$!off', $off ?? 1 !! 0);
        $obj
    }

    method KNOWN-ISMS() {
        my constant ISMS := nqp::hash(
          'Perl5', 'p5isms',
          'C++',   'c++isms',
        )
    }

    method IS-ISM(Str $name) {
        nqp::existskey(self.KNOWN-ISMS, $name)
    }

    method categoricals() { () }

    method PERFORM-BEGIN(
      RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        my $arglist := $!argument
          ?? self.IMPL-BEGIN-TIME-EVALUATE(
               $!argument, $resolver, $context
             ).List.FLATTENABLE_LIST
          !! Nil;
        my int $on := nqp::not_i($!off);

        if nqp::islist($arglist) {
            for $arglist -> $ism {
                (my $pragma := self.KNOWN-ISMS{$ism})
                  ?? $*LANG.set_pragma($pragma, $on)
                  !! $resolver.build-exception(
                       "X::Ism::Unknown", :name($_)
                     ).throw;
            }
        }
        else {
            $*LANG.set_pragma($_.value, $on) for self.KNOWN-ISMS;
        }
    }
}
