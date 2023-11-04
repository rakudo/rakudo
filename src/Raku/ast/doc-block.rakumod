# Anything RakuAST::Doc related
class RakuAST::Doc
  is RakuAST::Node
{
    method must-define(str $method) {
        nqp::die(self.HOW.name(self) ~ " must define a '$method' method");
    }
    method cannot-be-instantiated() {
        nqp::die(self.HOW.name(self) ~ " cannot be instantiated");
    }

    method new(*@_, *%_) { self.cannot-be-instantiated }
    method visit-children(Code $visitor) { self.must-define("visit-children") }
}

# Generic paragraph
class RakuAST::Doc::Paragraph
  is RakuAST::Doc
{
    has List $!atoms;

    method new(*@atoms) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Doc::Paragraph, '$!atoms', @atoms);
        $obj
    }

    method visit-children(Code $visitor) {
        for $!atoms {
            if nqp::istype($_,RakuAST::Node) {
                $visitor($_);
            }
        }
    }

    method add-atom($atom) { nqp::push($!atoms, $atom) }
    method atoms() { self.IMPL-WRAP-LIST($!atoms) }
}

# Generic block with paragraphs
class RakuAST::Doc::Block
  is RakuAST::Doc
  is RakuAST::CheckTime
{
    has str  $.margin;           # the left margin to be applied
    has str  $.type;             # the type (e.g. "doc", "head", "item", etc)
    has int  $.level;            # the level (default "", or numeric 1..N)
    has Hash $!config;           # the config hash (e.g. :numbered, :allow<B>)
    has List $!paragraphs;       # the actual content
    has int  $!status;           # 0 =begin, 1 =for, 2 =abbrev, 3 =directive
    has int  $!pod-index;        # index in $=pod
    has Mu   $!resolved-config;  # HLL-resolved config

    method new(Str :$margin,
               Str :$type!,
               Int :$level,
              Hash :$config,
              List :$paragraphs,
              Bool :$directive,
              Bool :$for,
              Bool :$abbreviated
    ) {
        my $obj := nqp::create(self);
        $obj.set-margin($margin);
        $obj.set-type($type);
        $obj.set-level($level);
        $obj.set-config($config);
        $obj.set-paragraphs($paragraphs);

        my int $status :=
          $directive ?? 3 !! $abbreviated ?? 2 !! $for ?? 1 !! 0;
        nqp::bindattr_i($obj,RakuAST::Doc::Block,'$!status',$status);

        if nqp::isconcrete($*LEGACY-POD-INDEX) {
            nqp::bindattr_i($obj,RakuAST::Doc::Block,
              '$!pod-index', $*LEGACY-POD-INDEX++);
        }
        else {
            nqp::bindattr_i($obj,RakuAST::Doc::Block,'$!pod-index',-1);
        }
        $obj
    }

    method set-margin(Str $margin) {
        nqp::bindattr_s(self, RakuAST::Doc::Block, '$!margin', $margin // "");
        Nil
    }

    method set-type(Str $type) {
        nqp::bindattr_s(self, RakuAST::Doc::Block, '$!type',
          $type // nqp::die("Must specify a type"));
        Nil
    }

    method set-level(Int $level) {
        nqp::bindattr_i(self, RakuAST::Doc::Block, '$!level', $level // 0);
        Nil
    }
    method level() { $!level ?? ~$!level !! "" }

    method set-config($config) {
        nqp::bindattr(self, RakuAST::Doc::Block, '$!config',
          $config ?? self.IMPL-UNWRAP-MAP($config) !! nqp::hash);
        Nil
    }
    method add-config(Str $key, $value) {
        nqp::bindkey($!config, $key, $value)
    }
    method config() { self.IMPL-WRAP-MAP($!config) }

    method set-paragraphs($paragraphs) {
        nqp::bindattr(self, RakuAST::Doc::Block, '$!paragraphs',
          $paragraphs
            ?? self.IMPL-UNWRAP-LIST($paragraphs)
            !! nqp::list);
        Nil
    }
    method add-paragraph($paragraph, :$at-start) {
        $at-start
          ?? nqp::unshift($!paragraphs, $paragraph)
          !! nqp::push(   $!paragraphs, $paragraph)
    }
    method paragraphs() { self.IMPL-WRAP-LIST($!paragraphs) }

    method delimited()   { $!status == 0 }
    method for()         { $!status == 1 }
    method abbreviated() { $!status >= 2 }
    method directive()   { $!status == 3 }

    method visit-children(Code $visitor) {
        for $!paragraphs {
            if nqp::istype($_,RakuAST::Doc::Block) {
                my $*INNER-BLOCK := True;
                $visitor($_);
            }
            elsif nqp::istype($_,RakuAST::Node) {
                $visitor($_);
            }
        }
    }

    method PERFORM-CHECK(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        my $failed := self.literalize-config;
        if nqp::eqaddr($failed,RakuAST::Node::CannotLiteralize) {
            self.add-sorry:
              $resolver.build-exception: 'X::AdHoc',
                payload => "'$failed' is not constant in configuration";
        }

        # in an outermost block
        unless $*INNER-BLOCK {
            my $cu := $resolver.find-attach-target('compunit');
            if $!type eq 'data' {
                my $store := $cu.data-content;
                my $data  := self.Str.trim-trailing;

                (my $key := $!config<key>)
                  ?? $store.ASSIGN-KEY($key, $data)
                  !! $store.push($data);
            }
            $cu.set-pod-content($!pod-index, self.podify);
        }

        True
    }
}

# Doc markup support
class RakuAST::Doc::Markup
  is RakuAST::Doc
{
    has str  $.letter;     # the letter of Markup: A..Z
    has str  $.opener;     # opening sequence: < << «
    has str  $.closer;     # closing sequence: > >> »
    has List $!atoms;      # any contents of this markup (str | nested markup)
    has List $!meta;       # any meta info (e.g. url | lemma | original value)

    method new(:$letter!, :$opener, :$closer, :$atoms, :$meta) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Doc::Markup, '$!letter',
          $letter // nqp::die("Must specify a letter"));

        $obj.set-opener($opener // '<');
        $obj.set-closer($closer // '>');
        $obj.set-atoms($atoms);
        $obj.set-meta($meta);
        $obj
    }

    method visit-children(Code $visitor) {
        for $!atoms {
            if nqp::istype($_,RakuAST::Node) {
                $visitor($_);
            }
        }
        for $!meta {
            if nqp::istype($_,RakuAST::Node) {
                $visitor($_);
            }
        }
    }

    method set-opener(str $opener) {
        nqp::bindattr_s(self,RakuAST::Doc::Markup,'$!opener',$opener);
        Nil
    }

    method set-closer(str $closer) {
        nqp::bindattr_s(self,RakuAST::Doc::Markup,'$!closer',$closer);
        Nil
    }

    method set-atoms($atoms?) {
        nqp::bindattr(self, RakuAST::Doc::Markup, '$!atoms',
          $atoms ?? self.IMPL-UNWRAP-LIST($atoms) !! []);
        Nil
    }
    method add-atom($atom) { nqp::push($!atoms, $atom) }
    method atoms() { self.IMPL-WRAP-LIST($!atoms) }

    method set-meta($meta?) {
        nqp::bindattr(self, RakuAST::Doc::Markup, '$!meta',
          $meta ?? self.IMPL-UNWRAP-LIST($meta) !! []);
        Nil
    }
    method add-meta($meta) { nqp::push($!meta, $meta) }
    method meta() { self.IMPL-WRAP-LIST($!meta) }
}
