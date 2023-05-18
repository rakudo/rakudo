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

    method worry-ad-hoc(Str $payload) {
        if $*RESOLVER -> $resolver {
            $resolver.add-worry:
              $resolver.build-exception: 'X::AdHoc', :$payload;
        }
        else {
            nqp::say($payload)
        }
    }

    method sorry-ad-hoc(Str $payload) {
        if $*RESOLVER -> $resolver {
            $resolver.add-sorry:
              $resolver.build-exception: 'X::AdHoc', :$payload;
        }
        else {
            nqp::die($payload);
        }
    }
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

    # nothing to do
    method visit-children(Code $visitor) { }

    method add-atom($atom) { nqp::push($!atoms, $atom) }
    method atoms() { self.IMPL-WRAP-LIST($!atoms) }
}

# Generic block with paragraphs
class RakuAST::Doc::Block
  is RakuAST::Doc
  is RakuAST::CheckTime
{
    has str  $.type;         # the type (e.g. "doc", "head", "item", etc)
    has int  $.level;        # the level (default "", or numeric 1..N)
    has Hash $!config;       # the config hash (e.g. :numbered, :allow<B>)
    has Bool $.abbreviated;  # bool: true if =item rather than =begin item
    has List $!paragraphs;   # the actual content
    has int  $!pod-index;

    method new(Str :$type!,
               Int :$level,
              Hash :$config,
              List :$paragraphs,
              Bool :$abbreviated
    ) {
        my $obj := nqp::create(self);
        $obj.set-type($type);
        $obj.set-level($level);
        $obj.set-config($config);
        $obj.set-abbreviated($abbreviated);
        $obj.set-paragraphs($paragraphs);

        if nqp::isconcrete($*LEGACY-POD-INDEX) {
            nqp::bindattr_i($obj,RakuAST::Doc::Block,
              '$!pod-index', $*LEGACY-POD-INDEX++);
        }
        else {
            nqp::bindattr_i($obj,RakuAST::Doc::Block,'$!pod-index',-1);
        }
        $obj
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

    method set-abbreviated(Bool $value) {
        nqp::bindattr(self, RakuAST::Doc::Block, '$!abbreviated',
          $value ?? True !! False);
    }

    method set-paragraphs($paragraphs) {
        nqp::bindattr(self, RakuAST::Doc::Block, '$!paragraphs',
          $paragraphs
            ?? self.IMPL-UNWRAP-LIST($paragraphs)
            !! nqp::list);
        Nil
    }
    method add-paragraph($paragraph) { nqp::push($!paragraphs, $paragraph) }
    method paragraphs() { self.IMPL-WRAP-LIST($!paragraphs) }

    method visit-children(Code $visitor) {
        # no serviceable parts inside
    }

    method PERFORM-CHECK(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        my $*RESOLVER := $resolver;
        $resolver.find-attach-target('compunit').set-pod-content(
          $!pod-index, self.podify
        );
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
    has str  $.separator;  # separator inside meta: for deparsing mainly
    has List $!atoms;      # any contents of this markup (str | nested markup)
    has List $!meta;       # any meta info (e.g. url | lemma | original value)

    method new(:$letter!, :$opener, :$closer, :$atoms, :$meta, :$separator) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Doc::Markup, '$!letter',
          $letter // nqp::die("Must specify a letter"));
        nqp::bindattr_s($obj, RakuAST::Doc::Markup, '$!opener',
          $opener // '<');
        nqp::bindattr_s($obj, RakuAST::Doc::Markup, '$!closer',
          $closer // '>');
        nqp::bindattr_s($obj, RakuAST::Doc::Markup, '$!separator',
          $separator // "");

        $obj.set-atoms($atoms);
        $obj.set-meta($meta);
        $obj
    }

    method visit-children(Code $visitor) {
        for $!atoms {
            $visitor($_);
        }
        for $!meta {
            $visitor($_);
        }
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
