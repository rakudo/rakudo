# Anything RakuAST::Doc related
class RakuAST::Doc
  is RakuAST::CheckTime
{
    method must-define(str $method) {
        nqp::die(self.HOW.name(self) ~ " must define a '$method' method");
    }
    method cannot-be-instantiated() {
        nqp::die(self.HOW.name(self) ~ " cannot be instantiated");
    }

    method new(*@_, *%_) { self.cannot-be-instantiated }
    method visit-children(Code $visitor) { self.must-define("visit-children") }

    method PERFORM-CHECK(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        True
    }

    method worry-ad-hoc($resolver, $payload) {
        self.add-worry: $resolver.build-exception: 'X::AdHoc', :$payload
    }

    method sorry-ad-hoc($resolver, $payload) {
        self.add-sorry: $resolver.build-exception: 'X::AdHoc', :$payload
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
{
    has str  $.type;
    has int  $.level;
    has Hash $!config;
    has Bool $.abbreviated;
    has List $!paragraphs;

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
        $resolver.find-attach-target('compunit').pod-content.push(
          self.make-legacy-pod
        );
        True
    }
}

# Doc markup support
class RakuAST::Doc::Markup
  is RakuAST::Doc
{
    has str  $.letter;
    has List $!atoms;
    has List $!meta;
    has str  $.separator;

    method new(:$letter!, :$atoms, :$meta, :$separator) {
        my $obj := nqp::create(self);
        $obj.set-letter($letter);
        $obj.set-atoms($atoms);
        $obj.set-meta($meta);
        $obj.set-separator($separator);
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

    method set-letter(Str $letter) {
        nqp::bindattr_s(self, RakuAST::Doc::Markup, '$!letter',
          $letter // nqp::die("Must specify a letter"));
        Nil
    }

    method set-separator(Str $separator) {
        nqp::bindattr_s(self, RakuAST::Doc::Markup, '$!separator',
          $separator // "");
        Nil
    }

    method set-atoms($atoms) {
        nqp::bindattr(self, RakuAST::Doc::Markup, '$!atoms',
          $atoms ?? self.IMPL-UNWRAP-LIST($atoms) !! []);
        Nil
    }
    method add-atom($atom) { nqp::push($!atoms, $atom) }
    method atoms() { self.IMPL-WRAP-LIST($!atoms) }

    method set-meta($meta) {
        nqp::bindattr(self, RakuAST::Doc::Markup, '$!meta',
          $meta ?? self.IMPL-UNWRAP-LIST($meta) !! []);
        Nil
    }
    method add-meta($meta) { nqp::push($!meta, $meta) }
    method meta() { self.IMPL-WRAP-LIST($!meta) }
}
