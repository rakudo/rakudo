# Base class for anything pod related
class RakuAST::Pod
  is RakuAST::Node { }

class RakuAST::Pod::Block
  is RakuAST::Pod { }

class RakuAST::Pod::Declarator
  is RakuAST::Pod { }
