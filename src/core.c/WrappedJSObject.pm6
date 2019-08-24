my class WrappedJSObject is repr('WrappedJSObject') {
}

nqp::sethllconfig('perl6', nqp::hash('js_box', WrappedJSObject));

multi sub postcircumfix:<{ }>(WrappedJSObject \SELF, \key) is raw {
  nqp::getjsattr(SELF, key);
}

multi sub postcircumfix:<{ }>(WrappedJSObject \SELF, \key, \value) is raw {
  nqp::setjsattr(SELF, key, value);
}
