my class WrappedJSObject is repr('WrappedJSObject') {
}

nqp::sethllconfig('Raku', nqp::hash('js_box', WrappedJSObject));

multi sub postcircumfix:<{ }>(WrappedJSObject \SELF, \key) is raw {
  nqp::getjsattr(SELF, key);
}

multi sub postcircumfix:<{ }>(WrappedJSObject \SELF, \key, \value) is raw {
  nqp::setjsattr(SELF, key, value);
}

# vim: expandtab shiftwidth=4
