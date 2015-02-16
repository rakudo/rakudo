use v6;

use Test;

my $e = \(X::AdHoc, message => 'Can not set relative dir from different roots');

plan 143;

is TRANSPOSE-ONE('abcb','b','d'),'adcd', 'does TRANSPOSE-ONE work';

is FORWARD-SLASH('/a/b/c'),    '/a/b/c', 'does FORWARD-SLASH leave /';
is FORWARD-SLASH('\\a\\b\\c'), '/a/b/c', 'does FORWARD-SLASH convert \\';

is BACKWARD-SLASH('\\a\\b\\c'), '\\a\\b\\c', 'does BACKWARD-SLASH leave \\';
is BACKWARD-SLASH('/a/b/c'),    '\\a\\b\\c', 'does BACKWARD-SLASH convert /';

is MAKE-ABSOLUTE-PATH('/',     '/' ), '/',      'is /      /  ok';
is MAKE-ABSOLUTE-PATH('/a/b/c','/' ), '/a/b/c', 'is /a/b/c /  ok';
is MAKE-ABSOLUTE-PATH( 'a'    ,'/' ), '/a',     'is  a     /  ok';
is MAKE-ABSOLUTE-PATH( 'a/b/c','/' ), '/a/b/c', 'is  a/b/c /  ok';
is MAKE-ABSOLUTE-PATH(   'b/c','/a'), '/a/b/c', 'is    b/c /a ok';

is MAKE-ABSOLUTE-PATH('/',     'C:/' ), 'C:/',      'is /      C:/  ok';
is MAKE-ABSOLUTE-PATH('/a/b/c','C:/' ), 'C:/a/b/c', 'is /a/b/c C:/  ok';
is MAKE-ABSOLUTE-PATH( 'a'    ,'C:/' ), 'C:/a',     'is  a     C:/  ok';
is MAKE-ABSOLUTE-PATH( 'a/b/c','C:/' ), 'C:/a/b/c', 'is  a/b/c C:/  ok';
is MAKE-ABSOLUTE-PATH(   'b/c','C:/a'), 'C:/a/b/c', 'is    b/c C:/a ok';

is MAKE-ABSOLUTE-PATH('C:/',     '/'  ), 'C:/',      'is C:/      /   ok';
is MAKE-ABSOLUTE-PATH('C:/a/b/c','/'  ), 'C:/a/b/c', 'is C:/a/b/c /   ok';
throws_like { MAKE-ABSOLUTE-PATH('C:a','/') },      |$e;
throws_like { MAKE-ABSOLUTE-PATH('C:a/b/c','/') },  |$e;
throws_like { MAKE-ABSOLUTE-PATH('C:b/c'  ,'/a') }, |$e;

is MAKE-ABSOLUTE-PATH('C:/',     'A:/'  ), 'C:/',      'is C:/      A:/ ok';
is MAKE-ABSOLUTE-PATH('C:/a/b/c','A:/'  ), 'C:/a/b/c', 'is C:/a/b/c A:/ ok';
throws_like { MAKE-ABSOLUTE-PATH('C:a','A:/') },     |$e;
throws_like { MAKE-ABSOLUTE-PATH('C:a/b/c','A:/') }, |$e;
throws_like { MAKE-ABSOLUTE-PATH('C:b/c','A:/a')  }, |$e;

is MAKE-ABSOLUTE-PATH('C:/',     'C:/' ), 'C:/',      'is C:/      C:/  ok';
is MAKE-ABSOLUTE-PATH('C:/a/b/c','C:/' ), 'C:/a/b/c', 'is C:/a/b/c C:/  ok';
is MAKE-ABSOLUTE-PATH( 'C:a'    ,'C:/' ), 'C:/a',     'is  C:a     C:/  ok';
is MAKE-ABSOLUTE-PATH( 'C:a/b/c','C:/' ), 'C:/a/b/c', 'is  C:a/b/c C:/  ok';
is MAKE-ABSOLUTE-PATH(   'C:b/c','C:/a'), 'C:/a/b/c', 'is    C:b/c C:/a ok';

is MAKE-ABSOLUTE-PATH('/',     '//nm/' ), '/',          'is /      //nm/  ok';
is MAKE-ABSOLUTE-PATH('/a/b/c','//nm/' ), '/a/b/c',     'is /a/b/c //nm/  ok';
is MAKE-ABSOLUTE-PATH( 'a'    ,'//nm/' ), '//nm/a',     'is  a     //nm/  ok';
is MAKE-ABSOLUTE-PATH( 'a/b/c','//nm/' ), '//nm/a/b/c', 'is  a/b/c //nm/  ok';
is MAKE-ABSOLUTE-PATH(   'b/c','//nm/a'), '//nm/a/b/c', 'is    b/c //nm/a ok';

is MAKE-ABSOLUTE-PATH('//nm/',     '/'  ),  '//nm/',      'is //nm/      /    ok';
is MAKE-ABSOLUTE-PATH('//nm/a/b/c','/'  ),  '//nm/a/b/c', 'is //nm/a/b/c /    ok';
is MAKE-ABSOLUTE-PATH('//nm/',     '//no'), '//nm/',      'is //nm/      //no ok';
is MAKE-ABSOLUTE-PATH('//nm/a/b/c','//no'), '//nm/a/b/c', 'is //nm/a/b/c //no ok';

for '', 'C:', '//nm' {
    is MAKE-BASENAME("$_/"),            '',       "is $_/                   ok";
    is MAKE-BASENAME("$_/foo"),         'foo',    "is $_/foo         foo    ok";
    is MAKE-BASENAME("$_/foo/"),        '',       "is $_/foo/               ok";
    is MAKE-BASENAME("$_/foo.pm"),      'foo.pm', "is $_/foo.pm      foo.pm ok";
    is MAKE-BASENAME("$_/foo.pm/"),     '',       "is $_/foo.pm/            ok";
    is MAKE-BASENAME("$_/bar/foo"),     'foo',    "is $_/bar/foo     foo    ok";
    is MAKE-BASENAME("$_/bar/foo/"),    '',       "is $_/bar/foo/           ok";
    is MAKE-BASENAME("$_/bar/foo.pm"),  'foo.pm', "is $_/bar/foo.pm  foo.pm ok";
    is MAKE-BASENAME("$_/bar/foo.pm/"), '',       "is $_/bar/foo.pm/        ok";
}

is MAKE-EXT("foo"),        '',   "is foo           ok";
is MAKE-EXT("foo.pm"),     'pm', "is foo.pm     pm ok";
is MAKE-EXT("foo.bar.pm"), 'pm', "is foo.bar.pm pm ok";

sub MCP(\path) { MAKE-CLEAN-PARTS(path).join('/') }
for '', 'C:', '//nm' -> $volume {
    for
      '',
      'foo/..',
      'foo//..',
      'foo/../',
      'foo/..//',
      'foo/../.',
      'foo/../..',
      'foo/../../',
      'foo/./../..',
      'foo/././..',
    () {
        is MCP("$volume/$_"), "$volume/", "is $volume/$_ $volume/ ok";
    }

    for
      'foo',
      'foo/',
      'foo//',
      'foo/.',
      'foo/./',
      'foo//./',
      'foo/bar/..',
      'foo/bar/../',
      'foo/bar/../.',
      'foo/bar/./..',
      'foo/bar/./../',
      'foo/bar/./..//',
      'foo/bar/./../.',
    () {
        is MCP("$volume/$_"), "$volume/foo", "is $volume/$_ $volume/foo ok";
    }
}

is REMOVE-ROOT('/',       '/'),    '/',    'is /        /    /    ok';
is REMOVE-ROOT('/',       '/foo'), 'foo',  'is /        /foo foo  ok';
is REMOVE-ROOT('/foo',    '/foo'), '/foo', 'is /foo     /foo /foo ok';
is REMOVE-ROOT('/foo/bar','/foo'), '/foo', 'is /foo/bar /foo /foo ok';
is REMOVE-ROOT('/bar',    '/foo'), '/foo', 'is /bar     /foo /foo ok';
