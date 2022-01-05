# Dependencies

Install node 10.16.0 or 12.11.1 from https://nodejs.org

# Getting started with node.js rakudo.js

The easiest way is to install the rakudo package from npm

```bash
mkdir tutorial-project # Create a fresh project directory
cd tutorial-project
npm init
npm install --save rakudo
./node_modules/.bin/perl6-js -e 'say "Hello World"'
```
# Using a node.js module

```bash
cd tutorial-project
npm install --save chalk
```

To use node.js modules you need to specify where they should be looked for.

```use lib 'nodejs#/your/path/to/node_modules'``` is a good way to do that.

```
use lib 'nodejs#' ~ $*PROGRAM.parent.add('node_modules').absolute;
use chalk:from<node.js>;
say("Hello {chalk.blue("Blue")} World");
```

Keep in mind that if you load a node.js module during precompilation it gets
*reloaded* at runtime, so it's internal state gets lost.

# Interoperability with JS

Passing :lang<JavaScript> to eval will execute the passed code as JavaScript.

my $document = EVAL(:lang<JavaScript>, 'return document')

You can access attributes of those objects using postcircumfix:<{ }>
(you should often use the <> shorcut)
You can call methods on the the objects using regular Raku syntax.

```raku
$document<body>.appendChild($document.createTextNode('Hello World'));
```

Primitive JS data types are converted rather then wrapped

| JavaScript   | Raku |
| -------------|-------|
| true         | True  |
| false        | False |
| String       | Str   |
| null         | Mu    |
| undefined    | Mu    |
| BigInt       | Int   |
| Number       | Num   |

A Raku Mu when passed to JS land ends up as null

To pass values to Raku land the executed code needs a return.

```raku
EVAL(:lang<JavaScript>, '123') # This returns Mu
EVAL(:lang<JavaScript>, 'return 123') # This returns 123
```

# Extra methods on wrapped JS objects

In order to enable using wrapped objects in Raku land wrapped objects
offer some methods that Raku expects.

* sink

  Does nothing.

* defined

  Always returns True

* Bool

  Always returns True

* item

  Returns the object it is called on

* new

  Uses the JavaScript new operator to create an new instance

  ```raku
  my $Date = EVAL(:lang<JavaScript>, 'return Date');
  my $instance = $Date.new('December 17, 1995 03:24:00');
  say($instance.getFullYear()); # 1995
  ```


If the wrapped object has method of that same name you can use an :INTERNAL modifier to access it.

```$obj.new(:INTERNAL, 123)```
| -------------|-------|

This will call a js new method rather then doing ``new $obj(123)```
