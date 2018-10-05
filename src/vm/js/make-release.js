const fs = require('fs');
const path = require('path');

const releaseDir = 'release';

const version = process.argv[2];

if (!version) {
  console.error('USAGE: node make-release.js VERSION');
  process.exit();
}

try {
  fs.mkdirSync(releaseDir);
} catch (e) {
  if (e.code !== 'EEXIST') throw e;
}

const precompiledPerl6 = '/home/pmurias/rakudo/node_modules';

function prepare(oldPath, newPath) {
  console.log('generating', newPath);
  let contents = fs.readFileSync(oldPath, 'utf8');

  contents = contents.replace('var nqp = require("/home/pmurias/nqp/install/share/nqp/lib/nqp-js-on-js/node_modules/nqp-runtime");', 'var nqp = require("nqp-runtime");\n');

  contents = contents.replace('nqp.libpath(["/home/pmurias/rakudo/node_modules/","/home/pmurias/nqp/install/share/nqp/lib/nqp-js-on-js/"]);', 'nqp.libpath([{module: module, prefix:\'.\/\'}, {module: module, prefix:\'nqp-js-on-js/\'}]);\n');

  contents = contents.replace('nqp.extraRuntime(\'perl6\', "/home/pmurias/rakudo/src/vm/js/perl6-runtime")', 'nqp.extraRuntime(\'perl6\', module);');
  fs.writeFileSync(newPath, contents);
}

for (const file of fs.readdirSync(precompiledPerl6)) {
  if (!/\.js$/.test(file)) continue;

  const oldPath = path.join(precompiledPerl6, file);
  const newPath = path.join(releaseDir, file);
  prepare(oldPath, newPath);
}

prepare('rakudo.js', path.join(releaseDir, 'rakudo.js'));

fs.copyFileSync('src/vm/js/rakudo-library.js', path.join(releaseDir, 'rakudo-library.js'));


fs.writeFileSync(path.join(releaseDir, 'package.json'), JSON.stringify({
  "version": version,
  "name": "rakudo",
  "bin": {
    "perl6-js": "rakudo.js"
  },
  "files": [
    "*.js"
  ],
  "licenses": [
    {
      "type": "Artistic 2",
      "url": "http://opensource.org/licenses/Artistic-2.0"
    }
  ],
  "dependencies": {
    "nqp-runtime": version,
    "perl6-runtime": version,
    "nqp-js-on-js": version
  }
}, null, 2));
