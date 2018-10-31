const fs = require('fs');
const path = require('path');

const releaseDir = 'release';

const version = process.argv[2];
const rakudoPath = process.argv[3];
const nqpInstallPath = process.argv[4];

if (!version) {
  console.error('USAGE: node make-release.js VERSION');
  process.exit();
}

try {
  fs.mkdirSync(releaseDir);
} catch (e) {
  if (e.code !== 'EEXIST') throw e;
}

const precompiledPerl6 = path.join(rakudoPath, 'node_modules');

function prepare(oldPath, newPath) {
  console.log('generating', newPath);
  let contents = fs.readFileSync(oldPath, 'utf8');

  contents = contents.replace('var nqp = require("' + path.join(nqpInstallPath, "share/nqp/lib/nqp-js-on-js/node_modules/nqp-runtime") + '");', 'var nqp = require("nqp-runtime");\n');


  contents = contents.replace('nqp.libpath(["' + path.join(rakudoPath, "node_modules/") + '","' + path.join(nqpInstallPath, "share/nqp/lib/nqp-js-on-js/") + '"]);', 'nqp.libpath([{module: module, prefix:\'.\/\'}, {module: module, prefix:\'nqp-js-on-js/\'}]);\n');

  contents = contents.replace('nqp.extraRuntime(\'perl6\', "' + path.join(rakudoPath, "src/vm/js/perl6-runtime") + '")', 'nqp.extraRuntime(\'perl6\', module);');

  contents = contents.replace('nqp.execname("' + path.join(rakudoPath, "perl6-js") + '")', 'nqp.execname(module.filename, true)');

  fs.writeFileSync(newPath, contents);
}

for (const file of fs.readdirSync(precompiledPerl6)) {
  if (/\.js$/.test(file)) {
    const oldPath = path.join(precompiledPerl6, file);
    const newPath = path.join(releaseDir, file);
    prepare(oldPath, newPath);
  } else if (/\.map$/.test(file)) {
    const oldPath = path.join(precompiledPerl6, file);
    const newPath = path.join(releaseDir, file);
    fs.copyFileSync(oldPath, newPath);
  } else {
    console.log('skipping', file);
  }
}

fs.copyFileSync('rakudo.js.map', path.join(releaseDir, 'rakudo.js.map'));
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
