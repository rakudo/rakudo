const fs = require('fs');
const path = require('path');

const releaseDir = 'release';

const version = process.argv[2];
const rakudoPath = process.argv[3];
const nqpInstallPath = process.argv[4];
const nqpPath = process.argv[5];
const parcelPluginPath = process.argv[6];

if (process.argv.length !== 7) {
  console.error('USAGE: node make-release.js VERSION PATH-TO-RAKUDO PATH-To-NQP-INSTALL PATH-TO-NQP-REPO PATH-TO-PARCEL-PLUGIN-REPO');
  process.exit();
}

function mkdirIfMissing(dir) {
  try {
    fs.mkdirSync(dir);
  } catch (e) {
    if (e.code !== 'EEXIST') throw e;
  }
}

mkdirIfMissing(releaseDir);
mkdirIfMissing(path.join(releaseDir, 'Perl6'));
mkdirIfMissing(path.join(releaseDir, 'Perl6', 'BOOTSTRAP'));

function prepare(oldPath, newPath, keepLibpath) {
  console.log('generating', newPath);
  let contents = fs.readFileSync(oldPath, 'utf8');

  const runtime = path.join(nqpInstallPath, "share/nqp/lib/nqp-js-on-js/node_modules/nqp-runtime");

  contents = contents.replace('var nqp = require("' + path.join(nqpInstallPath, "share/nqp/lib/nqp-js-on-js/node_modules/nqp-runtime") + '");', 'var nqp = require("nqp-runtime");\n');

  contents = contents.replace('body(require("' + runtime + '"), true)', 'body(require("nqp-runtime"), true)');

  contents = contents.replace('nqp.libpath(["' + path.join(rakudoPath, "blib") + '","' + path.join(nqpInstallPath, "share/nqp/lib/nqp-js-on-js") + '"]);', keepLibpath ? 'nqp.libpath([{module: module, prefix:\'.\/\'}, {module: module, prefix:\'nqp-js-on-js/\'}]);\n' : '');

  contents = contents.replace('nqp.extraRuntime(\'perl6\', "' + path.join(rakudoPath, "src/vm/js/perl6-runtime") + '")', 'nqp.extraRuntime(\'perl6\', module);');

  contents = contents.replace('nqp.execname("' + path.join(rakudoPath, "perl6-js") + '")', 'nqp.execname(module.filename, true)');

  fs.writeFileSync(newPath, contents);
}

function copyOver(from, to) {
  for (const file of fs.readdirSync(from)) {
    if (/\.js$/.test(file)) {
      const oldPath = path.join(from, file);
      const newPath = path.join(to, file);
      prepare(oldPath, newPath, false);
    } else if (/\.map$/.test(file)) {
      const oldPath = path.join(from, file);
      const newPath = path.join(to, file);
      fs.copyFileSync(oldPath, newPath);
    } else {
      console.log('skipping', file);
    }
  }
}

copyOver(path.join(rakudoPath, 'blib'), releaseDir);
copyOver(path.join(path.join(rakudoPath, 'blib'), 'Perl6'), path.join(releaseDir, 'Perl6'));
copyOver(path.join(path.join(rakudoPath, 'blib'), 'Perl6', 'BOOTSTRAP'), path.join(releaseDir, 'Perl6', 'BOOTSTRAP'));


fs.copyFileSync('perl6.js.map', path.join(releaseDir, 'perl6.js.map'));
prepare('perl6.js', path.join(releaseDir, 'perl6.js'), true);

fs.copyFileSync('src/vm/js/rakudo-library.js', path.join(releaseDir, 'rakudo-library.js'));


fs.writeFileSync(path.join(releaseDir, 'package.json'), JSON.stringify({
  "version": version,
  "name": "rakudo",
  "bin": {
    "perl6-js": "perl6.js"
  },
  "files": [
    "**/*.js", "**/*.js.map"
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
    "nqp-js-on-js": version,
    "tmp": "0.0.33"
  }
}, null, 2));

function updateVersionFor(keyword, version, content) {
  const regexp = new RegExp('("' + keyword + '": ")' + '\\d+\\.\\d+\\.\\d+');
  return content.replace(regexp,
    (whole, before) => before + version);
}

function bumpVersion(path, version, bumpDeps=[]) {
  let content = fs.readFileSync(path, 'utf8');

  content = updateVersionFor('version', version, content);

  for (const dep of bumpDeps) {
    content = updateVersionFor(dep, version, content);
  }

  fs.writeFileSync(path, content);
}

bumpVersion('src/vm/js/perl6-runtime/package.json', version);
bumpVersion(path.join(nqpPath, 'src/vm/js/nqp-runtime/package.json'), version);
bumpVersion(path.join(nqpPath, 'nqp-js-on-js/package.json'), version, ['nqp-runtime']);
bumpVersion(path.join(parcelPluginPath, 'package.json'), version, ['rakudo', 'nqp-browser-runtime']);
