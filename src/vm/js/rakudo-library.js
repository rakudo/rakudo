const fs = require('fs');
const tmp = require('tmp');

const nqp = require('nqp-runtime');
const oldRun = nqp.run;

const oldArgs = nqp.args;

nqp.run = function(code) {
  nqp.run = oldRun;
  return code;
};

let passedArgs;

nqp.args = function(calledFrom) {
  if (calledFrom.parent === module) {
      return passedArgs.map(arg => new nqp.NativeStrArg(arg));
  }
  return oldArgs(calledFrom);
};

const code = require('./rakudo.js');

const core = require('nqp-runtime/core.js');

module.exports = function(source, options = {}) {
  const tmpFile = tmp.tmpNameSync();

  passedArgs = ['perl6-js', '--output', tmpFile, '--target=js', source];

  const oldWritefh = nqp.op.getstdout().constructor.prototype.$$writefh;
  const output = [];
  nqp.op.getstdout().constructor.prototype.$$writefh = function(buf) {
    output.push(core.toRawBuffer(buf));
  }

  if (options.rakudoPrecompWith) {
    const oldValue = options.rakudoPrecompWith;
    process.env.RAKUDO_PRECOMP_WITH = options.rakudoPrecompWith;
    code();
    process.env.RAKUDO_PRECOMP_WITH = oldValue;
  } else {
    code();
  }

  nqp.op.getstdout().constructor.prototype.$$writefh = oldWritefh;
  const lines = Buffer.concat(output).toString().split(/\n/);

  const loaded = [];

  for (const line of lines) {
    let match;
    if (/^[A-Z0-9]{40}\0/.test(line)) {
    } else if (match = line.match(/^LOAD-UNIT ID:(.*?) DEPS:(.*?) PATH:(.*)/)) {
      const deps = match[2] == '' ? [] : match[2].split(',');
      loaded.push({id: match[1], deps: deps, path: match[3]});
    } else {
      console.warn('extra line', line);
    }
  }


  return {js: fs.readFileSync(tmpFile, 'utf8'), loaded: loaded};
};
