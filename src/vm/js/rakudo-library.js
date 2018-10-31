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

module.exports = function(source) {
    const tmpFile = tmp.tmpNameSync();

    passedArgs = ['perl6-js', '--output', tmpFile, '--target=js', source];

    const oldWritefh = nqp.op.getstdout().constructor.prototype.$$writefh;
    const output = [];
    nqp.op.getstdout().constructor.prototype.$$writefh = function(buf) {
      output.push(core.toRawBuffer(buf));
    }

    code();

    nqp.op.getstdout().constructor.prototype.$$writefh = oldWritefh;
    const lines = Buffer.concat(output).toString().split(/\n/);

    const loaded = [];

    for (const line of lines) {
      let match;
      if (/^[A-Z0-9]{40}\0/.test(line)) {
      } else if (match = line.match(/^load-unit: (.+)/)) {
        loaded.push(match[1]);
      } else {
      }
    }


    return {js: fs.readFileSync(tmpFile, 'utf8'), loaded: loaded};
};
