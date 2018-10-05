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
    passedArgs = ['perl6-js', '--target=js', source];

    const oldWritefh = nqp.op.getstdout().constructor.prototype.$$writefh;
    let output;
    nqp.op.getstdout().constructor.prototype.$$writefh = function(buf) {
      output = core.toRawBuffer(buf).toString();
    }
    code();
    nqp.op.getstdout().constructor.prototype.$$writefh = oldWritefh;
    return output;
};

   
