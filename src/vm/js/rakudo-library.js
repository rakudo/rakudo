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

module.exports.compile = function(source, options = {}) {
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

module.exports.capturedRun = function(source, input, compileArgs, args) {
  const out = [];
  const err = [];

  passedArgs = ['perl6-js'].concat(compileArgs, ['*SOURCE*'], args);

  const oldExit = nqp.op.exit;

  class Exit {
    constructor(status) {
      this.status = status;
    }
  }

  nqp.op.exit = function(status) {
    throw new Exit(status);
  };

  const oldStdoutWritefh = nqp.op.getstdout().constructor.prototype.$$writefh;
  nqp.op.getstdout().constructor.prototype.$$writefh = function(buf) {
    out.push(core.toRawBuffer(buf));
  }

  const oldStderrWritefh = nqp.op.getstderr().constructor.prototype.$$writefh;
  nqp.op.getstderr().constructor.prototype.$$writefh = function(buf) {
    err.push(core.toRawBuffer(buf));
  }

  const oldOpen = nqp.op.open;

  class SourceFileHandle {
    constructor(source) {
      this.source = Buffer.from(source, 'utf8');
    }

    $$readfh(buf, bytes) {
      core.writeBuffer(buf, this.source.slice(0, bytes));
      this.source = this.source.slice(bytes);
      return buf;
    }

    $$closefh() {
    }
  };

  nqp.op.open = function(name, mode) {
    if (name === '*SOURCE*') {
      return new SourceFileHandle(source);
    } else {
      return oldOpen(name, mode);
    }
  };

  const oldStat = nqp.op.stat;

  const EXISTS = 0;
  const ISDIR = 2;

  nqp.op.stat = function(file, code) {
    if (file === '*SOURCE*' && code === EXISTS) {
      return 1;
    } else if (file === '*SOURCE*' && code === ISDIR) {
      return 0;
    } else {
      return oldStat(file, code);
    }
  };

  let status = 0;

  try {
    code();
  } catch (e) {
    if (e instanceof Exit) {
      status = e.status;
    } else {
      throw e;
    }
  }

  nqp.op.getstdout().constructor.prototype.$$writefh = oldStdoutWritefh;
  nqp.op.getstderr().constructor.prototype.$$writefh = oldStderrWritefh;

  nqp.op.exit = oldExit;
  nqp.op.open = oldOpen;

  return {
    status: status,
    out: Buffer.concat(out).toString(),
    err: Buffer.concat(err).toString()
  };
};
