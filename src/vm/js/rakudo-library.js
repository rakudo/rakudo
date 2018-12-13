const nqp = require('nqp-runtime');

let passedArgs;

let oldArgs;

function fakeArgs(isMain) {
  return isMain ? passedArgs.map(arg => new nqp.NativeStrArg(arg)) : [];
};


const code = /*async*/ function() {
  /*await*/ require('./rakudo.js')(nqp, true);
};

const core = require('nqp-runtime/core.js');

module.exports.compile = function(source, options = {}) {
  const oldGlobalContext = nqp.freshGlobalContext();

  const oldArgs = nqp.args;
  nqp.args = fakeArgs;
  const tmp = require('tmp');
  const tmpFile = tmp.tmpNameSync();

  passedArgs = ['perl6-js', '--output', tmpFile, '--target=js', source];

  if (Object.prototype.hasOwnProperty.call(nqp.op.getstdout(), '$$writefh')) {
    throw `Can't overwrite $$writefh on stdout, it's already set`;
  }

  const output = [];

  nqp.op.getstdout().$$writefh = function(buf) {
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

  delete nqp.op.getstdout().$$writefh;

  const lines = Buffer.concat(output).toString().split(/\n/);

  const loaded = [];

  for (const line of lines) {
    let match;
    if (/^[A-Z0-9]{40}\0/.test(line)) {
    } else if (match = line.match(/^LOAD-UNIT ID:(.*?) DEPS:(.*?) PATH:(.*)/)) {
      const deps = match[2] == '' ? [] : match[2].split(',');
      loaded.push({id: match[1], deps: deps, path: match[3]});
    } else {
      // console.warn('extra line', line);
    }
  }

  nqp.args = oldArgs;

  nqp.setGlobalContext(oldGlobalContext);

  const fs = require('fs');
  return {js: fs.readFileSync(tmpFile, 'utf8'), loaded: loaded};
};

module.exports.capturedRun = /*async*/ function(source, input, compileArgs, args, passedEnv) {
  const oldGlobalContext = nqp.freshGlobalContext();

  const env = nqp.hash();

  const pid = core.randomInt()[0];

  const oldGetpid = nqp.op.getpid;

  nqp.op.getpid = function() {
    return pid;
  };

  passedEnv.content.forEach((value, key, map) => {
    env.content.set(key, new nqp.NQPStr(value.$$getStr()));
  });

  const oldArgs = nqp.args;
  nqp.args = fakeArgs;

  const oldGetEnvHash = nqp.op.getenvhash;

  nqp.op.getenvhash = function() {
    return env;
  };

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

  if (Object.prototype.hasOwnProperty.call(nqp.op.getstdout(), '$$writefh')) {
    throw `Can't overwrite $$writefh on stdout, it's already set`;
  }

  nqp.op.getstdout().$$writefh = function(buf) {
    out.push(core.toRawBuffer(buf));
  };

  if (Object.prototype.hasOwnProperty.call(nqp.op.getstderr(), '$$writefh')) {
    throw `Can't overwrite $$writefh on stderr, it's already set`;
  }

  nqp.op.getstderr().$$writefh = function(buf) {
    err.push(core.toRawBuffer(buf));
  };

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
    /*await*/ code();
  } catch (e) {
    if (e instanceof Exit) {
      status = e.status;
    } else {
      throw e;
    }
  }

  delete nqp.op.getstdout().$$writefh;
  delete nqp.op.getstderr().$$writefh;

  nqp.op.exit = oldExit;
  nqp.op.open = oldOpen;
  nqp.args = oldArgs;
  nqp.op.getenvhash = oldGetEnvHash;
  nqp.op.getpid = oldGetpid;

  nqp.setGlobalContext(oldGlobalContext);

  return {
    status: status,
    out: Buffer.concat(out).toString(),
    err: Buffer.concat(err).toString()
  };
};
