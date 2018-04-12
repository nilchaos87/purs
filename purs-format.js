'use strict';

const fs = require('fs');
const [ src, dest ] = ['src', 'dest'].map(param(process.argv, arg => { throw new Error(`No ${arg} specified!`); }));

fs.readFile(src, 'utf8', function(err, src) {
  if (err) throw err;
  fs.writeFile(dest, format(src), function(err) {
    if (err) throw err;
  });
});

function format(data) {
  return data
    .replace(/forall/g, '∀')
    .replace(/\->/g, '→')
    .replace(/<\-/g, '←')
    .replace(/=>/g, '⇒')
    .replace(/<=/g, '⇐')
    .replace(/::/g, '∷');
}

function param(args, onError) {
  return function(name) {
    const arg = `--${name}`;
    const index = args.indexOf(arg);

    if (index === -1) {
      return onError(arg);
    }

    const val = args[index+1];

    if (!val) {
      return onError(arg);
    }

    return val;
  }
}
