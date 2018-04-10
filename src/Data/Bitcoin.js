var bjs = require('bitcoinjs-lib');
var b58 = require('bs58check');

exports.deriveAddress = function(xpub) {
  var hdNode = bjs.HDNode.fromBase58(xpub);
  return function(n) {
    return nodeToP2shSegwitAddress(hdNode.derive(0).derive(n));
  };
};

function nodeToP2shSegwitAddress(hdNode) {
  var pubkeyBuf = hdNode.keyPair.getPublicKeyBuffer();
  var hash = bjs.crypto.hash160(pubkeyBuf);
  var redeemScript = bjs.script.witnessPubKeyHash.output.encode(hash);
  var hash2 = bjs.crypto.hash160(redeemScript);
  var scriptPubkey = bjs.script.scriptHash.output.encode(hash2);
  return bjs.address.fromOutputScript(scriptPubkey);
}
