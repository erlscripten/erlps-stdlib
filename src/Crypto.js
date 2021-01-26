"use static";

var SHA256 = require("crypto-js/sha256");

exports.sha256Impl = function(buf) {
    return Buffer.from(crypto.createHash('sha256').update(buf).digest('hex'), 'hex');
}
