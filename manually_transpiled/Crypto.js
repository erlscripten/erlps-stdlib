"use static";

var crypto = require("crypto");

exports.sha256Impl = function(buf) {
    return Buffer.from(crypto.createHash('sha256').update(buf).digest('hex'), 'hex');
}
