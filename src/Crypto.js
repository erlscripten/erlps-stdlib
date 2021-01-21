"use static";

var SHA256 = require("crypto-js/sha256");

exports.sha256Impl = function(str) {
    return SHA256(str).toString();
}
