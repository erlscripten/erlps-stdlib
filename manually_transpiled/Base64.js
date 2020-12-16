"use static";

exports.bufToU8 = function(buf) { return new Uint8Array(buf); };
exports.u8ToBuf = function(u8) { return u8.buffer; };
