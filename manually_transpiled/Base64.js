"use static";

exports.bufToU8 = function(buf) { return new Uint8Array(buf); };
exports.u8ToBuf = function(u8) {
    var buf = Buffer.alloc(u8.byteLength);
    for (var i = 0; i < buf.length; ++i) {
        buf[i] = u8[i];
    }
    return buf;
};
