"use static";

import crypto from "crypto";

export const sha256Impl = function(buf) {
    return Buffer.from(crypto.createHash('sha256').update(buf).digest('hex'), 'hex');
}
