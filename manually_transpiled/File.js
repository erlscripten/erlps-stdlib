"use static";

import * as fs from "./filemap.json";

function simplifyPath(path) {
    var stack = [],
        parts = path.split("/");
    for(var i = 0; i < parts.length; i++) {
        if(parts[i] == ".")
            continue;
        if(parts[i] == "..")
            stack.pop();
        else
            stack.push(parts[i]);
    }
    return stack.join("/");
}

export const readFileImpl = function(makeBinary) {
    return function(enoent) {
	return function(filename) {
	    var filename = simplifyPath(filename);
	    var file = fs[filename];
	    if(file || file === "") {
		return makeBinary(Buffer.from(file))
	    };
	    return enoent;
	}
    }
}
