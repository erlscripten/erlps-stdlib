"use static";

var ets_map = {};
var ets_name_map = {};

exports.registerNamed
    = function(name) {
        return function(ref) {
            return function(table) {
                ets_map[ref] = table;
                ets_name_map[name] = ref;
            };
        };
    };

exports.register
    = function(ref) {
        return function(table) {
            ets_map[ref] = table;
        };
    };

exports.getTableImpl
    = function(badargCallback) {
        return function(ref) {
            var t = ets_map[ref];
            if(t) {
                return t;
            } else {
                badargCallback();
            }
        };
    };

exports.solveNameImpl
    = function(nothing) {
        return function(just) {
            return function(name) {
                var t = ets_name_map[name];
                if(t) {
                    return just(t);
                } else {
                    return nothing;
                }
            };
        };
    };

exports.etsDeleteImpl
    = function(badargCallback){
        return function(ref) {
            if(!ets_map[ref]) {
                badargCallback();
            }
            delete(ets_map[ref]);
        };
    };
