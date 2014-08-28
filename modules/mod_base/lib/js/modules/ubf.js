/* UBF(A) encoder/decoder

@package: Channel.me 2013
@Author: MM Zeeman <mmzeeman@xs4all.nl.nl>

Copyright 2013-2014 Maas-Maarten Zeeman

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

*/

;(function(window, $) {
    var window = window;
    var $ = $;
    var ubf = {};
    var specs = {};

    ubf.BINARY = 0;
    ubf.CONSTANT = 1;
    ubf.STRING = 2;
    ubf.TUPLE = 3;
    ubf.LIST = 4;
    ubf.OPCODE = 5;

    DecodeStack = function(start) {
        this._stack = start || [];
        this._markers = [];
    };
    DecodeStack.prototype.length = function() {
        return this._stack.length;
    };
    DecodeStack.prototype.push = function(v) {
        this._stack.push(v);
    };
    DecodeStack.prototype.pop = function() {
        return this._stack.pop();
    };
    DecodeStack.prototype.push_offset = function() {
        this._markers.push(this._stack.length);
    };
    DecodeStack.prototype.pop_offset_diff = function() {
        return this._stack.length - this._markers.pop();
    };


    // Make a constant
    function constant(value) {
        var s = new String(value);
        s.ubf_type = ubf.CONSTANT;
        return s;
    }
    ubf.constant = constant;

    // Encode the value as a ubf(a) tuple. 
    function encode_as_tuple(value, spec, buffer) {
        if (value._record) {
            encode_as_record(value, value._record, spec || specs[value._record], buf);
        } else {
            var buf = buffer || [];
            var inner = [];
            buf.push('{');
            if (spec)
                $.each(spec, function(_i, k) { encode(value[k], inner); });
            else
                $.each(value, function(_i, v) { encode(v, inner); });

            buf.push(inner.join(","));
            buf.push('}');

            if(!buffer) {
                buf.push("$");
                return buf.join("");
            }
        }
    }
    ubf.encode_as_tuple = encode_as_tuple;

    // Encode the value as list
    function encode_as_list(value, buffer) {
        var buf = buffer || [];
        var i;

        buf.push("#");
        for(i=value.length-1; i >= 0; i--) {
            encode(value[i], buf);
            buf.push('&');
        }

        if(!buffer) {
            buf.push("$");
            return buf.join("");
        }
    }
    ubf.encode_as_list = encode_as_list;

    // Encode as proplist with {key,value} tuples.
    function encode_as_proplist(value, buffer) {
        var buf = buffer || [];
        var i;

        buf.push("#");
        $.each(value, function(k, v) {
                buf.push('{');
                encode(k, buf);
                buf.push(' ');
                encode(v, buf);
                buf.push('}&');
            });

        if(!buffer) {
            buf.push("$");
            return buf.join("");
        }
    }
    ubf.encode_as_proplist = encode_as_proplist;

    // Encode as record
    function encode_as_record(value, record_name, spec, buffer) {
        var buf = buffer || [], inner = [];
        spec = spec || specs[record_name];

        buf.push('{');
        if(spec) {
            encode_as_constant(record_name, inner);
            $.each(spec, function(_i, k) {
                encode(value[k], inner);
            });
        } else {
            $.each(value, function(_i, v) {
                encode(v, inner);
            });
        }
        buf.push(inner.join(","));
        buf.push('}');

        if(!buffer) {
            buf.push("$");
            return buf.join("");
        }
    }
    ubf.encode_as_record = encode_as_record;

    function string_escape(s) {
        if(s === undefined) return "";
        return s.replace(/\\/g, "\\\\").replace(/\"/g, '\\"');
    }

    function constant_escape(s) {
        if(s === undefined) return "";
        return s.replace(/\\/g, "\\\\").replace(/\'/g, "\\'");
    }

    function encode_as_string(value, buffer) {
        buffer.push(['"', string_escape(value), '"'].join(""));
    }
    ubf.encode_as_string = encode_as_string;

    function encode_as_binary(value, buffer) {
        buffer.push(_utf8len(value)+"~"+value+"~");
    }
    ubf.encode_as_binary = encode_as_binary;

    function encode_as_constant(value, buffer) {
        buffer.push(["'", constant_escape(value), "'"].join(""));
    }
    ubf.encode_as_constant = encode_as_constant;

    // ubf(a) encode javascript to ubf.
    //
    function encode(value, buffer) {
        var buf = buffer || [];

        if(value === undefined || value === null) {
            encode_as_constant("undefined", buf);
        } else {
            switch(value.ubf_type) {
            case ubf.STRING:
                encode_as_string(value, buf);
                break;
            case ubf.CONSTANT:
                encode_as_constant(value, buf);
                break;
            case ubf.BINARY:
                encode_as_binary(value, buf);
                break;
            case ubf.LIST:
                encode_as_list(value, buf);
                break;
            case ubf.TUPLE:
                encode_as_tuple(value, undefined, buf);
                break;
            case ubf.OPCODE:
                buf.push(value);
            default:
                if($.isArray(value)) {
                    encode_as_list(value, buf);
                } else if(typeof(value) == "number") {
                    // floats are not possible in UBF...
                    buf.push(Math.floor(value));
                } else if(typeof(value) == "string") {
                    // Per default encode strings as binary - better on the server
                    buf.push(_utf8len(value)+"~"+value+"~");
                } else if (typeof(value) == "object" && value._record) {
                    encode_as_record(value, value._record, specs[value._record], buf);
                } else if(typeof(value) == "object") {
                    var keys = $.keys(value).sort();
                    if (keys.length == 2 && keys[0] == 'name' && keys[1] == 'value') {
                        encode_as_tuple([value.name, value.value], undefined, buf);
                    } else {
                        encode_as_proplist(value, buf);
                    }
                } else if(typeof(value) == "object" && value.valueOf) {
                    buf.push(_utf8len(value.valueOf)+"~"+value.valueOf()+"~");
                } else if(typeof(value) == "boolean") {
                    encode_as_constant((value)?"true":"false", buf);
                } else if(value === null) {
                    encode_as_constant("undefined", buf);
                } else {
                    throw("ubf encode: unknown value");
                }
            }
        }

        if(!buffer) {
            buf.push("$");
            return buf.join("");
        }
    }
    ubf.encode = encode;

    // ubf(a) decoder
    //
    // 
    ubf.decode = function(bytes, ready_fun, env, startStack) {
        var j, opcode;

        stack = new DecodeStack(startStack);
        env = env || {};

        try {
            while(true) {
                opcode = bytes[0];
                j = _operation(opcode, bytes, env, stack);
                if(j === 0) {
                    if(ready_fun) {
                        data = stack.pop();
                        ready_fun(data);
                        bytes = bytes.slice(1);
                        if(bytes.length === 0)
                            return data;
                    } else {
                        return stack.pop();
                    }
                }
                bytes = bytes.slice(j);
            }
        }
        catch(err) {
            console.log(err, stack, bytes);
            throw err;
        }
    };

    /* A spec is a list of field names */
    ubf.add_spec = function(name, spec) {
        specs[name] = spec;
    };

    function _read(bytes, terminator, type, stack) {
        var current, buf = [], i = 0;

        while(true) {
            current = bytes[i];
            
            if(current === undefined)
                throw "Missing terminator";
            
            if(current == "\\") {
                switch(bytes[i+1]) {
                    case "\\":
                        buf.push("\\");
                        break;
                    case terminator:
                        buf.push(terminator);
                        break;
                    default:
                        throw "Wrong " + type + " escape sequence";
                }
                i += 2;
                continue;
            }

            if(current == terminator) {
                if(stack) {
                    var obj;
                    var buf_s = buf.join("");
                    if (type == ubf.STRING) {
                        obj = buf_s;
                    } else {
                        if (type == ubf.CONSTANT) {
                            switch (buf_s)
                            {
                                case 'false':     obj = false; break;
                                case 'true':      obj = true; break;
                                case 'undefined': break;
                                default:
                                    obj = new String(buf_s);
                                    obj.ubf_type = type;
                                    break;
                            }
                        } else {
                            obj = new String(buf_s);
                            obj.ubf_type = type;
                        }
                    }
                    stack.push(obj);
                }
                return i + 1;
            }

            buf.push(current);
            i += 1;
        }
    }

    function skip_ws(bytes) {
        if(!bytes) return 0;
        var ws = bytes.match(/^(\s|,)+/);
        if(ws)
            return ws[0].length;
        return 0;
    }

    function _integer_or_binary_data(bytes, stack) {
        var found = bytes.match(/^\-?[0-9]+/)[0],
            integer = Number(found),
            length = found.length,
            rest = bytes.slice(length),
            ws_length = skip_ws(rest);

        if(rest[ws_length] != "~") {
            stack.push(integer);
            return length;
        }
        // assume input was utf-8 data, correct for decoded JS utf-16 chars
        var charct = _binarychars(rest, ws_length+1, integer);
        var binary = rest.slice(ws_length + 1, ws_length + charct + 1);
        stack.push(binary);

        if(rest[ws_length+1+charct] != "~")
            throw "UBF decode: missing closing ~";

        return length + ws_length + charct + 2;
    }

    function _string(bytes, stack) {
        return _read(bytes.slice(1), '"', ubf.STRING, stack) + 1;
    }

    function _constant(bytes, stack) {
        return _read(bytes.slice(1), "'", ubf.CONSTANT, stack) + 1;
    }

    function _start_tuple(stack) {
        stack.push_offset(); // marker for building a tuple
        return 1;
    }

    function _end_tuple(stack) {
        // pop items from the stack until we find NaN
        var tuple = [];
        tuple.ubf_type = ubf.TUPLE;

        var obj;
        var ct = stack.pop_offset_diff();

        if (ct < 0) {
            console.log("UBF decode error - empty stack for tuple", stack);
            throw "UBF decode: Empty stack on tuple";
        }
        while (ct--) {
            obj = stack.pop();
            tuple.unshift(obj);
        }
        if (tuple[0] &&
            tuple[0].ubf_type == ubf.CONSTANT &&
            typeof specs[tuple[0].valueOf()] !== 'undefined')
        {
            var rec = { _record: tuple[0].valueOf() };
            $.each(specs[tuple[0]], function(i, k) {
                rec[k] = tuple[i+1];
            });
            stack.push(rec);
        } else {
            stack.push(tuple);
        }
        return 1;
    }

    function _push_nil(stack) {
        var list = [];
        list.ubf_type = ubf.LIST;
        stack.push(list);
        return 1;
    }

    function _push_element(stack) {
        var obj = stack.pop(), list = stack.pop();
        if(list.ubf_type != ubf.LIST) throw "Push error: not a list";
        list.unshift(obj);
        stack.push(list);
        return 1;
    }

    function _comment(bytes) {
        return _read(bytes.slice(1), "%") + 1;
    }

    function _pop(bytes, env, stack) {
        var code = bytes[1];
        env[code] = stack.pop();
        return 2;
    }

    function _push(bytes, env, stack) {
        var code = bytes[0];
        if(!env.hasOwnProperty(code))
            throw "Unknown register value: " + code;
        stack.push(env[code]);
        return 1;
    }

    function _return(stack) {
        if(stack.length() == 1)
            return 0;
        throw "The stack should contain one item";
    }

    function _operation(opcode, bytes, env, stack) {
        switch(opcode) {
        case " ":case "\r":case"\n":case"\t":case",": return skip_ws(bytes);
        case "-":
        case"0":case"1":case"2":case"3":case"4":
        case"5":case"6":case"7":case"8":case"9": return _integer_or_binary_data(bytes, stack);
        case '"': return _string(bytes, stack);
        case "'": return _constant(bytes, stack);
        case "{": return _start_tuple(stack);
        case "}": return _end_tuple(stack);
        case "#": return _push_nil(stack);
        case "&": return _push_element(stack);
        case "%": return _comment(bytes);
        case ">": return _pop(bytes, env, stack);
        case "$": return _return(stack);
        default: return _push(bytes, env, stack);
        }
    }
    
    function _utf8len ( s )
    {
        var n = 0;
        for (var i = 0; i < s.length; i++) {
            var code = s.charCodeAt(i);
            if (code <= 0x7f) n++;
            else if (code <= 0x7ff) n += 2;
            else if (code >= 0xd800 && code <= 0xdfff) {
                n += 4;
                i++;
            }
            else if (code < 0xffff) n += 3;
            else n += 4;
        }
        return n;
    }

    function _binarychars ( s, offset, bytect )
    {
        var i = offset;
        while (bytect > 0)
        {
            var code = s.charCodeAt(i++);
            if (code <= 0x7f) bytect--;
            else if (code <= 0x7ff) bytect -= 2;
            else if (code >= 0xd800 && code <= 0xdfff) {
                bytect -= 4;
                i++;
            }
            else if (code < 0xffff) bytect -= 3;
            else bytect -= 4;
        }
        return i - offset;
    }

    window.ubf = ubf;
})(window, jQuery);
