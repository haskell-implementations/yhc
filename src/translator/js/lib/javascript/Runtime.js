// Javascript Runtime Functions for NHC-based Haskell -> Javascript compiler.

// Runtime support functions.

// Declare a variable which will serve as a constructor index.
// It will be populated later.

var conIdx = { };

// Declare a variable which will serve as a function name index.
// It will be populated later.

var funIdx = { };

// Declare a variable which will serve as a string literals reverse index.
// It will be populated later.

var strIdx = { };                

// Substitute the toSource method if missing

if (!Object.prototype.toSource) Object.prototype.toSource = function () {
  return "<<This browser does not support toSource>>";
}

// Make primitive types behave as thunks.

Number.prototype._c = function () {return this;};
Number.prototype._r = false;
Number.prototype._chr = false;

Boolean.prototype._c = function () {
  return new HSData(this.valueOf() ? true : false,[]);
};
Boolean.prototype._r = true;

// Strings when evaluated return their first character CONS'ed with
// the remainder of the string.

String.prototype._c = function () {
  var cmeth = this._c;
  this._c = function () {
    throw ("B '" + this + "' blackhole: thunk reentered");
  }
  try {
    var res = consStr (this);
    this._r = true;
    this._c = function () {return res;}
    return res;
  } catch (e) {
    this._c = cmeth;
    throw (e);
  }
};

String.prototype._r = true;

// Arrays when evaluated return their first element CONS'ed with
// the remainder of the array.

Array.prototype._c = function () {
  var cmeth = this._c;
  this._c = function () {
    throw ("B [" + this.toString() + "] blackhole: thunk reentered");
  }
  try {
    var res = consArr (this);
    this._r = true;
    this._c = function () {return res;}
    return res;
  } catch (e) {
    this._c = cmeth;
    throw (e);
  }
};

Array.prototype._r = true;
Array.prototype._toArray = function () {return this;};

// Convert a String into its first character CONS'ed with the string remainder.

var consStr = function (s) {
  if (s.length == 0) {
    return new HSEOL ();
  } else {
    var hdc = mkChar (s.charCodeAt (0));
    return new HSCons (hdc, s.length > 1 ? s.substring (1) : new HSEOL ());
  };
};

// Convert an Array into its first element CONS'ed with the array remainder.
    
var consArr = function (ar) {
  if (ar.length == 0) {
    return new HSEOL ();
  } else {
    return new HSCons (ar[0], ar.length > 1 ? ar.slice (1) : consArr ([]));
  };
};

// Create a Number object with a special property pretending that
// this is a character.

function mkChar(c) {
  var n = new Number(c);
  n._chr = true;
  return n;
}

// Object for consistent representation of lists

function HSCons (head,tail) {
  this._r = false;
  this._c = function () { return this; };
  this._t = conIdx['Prelude;:'];
  this._f = [head, tail];
};

HSCons.prototype.toString = function () {
    var evhead = exprEval (this._f[0]);
    if (evhead._chr == true) {
      return mkString(this);
    } else {
      return this._toArray().toString();
    };
};

function mkString(cons) {
  var rs = "";
  var elt = cons;
  while (true) {
    if (!!elt._t && elt._t == 3) {                        /* HSCons */
      rs = rs + String.fromCharCode(exprEval(elt._f[0])); /* char from list head */
      elt = exprEval(elt._f[1]);                          /* remainder of the list */
      continue;
    } else if (!!elt._t && elt._t == 2) {                 /* HSEOL */
      break;                                              /* end of list reached */
    } else {                                              /* any other object */
      rs = rs + exprEval(elt).toString();                 /* try to get it as a string */
      break;                                              /* done */
    }
  };
  return rs;
}


HSCons.prototype._toArray = function () {
    return [exprEval(this._f[0])].concat(exprEval(this._f[1])._toArray ());
  };


function HSEOL() {
  this._r = false;
  this._c = function () {return this; };
  this._t = conIdx['Prelude;[]'];
  this._f = [];
  this.toString = function() { return ""};
  this._toArray = function() {return [];};
}

// A HSFun object represents function body but holds no arguments.
// It may be evaluated only for nullary functions.
// Its _ap method creates a HSDly object referring to this function
// and holding call arguments.

function HSFun (name, arity, body) {
  this._n = name;                     // function name
  this._r = (arity == 0);             // evaluable only for nullary functions
  this._x = arity;                    // function arity
  this._d = this;                     // refer to self
  this._y = 0;                        // actual number of args
  this._u = null;                     // nothing up stack
  this._b = body;                     // function body
}

HSFun.prototype._c = function () {
    if (!this._r) return this;        // nothing to do if not evaluable
    var cmeth = this._c;              // save the _c method
    this._c = function () {           // install blackhole handler
      throw ("B " + this._n + " blackhole: thunk reentered");
    };
    try {
      var res = this._b();            // obtain evaluation result
      this._c = function () {         // replace the _c method to return the result
        return res;                   // without re-evaluation
      };
      return res;                     // return result of this evaluation
    } catch (e) {                     // if an exception is caught
      this._c = cmeth;                // restore the method
      throw (e);                      // rethrow
    }
  };

HSFun.prototype._ap = function (targs) {
    if (targs.length == 0) return this;
    return new HSDly(this, this, targs);
  };


// A HSDly object holds function arguments. It also holds
// a reference to the HSFun to be evaluated when required,
// and also reference to another HSDly (or to the HSFun if first application).
// Its _ap method acts same way as HSFun's does.
// Its _c method retrieves all arguments in proper order,
// evaluates the HSFun object with proper number of arguments applied,
// and returns a new HSDly with all remaining arguments and whatever came out
// from the evaluation of HSFun.

function HSDly (fun, up, targs) {
  this._a = targs;                    // arguments to hold
  this._n = fun._n;                   // function name
  this._d = fun;                      // delayed function object (HSFun)
  this._u = up;                       // up stack (HSFun or HSDly)
  this._y = targs.length + up._y;     // summary number of arguments in the stack
  this._r = (this._y >= fun._x);      // evaluable only if # args >= function arity
}

HSDly.prototype._c = function () {
    if (!this._r) return this;
    var cmeth = this._c;
    this._c = function () {
      throw ("B " + this._n + " blackhole: thunk reentered");
    };
    var cargs = this._a;              // arguments read from the stack
    var stack = this._u;
    for (; stack._u; stack = stack._u) {
      cargs = stack._a.concat(cargs); // concatenate in correct order
    };
    var res;                          // to store result
    try {
      if (cargs.length == this._d._x) { // saturated call
        res = this._d._b.apply(this._d,cargs);
        this._u = null;               // tear the stack apart
        this._c = function () {       // replace the method to return result
          return res;                 // without reevaluation
        };
      } else {                        // oversaturated call
        var evd;
        if (this._d._x == 0) {        // nullary function
          evd = exprEval(this._d);    // evaluate it 
          this._a = cargs;            // carry all the arguments over
        } else {                      // need to feed some arguments
          var feed = cargs.slice (0, this._d._x);
          var over = cargs.slice (this._d._x);
          evd = exprEval(this._d._ap(feed));
          this._a = over;             // and carry the rest over
        }
        this._n = evd._n;             // name of whatever was computed
        this._d = evd._d;             // now refer to whatever it refers to
        this._u = evd;                // now this is our upstack 
        this._y = this._a.length + evd._y; // new total arguments
        this._r = (this._y >= this._d._x); // are we evaluable? 
        res = this;                   // reuse this object
        this._c = cmeth;              // restore the _c method
      }
      return res;                     // return result;
    } catch (e) {                     // if exception is raised
      this._c = cmeth;                // restore method
      throw (e);                      // rethrow
    }
  };

HSDly.prototype._ap = function (targs) {
    if (targs.length == 0) return this;
    return new HSDly(this._d, this, targs);
  };


// Object type for a Haskell data

function HSData (con, arrs) {
  this._r = false;
  this._c = function () { return this; };
  this._t = con;
  this._f = arrs;
};

// Toplevel expression evaluator entry point

function exprEval (e) {
  for (var ex = e; ex != undefined && ex._r ; ex = ex._c())
    ;
  return ex;
};

// Cross-browser version of the `apply' function
// Suggested by Neil, should work around MSIE DOM methods
// that do not support the `apply' method.

function cbrApply(func, thisp, args)
{
       if (func.apply)
               return func.apply(thisp,args);
       else
       {
               var s = "func(";
               for (var i = 0; i < args.length; i++)
                       s += (i == 0 ? "" : ",") + "args[" + i + "]";
               s += ")";
               return eval(s);
       }
}

// Support for XML HTTP requests. Based on 
// http://www.w3schools.com/xml/tryit.asp?filename=try_xmlhttprequest_js4

function newXmlHttpRequest () {
  var xmlhttp;
  xmlhttp = null;
  if (window.XMLHttpRequest) {
    xmlhttp=new XMLHttpRequest();
  } else if (window.ActiveXObject) {
    xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
  } else throw "R XML HTTP Request not supported";
  return xmlhttp;
}

// For some reason, `negate' translates to NEG_x rather than its inlined equivalent.
// Provide support here.


function NEG_W(a) {
  return -(exprEval(a));
}

function NEG_F(a) {
  return -(exprEval(a));
}

function NEG_D(a) {
  return -(exprEval(a));
}

// ORD is compiled as an external primitive: for now assume it returns
// the numeric tag of data constructor

function ORD(a) {
  return (exprEval(a)._t);
}

// Obtain event's target element: it is `target' in Mozilla (per DOM)
// and `srcElement' in MSIE. 
// See also http://www.quirksmode.org/js/events_properties.html

function getEventTarget(e) {
  var targ;
  if (e.target) targ = e.target;
  else targ = e.srcElement;
  if (targ.nodeType == 3) // defeat Safari bug
    targ = targ.parentNode;
  return targ;
}

// A global object to keep references to continuations delayed
// by setTimeout. The setTimeout function returns an unique
// value identifying a delayed conputation, so it may be
// retrieved from within the continuation context.

var delCont = {  };

// A global variable to hold an unique (incremented each time) number
// of a continuation to resume. It is necessary to have this variable
// because the value returned by setTimeout is not available to build
// the string of code to execute at the moment this string is being formed.

var contNum = 0;

// A helper function to implement signum.

function primSignum(a) {
  if (a < 0) return -1;
  if (a > 0) return 1;
  return 0;
}

// MS IE may need this

String.prototype.constructor.name = "String"
Object.prototype.constructor.name = "Object"
Boolean.prototype.constructor.name = "Boolean"
Number.prototype.constructor.name = "Number"
Array.prototype.constructor.name = "Array"
HSCons.prototype.constructor.name = "HSCons"
HSEOL.prototype.constructor.name = "HSEOL"


/*  Version found in CouchDB 0.7.2
    json.js
    2007-08-19

    Public Domain

    This file adds these methods to JavaScript:

        array.toJSONString(whitelist)
        boolean.toJSONString()
        date.toJSONString()
        number.toJSONString()
        object.toJSONString(whitelist)
        string.toJSONString()
            These methods produce a JSON text from a JavaScript value.
            It must not contain any cyclical references. Illegal values
            will be excluded.

            The default conversion for dates is to an ISO string. You can
            add a toJSONString method to any date object to get a different
            representation.

            The object and array methods can take an optional whitelist
            argument. A whitelist is an array of strings. If it is provided,
            keys in objects not found in the whitelist are excluded.

        string.parseJSON(filter)
            This method parses a JSON text to produce an object or
            array. It can throw a SyntaxError exception.

            The optional filter parameter is a function which can filter and
            transform the results. It receives each of the keys and values, and
            its return value is used instead of the original value. If it
            returns what it received, then structure is not modified. If it
            returns undefined then the member is deleted.

            Example:

            // Parse the text. If a key contains the string 'date' then
            // convert the value to a date.

            myData = text.parseJSON(function (key, value) {
                return key.indexOf('date') >= 0 ? new Date(value) : value;
            });

    It is expected that these methods will formally become part of the
    JavaScript Programming Language in the Fourth Edition of the
    ECMAScript standard in 2008.

    This file will break programs with improper for..in loops. See
    http://yuiblog.com/blog/2006/09/26/for-in-intrigue/

    This is a reference implementation. You are free to copy, modify, or
    redistribute.

    Use your own copy. It is extremely unwise to load untrusted third party
    code into your pages.
*/

/*jslint evil: true */

// Augment the basic prototypes if they have not already been augmented.

if (!Object.prototype.toJSONString) {

    Array.prototype.toJSONString = function (w) {
        var a = [],     // The array holding the partial texts.
            i,          // Loop counter.
            l = this.length,
            v;          // The value to be stringified.

// For each value in this array...

        for (i = 0; i < l; i += 1) {
            v = this[i];
            switch (typeof v) {
            case 'object':

// Serialize a JavaScript object value. Ignore objects thats lack the
// toJSONString method. Due to a specification error in ECMAScript,
// typeof null is 'object', so watch out for that case.

                if (v) {
                    if (typeof v.toJSONString === 'function') {
                        a.push(v.toJSONString(w));
                    }
                } else {
                    a.push('null');
                }
                break;

            case 'string':
            case 'number':
            case 'boolean':
                a.push(v.toJSONString());

// Values without a JSON representation are ignored.

            }
        }

// Join all of the member texts together and wrap them in brackets.

        return '[' + a.join(',') + ']';
    };


    Boolean.prototype.toJSONString = function () {
        return String(this);
    };


    Date.prototype.toJSONString = function () {

// Eventually, this method will be based on the date.toISOString method.

        function f(n) {

// Format integers to have at least two digits.

            return n < 10 ? '0' + n : n;
        }

        return '"' + this.getUTCFullYear() + '-' +
                f(this.getUTCMonth() + 1)  + '-' +
                f(this.getUTCDate())       + 'T' +
                f(this.getUTCHours())      + ':' +
                f(this.getUTCMinutes())    + ':' +
                f(this.getUTCSeconds())    + 'Z"';
    };


    Number.prototype.toJSONString = function () {

// JSON numbers must be finite. Encode non-finite numbers as null.

        return isFinite(this) ? String(this) : 'null';
    };


    Object.prototype.toJSONString = function (w) {
        var a = [],     // The array holding the partial texts.
            k,          // The current key.
            i,          // The loop counter.
            v;          // The current value.

// If a whitelist (array of keys) is provided, use it assemble the components
// of the object.

        if (w) {
            for (i = 0; i < w.length; i += 1) {
                k = w[i];
                if (typeof k === 'string') {
                    v = this[k];
                    switch (typeof v) {
                    case 'object':

// Serialize a JavaScript object value. Ignore objects that lack the
// toJSONString method. Due to a specification error in ECMAScript,
// typeof null is 'object', so watch out for that case.

                        if (v) {
                            if (typeof v.toJSONString === 'function') {
                                a.push(k.toJSONString() + ':' +
                                       v.toJSONString(w));
                            }
                        } else {
                            a.push(k.toJSONString() + ':null');
                        }
                        break;

                    case 'string':
                    case 'number':
                    case 'boolean':
                        a.push(k.toJSONString() + ':' + v.toJSONString());

// Values without a JSON representation are ignored.

                    }
                }
            }
        } else {

// Iterate through all of the keys in the object, ignoring the proto chain
// and keys that are not strings.

            for (k in this) {
                if (typeof k === 'string' &&
                        Object.prototype.hasOwnProperty.apply(this, [k])) {
                    v = this[k];
                    switch (typeof v) {
                    case 'object':

// Serialize a JavaScript object value. Ignore objects that lack the
// toJSONString method. Due to a specification error in ECMAScript,
// typeof null is 'object', so watch out for that case.

                        if (v) {
                            if (typeof v.toJSONString === 'function') {
                                a.push(k.toJSONString() + ':' +
                                       v.toJSONString());
                            }
                        } else {
                            a.push(k.toJSONString() + ':null');
                        }
                        break;

                    case 'string':
                    case 'number':
                    case 'boolean':
                        a.push(k.toJSONString() + ':' + v.toJSONString());

// Values without a JSON representation are ignored.

                    }
                }
            }
        }

// Join all of the member texts together and wrap them in braces.

        return '{' + a.join(',') + '}';
    };


    (function (s) {

// Augment String.prototype. We do this in an immediate anonymous function to
// avoid defining global variables.

// m is a table of character substitutions.

        var m = {
            '\b': '\\b',
            '\t': '\\t',
            '\n': '\\n',
            '\f': '\\f',
            '\r': '\\r',
            '"' : '\\"',
            '\\': '\\\\'
        };


        s.parseJSON = function (filter) {
            var j;

            function walk(k, v) {
                var i;
                if (v && typeof v === 'object') {
                    for (i in v) {
                        if (Object.prototype.hasOwnProperty.apply(v, [i])) {
                            v[i] = walk(i, v[i]);
                        }
                    }
                }
                return filter(k, v);
            }


// Parsing happens in three stages. In the first stage, we run the text against
// a regular expression which looks for non-JSON characters. We are especially
// concerned with '()' and 'new' because they can cause invocation, and '='
// because it can cause mutation. But just to be safe, we will reject all
// unexpected characters.

// We split the first stage into 3 regexp operations in order to work around
// crippling deficiencies in Safari's regexp engine. First we replace all
// backslash pairs with '@' (a non-JSON character). Second we delete all of
// the string literals. Third, we look to see if only JSON characters
// remain. If so, then the text is safe for eval.

            if (/^[,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t]*$/.test(this.
                    replace(/\\./g, '@').
                    replace(/"[^"\\\n\r]*"/g, ''))) {

// In the second stage we use the eval function to compile the text into a
// JavaScript structure. The '{' operator is subject to a syntactic ambiguity
// in JavaScript: it can begin a block or an object literal. We wrap the text
// in parens to eliminate the ambiguity.

                j = eval('(' + this + ')');

// In the optional third stage, we recursively walk the new structure, passing
// each name/value pair to a filter function for possible transformation.

                return typeof filter === 'function' ? walk('', j) : j;
            }

// If the text is not JSON parseable, then a SyntaxError is thrown.

            throw new SyntaxError('parseJSON');
        };


        s.toJSONString = function () {

// If the string contains no control characters, no quote characters, and no
// backslash characters, then we can simply slap some quotes around it.
// Otherwise we must also replace the offending characters with safe
// sequences.

            if (/["\\\x00-\x1f]/.test(this)) {
                return '"' + this.replace(/[\x00-\x1f\\"]/g, function (a) {
                    var c = m[a];
                    if (c) {
                        return c;
                    }
                    c = a.charCodeAt();
                    return '\\u00' +
                        Math.floor(c / 16).toString(16) +
                        (c % 16).toString(16);
                }) + '"';
            }
            return '"' + this + '"';
        };
    })(String.prototype);
}


/*
	parseUri 1.2.1
	(c) 2007 Steven Levithan <stevenlevithan.com>
	MIT License
*/

function parseUri (str) {
	var	o   = parseUri.options,
		m   = o.parser[o.strictMode ? "strict" : "loose"].exec(str),
		uri = {},
		i   = 14;

	while (i--) uri[o.key[i]] = m[i] || "";

	uri[o.q.name] = {};
	uri[o.key[12]].replace(o.q.parser, function ($0, $1, $2) {
		if ($1) uri[o.q.name][$1] = $2;
	});

	return uri;
};

parseUri.options = {
	strictMode: false,
	key: ["source","protocol","authority","userInfo","user","password","host","port","relative","path","directory","file","query","anchor"],
	q:   {
		name:   "queryKey",
		parser: /(?:^|&)([^&=]*)=?([^&]*)/g
	},
	parser: {
		strict: /^(?:([^:\/?#]+):)?(?:\/\/((?:(([^:@]*):?([^:@]*))?@)?([^:\/?#]*)(?::(\d*))?))?((((?:[^?#\/]*\/)*)([^?#]*))(?:\?([^#]*))?(?:#(.*))?)/,
		loose:  /^(?:(?![^:@]+:[^:@\/]*@)([^:\/?#.]+):)?(?:\/\/)?((?:(([^:@]*):?([^:@]*))?@)?([^:\/?#]*)(?::(\d*))?)(((\/(?:[^?#](?![^?#\/]*\.[^?#\/.]+(?:[?#]|$)))*\/?)?([^?#\/]*))(?:\?([^#]*))?(?:#(.*))?)/
	}
};

