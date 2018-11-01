// Profiling support. This script must be added to a page if
// profiling option is on.

// Profiling table: a two-dimensional associative array.

var profTable = {};

// Profiling function, takes two arguments:
//  1. Profiling category
//  2. Function index to profile.

function profile (pcat, pfun) {
  if (!profTable [pcat]) profTable [pcat] = {};
  if (!profTable [pcat] [pfun]) profTable [pcat] [pfun] = 1;
  else (profTable [pcat] [pfun]) ++;
}

// Reset profiling information

function resetProfile () {
  profTable = {};
}

// Read a profiling table.

function readProfile (pcat, look) {
  var tup2;
  if (!conIdx ["Prelude;(,)"]) {
    throw ("P Constructor index for a 2-tuple is not available");
  } else {
    tup2 = conIdx ["Prelude;(,)"];
  }
  if (!profTable [pcat]) return [];
  var ret = new HSEOL ();
  for (var i in profTable [pcat]) {
    var pkey;
    if (look) {
      if (!strIdx [i]) pkey = "{" + i + "}";
      else pkey = strIdx [i];
    } else pkey = i;
    pcnt = Number (profTable [pcat] [i]);
    ret = new HSCons ((new HSData (tup2, [pkey.toString(), pcnt])), ret);
  }
  return ret;
}

