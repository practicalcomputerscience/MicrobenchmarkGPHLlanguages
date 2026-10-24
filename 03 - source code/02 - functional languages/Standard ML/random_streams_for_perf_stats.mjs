import {stdout} from "node:process";
import {createWriteStream} from "node:fs";
function _list(a) {
    let x = null;
    for (let i = a.length - 1; i >= 0; --i) {
        x = [a[i], x];
    }
    return x;
}
function _Match_tag() {}
_Match_tag.prototype.name = "Match";
const _Match = new _Match_tag();
function _Div_tag() {}
_Div_tag.prototype.name = "Div";
const _Div = new _Div_tag();
function _Overflow_tag() {}
_Overflow_tag.prototype.name = "Overflow";
const _Overflow = new _Overflow_tag();
function _Size_tag() {}
_Size_tag.prototype.name = "Size";
const _Size = new _Size_tag();
function _Subscript_tag() {}
_Subscript_tag.prototype.name = "Subscript";
const _Subscript = new _Subscript_tag();
function _String_EQUAL(s, t) {
    if (s === t) { return true; }
    const n = s.length;
    if (n !== t.length) { return false; }
    for (let i = 0; i < n; ++i) {
        if (s[i] !== t[i]) {
            return false;
        }
    }
    return true;
}
const MIN_INT54 = -0x20000000000000;
const MAX_INT54 = 0x1fffffffffffff;
function _Int54_add(x, y) {
    const z = x + y;
    if ((MIN_INT54 < z && z <= MAX_INT54) || (z === MIN_INT54 && (x & 1) === (y & 1))) {
        return z;
    } else {
        throw _Overflow;
    }
}
function _Int54_sub(x, y) {
    const z = x - y;
    if ((MIN_INT54 < z && z <= MAX_INT54) || (z === MIN_INT54 && (x & 1) === (y & 1))) {
        return z;
    } else {
        throw _Overflow;
    }
}
function _Int54_mul(x, y) {
    const z = 0 + x * y;
    if ((MIN_INT54 < z && z <= MAX_INT54) || (z === MIN_INT54 && ((x & 1) === 0 || (y & 1) === 0))) {
        return z;
    } else {
        throw _Overflow;
    }
}
function _Int54_mod(x, y) {
    if (y === 0) {
        throw _Div;
    } else {
        const r = 0 + x % y;
        if (r === 0 || x * y >= 0) {
            return r;
        } else {
            return r + y;
        }
    }
}
const Math_imul = Math.imul;
function _String_append(a, b) {
    if (a.length === 0) { return b; }
    if (b.length === 0) { return a; }
    const c = new Uint8Array(a.length + b.length);
    c.set(a);
    c.set(b, a.length);
    return c;
}
function _String_concat(xs) {
    let n = 0;
    let xs0 = xs;
    while (xs0 !== null) {
        const s = xs0[0];
        n += s.length;
        xs0 = xs0[1];
    }
    const a = new Uint8Array(n);
    let m = 0;
    while (xs !== null) {
        const s = xs[0];
        a.set(s, m);
        m += s.length;
        xs = xs[1];
    }
    return a;
}
function _String_implode(xs) {
    let n = 0;
    let xs0 = xs;
    while (xs0 !== null) {
        ++n;
        xs0 = xs0[1];
    }
    const a = new Uint8Array(n);
    let i = 0;
    while (xs !== null) {
        a[i] = xs[0];
        xs = xs[1];
        ++i;
    }
    return a;
}
function _Array_array(n, init) {
    if (n < 0 || n > 0xffffffff) {
        throw _Size;
    }
    const a = new Array(n);
    a.fill(init);
    return a;
}
function _VectorOrArray_fromList(xs) {
    const a = [];
    while (xs !== null) {
        a.push(xs[0]);
        xs = xs[1];
    }
    return a;
}
function _PromptTag() {
}
const _topLevel = new _PromptTag();
const _emptySeq = { tag: "nil" };
function _consPrompt(p, xs) {
    return { tag: "prompt", p: p, tail: xs };
}
function _consCont(k, exh, xs) {
    return { tag: "cont", k: k, exh: exh, tail: xs }
}
function _appendSeq(xs, ys) {
    let a = [];
    while (xs.tag !== "nil") {
        a.push(xs);
        xs = xs.tail;
    }
    let zs = ys;
    for (let i = a.length - 1; i >= 0; --i) {
        let x = a[i];
        if (x.tag === "cont") {
            zs = _consCont(x.k, x.exh, zs);
        } else {
            zs = _consPrompt(x.p, zs);
        }
    }
    return zs;
}
function _splitSeq(p, xs) {
    let a = [];
    for (;;) {
        if (xs.tag === "nil") {
            throw new Error("Prompt was not found on the stack");
        } else if (xs.tag === "prompt" && xs.p === p) {
            let ys = _emptySeq;
            for (let i = a.length - 1; i >= 0; --i) {
                let x = a[i];
                if (x.tag === "cont") {
                    ys = _consCont(x.k, x.exh, ys);
                } else {
                    ys = _consPrompt(x.p, ys);
                }
            }
            return [ys, xs.tail];
        } else {
            a.push(xs);
            xs = xs.tail;
        }
    }
}
let _metaCont = _emptySeq;
let _exh = undefined;
function _underflow(v) {
    while (_metaCont.tag !== "nil") {
        if (_metaCont.tag === "cont") {
            const k = _metaCont.k;
            _exh = _metaCont.exh;
            _metaCont = _metaCont.tail;
            return [false, k, [v]];
        }
        _metaCont = _metaCont.tail;
    }
    return [true, v];
}
function _throw(e) {
    while (_metaCont.tag !== "nil") {
        if (_metaCont.tag === "cont") {
            const exh = _metaCont.exh;
            _metaCont = _metaCont.tail;
            return [false, exh, [e]];
        }
        _metaCont = _metaCont.tail;
    }
    throw e;
}
function _pushPrompt(k, p, f) {
    /* f : unit -> 'a */
    _metaCont = _consPrompt(p, _consCont(k, _exh, _metaCont));
    _exh = _throw;
    return [false, f, [_underflow, undefined]];
}
function _withSubCont(k, p, f) {
    /* f : ('a,'b) subcont -> 'b */
    const [aboveP, belowP] = _splitSeq(p, _metaCont);
    const exh_old = _exh;
    _metaCont = belowP;
    _exh = _throw;
    return [false, f, [_underflow, _consCont(k, exh_old, aboveP)]];
}
function _pushSubCont(k, subcont, f) {
    /* f : unit -> 'a */
    _metaCont = _appendSeq(subcont, _consCont(k, _exh, _metaCont));
    _exh = _throw;
    return [false, f, [_underflow, undefined]];
}
function _run(f, topLevel) {
    const metaCont_old = _metaCont;
    const exh_old = _exh;
    _metaCont = topLevel ? _consPrompt(_topLevel, _emptySeq) : _emptySeq;
    _exh = undefined;
    let r;
    try {
        r = f(result => [true, result]);
    } catch (e) {
        if (typeof _exh === "undefined") {
            _metaCont = metaCont_old;
            _exh = exh_old;
            throw e;
        } else {
            r = [false, _exh, [e]];
        }
    }
    while (!r[0]) {
        try {
            while (!r[0]) {
                r = r[1].apply(undefined, r[2]);
            }
        } catch (e) {
            if (typeof _exh === "undefined") {
                _metaCont = metaCont_old;
                _exh = exh_old;
                throw e;
            } else {
                r = [false, _exh, [e]];
            }
        }
    }
    _metaCont = metaCont_old;
    _exh = exh_old;
    return r[1];
}
function _function(f) {
    return function() {
        const metaCont_old = _metaCont;
        const exh_old = _exh;
        _metaCont = _emptySeq;
        _exh = undefined;
        let r;
        try {
            r = f(result => [true, result], arguments);
        } catch (e) {
            if (typeof _exh === "undefined") {
                _metaCont = metaCont_old;
                _exh = exh_old;
                throw e;
            } else {
                r = [false, _exh, [e]];
            }
        }
        while (!r[0]) {
            try {
                while (!r[0]) {
                    r = r[1].apply(undefined, r[2]);
                }
            } catch (e) {
                if (typeof _exh === "undefined") {
                    _metaCont = metaCont_old;
                    _exh = exh_old;
                    throw e;
                } else {
                    r = [false, _exh, [e]];
                }
            }
        }
        _metaCont = metaCont_old;
        _exh = exh_old;
        return r[1];
    };
}
_run(function(cont) {
 const $COLON$COLON = function(cont2, a) {
  const x = a[0];
  return [false, cont2, [[x, a[1]]]];
 };
 const Chr_tag = function() {
 };
 Chr_tag.prototype.name = "Chr";
 const Chr = new Chr_tag();
 const LESS = "LESS";
 const EQUAL = "EQUAL";
 const GREATER = "GREATER";
 const NONE = {tag: "NONE"};
 const revAppend = function(cont2, tmp4, ys) {
  let tmp5 = tmp4, tmp6 = ys;
  tmp: for (;;) {
   const tmp7 = tmp5, ys1 = tmp6;
   if (tmp7 === null) {
    return [false, cont2, [ys1]];
   } else if (! (tmp7 === null)) {
    const tmp8 = tmp7[0];
    const tmp9 = tmp7[1];
    [tmp5, tmp6] = [tmp9, [tmp8, ys1]];
    continue tmp;
   } else {
    throw _Match;
   }
  }
 };
 let map;
 map = function(cont2, a) {
  return [false, cont2, [function(cont3, a1) {
   if (a1 === null) {
    return [false, cont3, [null]];
   } else if (! (a1 === null)) {
    const tmp4 = a1[0];
    const tmp5 = a1[1];
    const cont4 = function(tmp6) {
     const cont5 = function(tmp7) {
      const cont6 = function(tmp8) {
       return [false, cont3, [[tmp6, tmp8]]];
      };
      return [false, tmp7, [cont6, tmp5]];
     };
     return [false, map, [cont5, a]];
    };
    return [false, a, [cont4, tmp4]];
   } else {
    throw _Match;
   }
  }]];
 };
 const tabulate = function(cont2, n, f) {
  if (n < 0) {
   throw _Size;
  } else if (n < 10) {
   let go;
   go = function(cont3, a) {
    if (a >= n) {
     return [false, cont3, [null]];
    } else {
     const cont4 = function(tmp4) {
      const cont5 = function(tmp5) {
       return [false, cont3, [[tmp4, tmp5]]];
      };
      return [false, go, [cont5, _Int54_add(a, 1)]];
     };
     return [false, f, [cont4, a]];
    }
   };
   return [false, go, [cont2, 0]];
  } else {
   let cont3;
   cont3 = function(i, acc) {
    if (i >= n) {
     return [false, revAppend, [cont2, acc, null]];
    } else {
     const tmp4 = _Int54_add(i, 1);
     const cont4 = function(tmp5) {
      return [false, cont3, [tmp4, [tmp5, acc]]];
     };
     return [false, f, [cont4, i]];
    }
   };
   return [false, cont3, [0, null]];
  }
 };
 const sub = function(cont2, a) {
  const arr = a[0];
  return [false, cont2, [arr[a[1]]]];
 };
 const update = function(cont2, a) {
  const arr = a[0];
  const i = a[1];
  arr[i] = a[2];
  return [false, cont2, [undefined]];
 };
 const tmp = globalThis.Number;
 const tmp1 = globalThis.BigInt;
 const tmp2 = tmp1.asUintN;
 const tmp3 = globalThis.Date.now;
 const size = function(cont2, a) {
  return [false, cont2, [a.length]];
 };
 const sub1 = function(cont2, a) {
  const s = a[0];
  const i = a[1];
  if (i < 0 || s.length <= i) {
   throw _Subscript;
  } else {
   return [false, cont2, [s[i]]];
  }
 };
 const concat = function(cont2, a) {
  return [false, cont2, [_String_concat(a)]];
 };
 const implode = function(cont2, a) {
  return [false, cont2, [_String_implode(a)]];
 };
 const length = function(cont2, a) {
  return [false, cont2, [a.length]];
 };
 const update1 = function(cont2, arr, i, value) {
  if (i < 0 || arr.length <= i) {
   throw _Subscript;
  } else {
   arr[i] = value;
   return [false, cont2, [undefined]];
  }
 };
 const array = function(cont2, a) {
  const n = a[0];
  return [false, cont2, [_Array_array(n, a[1])]];
 };
 const fromList = function(cont2, a) {
  return [false, cont2, [_VectorOrArray_fromList(a)]];
 };
 const substring = function(cont2, s, i, j) {
  let tmp4;
  cont: {
   if (i < 0) {
    tmp4 = true;
    break cont;
   } else if (j < 0) {
    tmp4 = true;
    break cont;
   } else {
    const tmp5 = s.length;
    tmp4 = tmp5 < _Int54_add(i, j);
    break cont;
   }
  }
  if (tmp4) {
   throw _Subscript;
  } else {
   return [false, cont2, [s.subarray(i, _Int54_add(i, j))]];
  }
 };
 const padLeft = function(cont2, a, a1) {
  return [false, cont2, [function(cont3, a2) {
   if (a2.length >= a1 || a1 <= 0) {
    return [false, cont3, [a2]];
   } else {
    const tmp4 = Uint8Array.of(a);
    let tmp5 = _Int54_sub(a1, a2.length), tmp6 = _list([a2]);
    tmp: for (;;) {
     const j = tmp5, acc = tmp6;
     if (j <= 0) {
      return [false, cont3, [_String_concat(acc)]];
     } else {
      [tmp5, tmp6] = [_Int54_sub(j, 1), [tmp4, acc]];
      continue tmp;
     }
    }
   }
  }]];
 };
 const foldr = function(cont2, a, a1) {
  return [false, cont2, [function(cont3, a2) {
   let cont4;
   cont4 = function(i, acc) {
    if (i < 0) {
     return [false, cont3, [acc]];
    } else {
     const tmp4 = _Int54_sub(i, 1);
     const cont5 = function(tmp5) {
      return [false, cont4, [tmp4, tmp5]];
     };
     return [false, a, [cont5, [a2[i], acc]]];
    }
   };
   return [false, cont4, [_Int54_sub(a2.length, 1), a1]];
  }]];
 };
 const MonoSequence = function(cont2, fromList1, length1, maxLen, tmp4, create, tmp5, tmp6, tmp7, tmp8, fromList2, length2, maxLen1, tmp9, vector, tmp10, tmp11, tmp12) {
  const tabulate1 = function(cont3, a) {
   const n = a[0];
   const cont4 = function(tmp13) {
    return [false, tmp10, [cont3, [n, tmp13]]];
   };
   return [false, tabulate, [cont4, n, a[1]]];
  };
  const sub2 = function(cont3, a) {
   const v = a[0];
   const i = a[1];
   const cont4 = function(tmp13) {
    if (tmp13) {
     return [false, tmp12, [cont3, [v, i]]];
    } else {
     throw _Subscript;
    }
   };
   if (0 <= i) {
    const cont5 = function(tmp13) {
     return [false, cont4, [i < tmp13]];
    };
    return [false, length2, [cont5, v]];
   } else {
    return [false, cont4, [false]];
   }
  };
  const update2 = function(cont3, a) {
   const v = a[0];
   const i = a[1];
   const x = a[2];
   const cont4 = function(tmp13) {
    const cont5 = function(tmp14) {
     const tmp15 = _Int54_add(i, 1);
     const cont6 = function(tmp16) {
      const cont7 = function(tmp17) {
       return [false, tmp8, [cont3, _list([tmp13, tmp14, tmp17])]];
      };
      return [false, tmp9, [cont7, {base: v, length: _Int54_sub(_Int54_sub(tmp16, i), 1), start: tmp15}]];
     };
     return [false, length2, [cont6, v]];
    };
    return [false, fromList2, [cont5, _list([x])]];
   };
   return [false, tmp9, [cont4, {base: v, length: i, start: 0}]];
  };
  const appi = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const cont5 = function(n) {
     let cont6;
     cont6 = function(a2) {
      if (a2 >= n) {
       return [false, cont4, [undefined]];
      } else {
       const cont7 = function(tmp13) {
	const cont8 = function(tmp14) {
	 return [false, cont6, [_Int54_add(a2, 1)]];
	};
	return [false, a, [cont8, [a2, tmp13]]];
       };
       return [false, tmp12, [cont7, [a1, a2]]];
      }
     };
     return [false, cont6, [0]];
    };
    return [false, length2, [cont5, a1]];
   }]];
  };
  const app = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const cont5 = function(n) {
     let cont6;
     cont6 = function(a2) {
      if (a2 >= n) {
       return [false, cont4, [undefined]];
      } else {
       const cont7 = function(tmp13) {
	const cont8 = function(tmp14) {
	 return [false, cont6, [_Int54_add(a2, 1)]];
	};
	return [false, a, [cont8, tmp13]];
       };
       return [false, tmp12, [cont7, [a1, a2]]];
      }
     };
     return [false, cont6, [0]];
    };
    return [false, length2, [cont5, a1]];
   }]];
  };
  const mapi = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const cont5 = function(n) {
     const cont6 = function(tmp13) {
      return [false, tmp10, [cont4, [n, tmp13]]];
     };
     return [false, tabulate, [cont6, n, function(cont7, i) {
      const cont8 = function(tmp13) {
       return [false, a, [cont7, [i, tmp13]]];
      };
      return [false, tmp12, [cont8, [a1, i]]];
     }]];
    };
    return [false, length2, [cont5, a1]];
   }]];
  };
  const map1 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const cont5 = function(n) {
     const cont6 = function(tmp13) {
      return [false, tmp10, [cont4, [n, tmp13]]];
     };
     return [false, tabulate, [cont6, n, function(cont7, i) {
      const cont8 = function(tmp13) {
       return [false, a, [cont7, tmp13]];
      };
      return [false, tmp12, [cont8, [a1, i]]];
     }]];
    };
    return [false, length2, [cont5, a1]];
   }]];
  };
  const foldli = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    return [false, cont4, [function(cont5, a2) {
     const cont6 = function(n) {
      let cont7;
      cont7 = function(i, acc) {
       if (i >= n) {
	return [false, cont5, [acc]];
       } else {
	const tmp13 = _Int54_add(i, 1);
	const cont8 = function(tmp14) {
	 const cont9 = function(tmp15) {
	  return [false, cont7, [tmp13, tmp15]];
	 };
	 return [false, a, [cont9, [i, tmp14, acc]]];
	};
	return [false, tmp12, [cont8, [a2, i]]];
       }
      };
      return [false, cont7, [0, a1]];
     };
     return [false, length2, [cont6, a2]];
    }]];
   }]];
  };
  const foldri = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    return [false, cont4, [function(cont5, a2) {
     const cont6 = function(tmp13) {
      let cont7;
      cont7 = function(i, acc) {
       if (i < 0) {
	return [false, cont5, [acc]];
       } else {
	const tmp14 = _Int54_sub(i, 1);
	const cont8 = function(tmp15) {
	 const cont9 = function(tmp16) {
	  return [false, cont7, [tmp14, tmp16]];
	 };
	 return [false, a, [cont9, [i, tmp15, acc]]];
	};
	return [false, tmp12, [cont8, [a2, i]]];
       }
      };
      return [false, cont7, [_Int54_sub(tmp13, 1), a1]];
     };
     return [false, length2, [cont6, a2]];
    }]];
   }]];
  };
  const foldl = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    return [false, cont4, [function(cont5, a2) {
     const cont6 = function(n) {
      let cont7;
      cont7 = function(i, acc) {
       if (i >= n) {
	return [false, cont5, [acc]];
       } else {
	const tmp13 = _Int54_add(i, 1);
	const cont8 = function(tmp14) {
	 const cont9 = function(tmp15) {
	  return [false, cont7, [tmp13, tmp15]];
	 };
	 return [false, a, [cont9, [tmp14, acc]]];
	};
	return [false, tmp12, [cont8, [a2, i]]];
       }
      };
      return [false, cont7, [0, a1]];
     };
     return [false, length2, [cont6, a2]];
    }]];
   }]];
  };
  const foldr1 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    return [false, cont4, [function(cont5, a2) {
     const cont6 = function(tmp13) {
      let cont7;
      cont7 = function(i, acc) {
       if (i < 0) {
	return [false, cont5, [acc]];
       } else {
	const tmp14 = _Int54_sub(i, 1);
	const cont8 = function(tmp15) {
	 const cont9 = function(tmp16) {
	  return [false, cont7, [tmp14, tmp16]];
	 };
	 return [false, a, [cont9, [tmp15, acc]]];
	};
	return [false, tmp12, [cont8, [a2, i]]];
       }
      };
      return [false, cont7, [_Int54_sub(tmp13, 1), a1]];
     };
     return [false, length2, [cont6, a2]];
    }]];
   }]];
  };
  const findi = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const cont5 = function(n) {
     let cont6;
     cont6 = function(a2) {
      if (a2 >= n) {
       return [false, cont4, [NONE]];
      } else {
       const cont7 = function(x) {
	const cont8 = function(tmp13) {
	 if (tmp13) {
	  return [false, cont4, [{tag: "SOME", payload: [a2, x]}]];
	 } else {
	  return [false, cont6, [_Int54_add(a2, 1)]];
	 }
	};
	return [false, a, [cont8, [a2, x]]];
       };
       return [false, tmp12, [cont7, [a1, a2]]];
      }
     };
     return [false, cont6, [0]];
    };
    return [false, length2, [cont5, a1]];
   }]];
  };
  const find = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const cont5 = function(n) {
     let cont6;
     cont6 = function(a2) {
      if (a2 >= n) {
       return [false, cont4, [NONE]];
      } else {
       const cont7 = function(x) {
	const cont8 = function(tmp13) {
	 if (tmp13) {
	  return [false, cont4, [{tag: "SOME", payload: x}]];
	 } else {
	  return [false, cont6, [_Int54_add(a2, 1)]];
	 }
	};
	return [false, a, [cont8, x]];
       };
       return [false, tmp12, [cont7, [a1, a2]]];
      }
     };
     return [false, cont6, [0]];
    };
    return [false, length2, [cont5, a1]];
   }]];
  };
  const exists = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const cont5 = function(n) {
     let cont6;
     cont6 = function(a2) {
      if (a2 >= n) {
       return [false, cont4, [false]];
      } else {
       const cont7 = function(tmp13) {
	const cont8 = function(tmp14) {
	 if (tmp14) {
	  return [false, cont4, [true]];
	 } else {
	  return [false, cont6, [_Int54_add(a2, 1)]];
	 }
	};
	return [false, a, [cont8, tmp13]];
       };
       return [false, tmp12, [cont7, [a1, a2]]];
      }
     };
     return [false, cont6, [0]];
    };
    return [false, length2, [cont5, a1]];
   }]];
  };
  const all = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const cont5 = function(n) {
     let cont6;
     cont6 = function(a2) {
      if (a2 >= n) {
       return [false, cont4, [true]];
      } else {
       const cont7 = function(tmp13) {
	const cont8 = function(tmp14) {
	 if (tmp14) {
	  return [false, cont6, [_Int54_add(a2, 1)]];
	 } else {
	  return [false, cont4, [false]];
	 }
	};
	return [false, a, [cont8, tmp13]];
       };
       return [false, tmp12, [cont7, [a1, a2]]];
      }
     };
     return [false, cont6, [0]];
    };
    return [false, length2, [cont5, a1]];
   }]];
  };
  const collate = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const xs = a1[0];
    const ys = a1[1];
    const cont5 = function(xl) {
     const cont6 = function(yl) {
      let cont7;
      cont7 = function(a2) {
       const tmp13 = xl <= a2;
       const tmp14 = yl <= a2;
       let tmp15;
       cont: {
	tmp15 = tmp13 && tmp14;
	break cont;
       }
       if (tmp15) {
	return [false, cont4, [EQUAL]];
       } else {
	let tmp16;
	cont: {
	 tmp16 = tmp13 && ! tmp14;
	 break cont;
	}
	if (tmp16) {
	 return [false, cont4, [LESS]];
	} else if (! tmp13 && tmp14) {
	 return [false, cont4, [GREATER]];
	} else if (! tmp13 && ! tmp14) {
	 const cont8 = function(tmp17) {
	  const cont9 = function(tmp18) {
	   const cont10 = function(exp) {
	    if (exp === "EQUAL") {
	     return [false, cont7, [_Int54_add(a2, 1)]];
	    } else {
	     return [false, cont4, [exp]];
	    }
	   };
	   return [false, a, [cont10, [tmp17, tmp18]]];
	  };
	  return [false, tmp12, [cont9, [ys, a2]]];
	 };
	 return [false, tmp12, [cont8, [xs, a2]]];
	} else {
	 throw _Match;
	}
       }
      };
      return [false, cont7, [0]];
     };
     return [false, length2, [cont6, ys]];
    };
    return [false, length2, [cont5, xs]];
   }]];
  };
  const toList = function(cont3, a) {
   const cont4 = function(tmp13) {
    const cont5 = function(tmp14) {
     return [false, tmp14, [cont3, a]];
    };
    return [false, tmp13, [cont5, null]];
   };
   return [false, foldr1, [cont4, $COLON$COLON]];
  };
  const append = function(cont3, a) {
   const v = a[0];
   const cont4 = function(tmp13) {
    return [false, tmp8, [cont3, _list([v, tmp13])]];
   };
   return [false, fromList2, [cont4, _list([a[1]])]];
  };
  const prepend = function(cont3, a) {
   const x = a[0];
   const v = a[1];
   const cont4 = function(tmp13) {
    return [false, tmp8, [cont3, _list([tmp13, v])]];
   };
   return [false, fromList2, [cont4, _list([x])]];
  };
  const length3 = function(cont3, tmp13) {
   return [false, cont3, [tmp13.length]];
  };
  const sub3 = function(cont3, a) {
   const base2 = a[0].base;
   const start = a[0].start;
   const length5 = a[0].length;
   const i = a[1];
   if (0 <= i && i < length5) {
    return [false, tmp12, [cont3, [base2, _Int54_add(start, i)]]];
   } else {
    throw _Subscript;
   }
  };
  const full = function(cont3, a) {
   const cont4 = function(tmp13) {
    return [false, cont3, [{base: a, length: tmp13, start: 0}]];
   };
   return [false, length2, [cont4, a]];
  };
  const slice = function(cont3, a) {
   if (a[2].tag === "NONE") {
    const v = a[0];
    const i = a[1];
    const cont4 = function(n) {
     if (0 <= i && i <= n) {
      return [false, cont3, [{base: v, length: _Int54_sub(n, i), start: i}]];
     } else {
      throw _Subscript;
     }
    };
    return [false, length2, [cont4, v]];
   } else if (a[2].tag === "SOME") {
    const v = a[0];
    const i = a[1];
    const n = a[2].payload;
    const cont4 = function(tmp13) {
     if (tmp13) {
      return [false, cont3, [{base: v, length: n, start: i}]];
     } else {
      throw _Subscript;
     }
    };
    if (0 <= i) {
     if (0 <= n) {
      const tmp13 = _Int54_add(i, n);
      const cont5 = function(tmp14) {
       return [false, cont4, [tmp13 <= tmp14]];
      };
      return [false, length2, [cont5, v]];
     } else {
      return [false, cont4, [false]];
     }
    } else {
     return [false, cont4, [false]];
    }
   } else {
    throw _Match;
   }
  };
  const subslice = function(cont3, a) {
   if (a[2].tag === "NONE") {
    const base2 = a[0].base;
    const start = a[0].start;
    const length5 = a[0].length;
    const i = a[1];
    if (0 <= i && i <= length5) {
     const tmp13 = _Int54_add(start, i);
     return [false, cont3, [{base: base2, length: _Int54_sub(length5, i), start: tmp13}]];
    } else {
     throw _Subscript;
    }
   } else if (a[2].tag === "SOME") {
    const base2 = a[0].base;
    const start = a[0].start;
    const length5 = a[0].length;
    const i = a[1];
    const n = a[2].payload;
    if (0 <= i && (0 <= n && _Int54_add(i, n) <= length5)) {
     return [false, cont3, [{base: base2, length: n, start: _Int54_add(start, i)}]];
    } else {
     throw _Subscript;
    }
   } else {
    throw _Match;
   }
  };
  const base = function(cont3, a) {
   const b = a.base;
   const start = a.start;
   return [false, cont3, [[b, start, a.length]]];
  };
  const concat1 = function(cont3, a) {
   const cont4 = function(tmp13) {
    const cont5 = function(tmp14) {
     return [false, tmp8, [cont3, tmp14]];
    };
    return [false, tmp13, [cont5, a]];
   };
   return [false, map, [cont4, tmp9]];
  };
  const isEmpty = function(cont3, a) {
   return [false, cont3, [a.length === 0]];
  };
  const getItem = function(cont3, a) {
   const base2 = a.base;
   const start = a.start;
   const length5 = a.length;
   if (length5 > 0) {
    const cont4 = function(tmp13) {
     const tmp14 = _Int54_add(start, 1);
     return [false, cont3, [{tag: "SOME", payload: [tmp13, {base: base2, length: _Int54_sub(length5, 1), start: tmp14}]}]];
    };
    return [false, tmp12, [cont4, [base2, start]]];
   } else {
    return [false, cont3, [NONE]];
   }
  };
  const appi1 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const length5 = a1.length;
    let cont5;
    cont5 = function(a2) {
     if (a2 >= length5) {
      return [false, cont4, [undefined]];
     } else {
      const cont6 = function(tmp13) {
       const cont7 = function(tmp14) {
	return [false, cont5, [_Int54_add(a2, 1)]];
       };
       return [false, a, [cont7, [a2, tmp13]]];
      };
      return [false, tmp12, [cont6, [base2, _Int54_add(start, a2)]]];
     }
    };
    return [false, cont5, [0]];
   }]];
  };
  const app1 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const tmp13 = _Int54_add(start, a1.length);
    let cont5;
    cont5 = function(a2) {
     if (a2 >= tmp13) {
      return [false, cont4, [undefined]];
     } else {
      const cont6 = function(tmp14) {
       const cont7 = function(tmp15) {
	return [false, cont5, [_Int54_add(a2, 1)]];
       };
       return [false, a, [cont7, tmp14]];
      };
      return [false, tmp12, [cont6, [base2, a2]]];
     }
    };
    return [false, cont5, [start]];
   }]];
  };
  const mapi1 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const length5 = a1.length;
    let cont5;
    cont5 = function(i, acc) {
     if (i >= length5) {
      return [false, tmp11, [cont4, [length5, acc]]];
     } else {
      const tmp13 = _Int54_add(i, 1);
      const cont6 = function(tmp14) {
       const cont7 = function(tmp15) {
	return [false, cont5, [tmp13, [tmp15, acc]]];
       };
       return [false, a, [cont7, [i, tmp14]]];
      };
      return [false, tmp12, [cont6, [base2, _Int54_add(start, i)]]];
     }
    };
    return [false, cont5, [0, null]];
   }]];
  };
  const map2 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const length5 = a1.length;
    const tmp13 = _Int54_add(start, length5);
    let cont5;
    cont5 = function(i, acc) {
     if (i >= tmp13) {
      return [false, tmp11, [cont4, [length5, acc]]];
     } else {
      const tmp14 = _Int54_add(i, 1);
      const cont6 = function(tmp15) {
       const cont7 = function(tmp16) {
	return [false, cont5, [tmp14, [tmp16, acc]]];
       };
       return [false, a, [cont7, tmp15]];
      };
      return [false, tmp12, [cont6, [base2, i]]];
     }
    };
    return [false, cont5, [start, null]];
   }]];
  };
  const foldli1 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    return [false, cont4, [function(cont5, a2) {
     const base2 = a2.base;
     const start = a2.start;
     const length5 = a2.length;
     let cont6;
     cont6 = function(i, acc) {
      if (i >= length5) {
       return [false, cont5, [acc]];
      } else {
       const tmp13 = _Int54_add(i, 1);
       const cont7 = function(tmp14) {
	const cont8 = function(tmp15) {
	 return [false, cont6, [tmp13, tmp15]];
	};
	return [false, a, [cont8, [i, tmp14, acc]]];
       };
       return [false, tmp12, [cont7, [base2, _Int54_add(start, i)]]];
      }
     };
     return [false, cont6, [0, a1]];
    }]];
   }]];
  };
  const foldri1 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    return [false, cont4, [function(cont5, a2) {
     const base2 = a2.base;
     const start = a2.start;
     let cont6;
     cont6 = function(i, acc) {
      if (i < 0) {
       return [false, cont5, [acc]];
      } else {
       const tmp13 = _Int54_sub(i, 1);
       const cont7 = function(tmp14) {
	const cont8 = function(tmp15) {
	 return [false, cont6, [tmp13, tmp15]];
	};
	return [false, a, [cont8, [i, tmp14, acc]]];
       };
       return [false, tmp12, [cont7, [base2, _Int54_add(start, i)]]];
      }
     };
     return [false, cont6, [_Int54_sub(a2.length, 1), a1]];
    }]];
   }]];
  };
  const foldl1 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    return [false, cont4, [function(cont5, a2) {
     const base2 = a2.base;
     const start = a2.start;
     const tmp13 = _Int54_add(start, a2.length);
     let cont6;
     cont6 = function(i, acc) {
      if (i >= tmp13) {
       return [false, cont5, [acc]];
      } else {
       const tmp14 = _Int54_add(i, 1);
       const cont7 = function(tmp15) {
	const cont8 = function(tmp16) {
	 return [false, cont6, [tmp14, tmp16]];
	};
	return [false, a, [cont8, [tmp15, acc]]];
       };
       return [false, tmp12, [cont7, [base2, i]]];
      }
     };
     return [false, cont6, [start, a1]];
    }]];
   }]];
  };
  const foldr2 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    return [false, cont4, [function(cont5, a2) {
     const base2 = a2.base;
     const start = a2.start;
     let cont6;
     cont6 = function(i, acc) {
      if (i < start) {
       return [false, cont5, [acc]];
      } else {
       const tmp13 = _Int54_sub(i, 1);
       const cont7 = function(tmp14) {
	const cont8 = function(tmp15) {
	 return [false, cont6, [tmp13, tmp15]];
	};
	return [false, a, [cont8, [tmp14, acc]]];
       };
       return [false, tmp12, [cont7, [base2, i]]];
      }
     };
     return [false, cont6, [_Int54_sub(_Int54_add(start, a2.length), 1), a1]];
    }]];
   }]];
  };
  const findi1 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const length5 = a1.length;
    let cont5;
    cont5 = function(a2) {
     if (a2 >= length5) {
      return [false, cont4, [NONE]];
     } else {
      const cont6 = function(x) {
       const cont7 = function(tmp13) {
	if (tmp13) {
	 return [false, cont4, [{tag: "SOME", payload: [a2, x]}]];
	} else {
	 return [false, cont5, [_Int54_add(a2, 1)]];
	}
       };
       return [false, a, [cont7, [a2, x]]];
      };
      return [false, tmp12, [cont6, [base2, _Int54_add(start, a2)]]];
     }
    };
    return [false, cont5, [0]];
   }]];
  };
  const find1 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const tmp13 = _Int54_add(start, a1.length);
    let cont5;
    cont5 = function(a2) {
     if (a2 >= tmp13) {
      return [false, cont4, [NONE]];
     } else {
      const cont6 = function(x) {
       const cont7 = function(tmp14) {
	if (tmp14) {
	 return [false, cont4, [{tag: "SOME", payload: x}]];
	} else {
	 return [false, cont5, [_Int54_add(a2, 1)]];
	}
       };
       return [false, a, [cont7, x]];
      };
      return [false, tmp12, [cont6, [base2, a2]]];
     }
    };
    return [false, cont5, [start]];
   }]];
  };
  const exists1 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const tmp13 = _Int54_add(start, a1.length);
    let cont5;
    cont5 = function(a2) {
     if (a2 >= tmp13) {
      return [false, cont4, [false]];
     } else {
      const cont6 = function(tmp14) {
       const cont7 = function(tmp15) {
	if (tmp15) {
	 return [false, cont4, [true]];
	} else {
	 return [false, cont5, [_Int54_add(a2, 1)]];
	}
       };
       return [false, a, [cont7, tmp14]];
      };
      return [false, tmp12, [cont6, [base2, a2]]];
     }
    };
    return [false, cont5, [start]];
   }]];
  };
  const all1 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const tmp13 = _Int54_add(start, a1.length);
    let cont5;
    cont5 = function(a2) {
     if (a2 >= tmp13) {
      return [false, cont4, [true]];
     } else {
      const cont6 = function(tmp14) {
       const cont7 = function(tmp15) {
	if (tmp15) {
	 return [false, cont5, [_Int54_add(a2, 1)]];
	} else {
	 return [false, cont4, [false]];
	}
       };
       return [false, a, [cont7, tmp14]];
      };
      return [false, tmp12, [cont6, [base2, a2]]];
     }
    };
    return [false, cont5, [start]];
   }]];
  };
  const collate1 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1[0].base;
    const start = a1[0].start;
    const length5 = a1[0].length;
    const base$PRIME = a1[1].base;
    const start$PRIME = a1[1].start;
    const length$PRIME = a1[1].length;
    const tmp13 = _Int54_add(start, length5);
    const tmp14 = _Int54_add(start$PRIME, length$PRIME);
    let cont5;
    cont5 = function(i, j) {
     const tmp15 = tmp13 <= i;
     const tmp16 = tmp14 <= j;
     let tmp17;
     cont: {
      tmp17 = tmp15 && tmp16;
      break cont;
     }
     if (tmp17) {
      return [false, cont4, [EQUAL]];
     } else {
      let tmp18;
      cont: {
       tmp18 = tmp15 && ! tmp16;
       break cont;
      }
      if (tmp18) {
       return [false, cont4, [LESS]];
      } else if (! tmp15 && tmp16) {
       return [false, cont4, [GREATER]];
      } else if (! tmp15 && ! tmp16) {
       const cont6 = function(tmp19) {
	const cont7 = function(tmp20) {
	 const cont8 = function(exp) {
	  if (exp === "EQUAL") {
	   return [false, cont5, [_Int54_add(i, 1), _Int54_add(j, 1)]];
	  } else {
	   return [false, cont4, [exp]];
	  }
	 };
	 return [false, a, [cont8, [tmp19, tmp20]]];
	};
	return [false, tmp12, [cont7, [base$PRIME, j]]];
       };
       return [false, tmp12, [cont6, [base2, i]]];
      } else {
       throw _Match;
      }
     }
    };
    return [false, cont5, [start, start$PRIME]];
   }]];
  };
  const array1 = function(cont3, a) {
   const n = a[0];
   const init = a[1];
   if (n < 0 || maxLen < n) {
    throw _Size;
   } else {
    return [false, tmp4, [cont3, [n, init]]];
   }
  };
  const tabulate2 = function(cont3, a) {
   const n = a[0];
   const f = a[1];
   if (maxLen < n) {
    throw _Size;
   } else {
    const cont4 = function(tmp13) {
     return [false, tmp5, [cont3, [n, tmp13]]];
    };
    return [false, tabulate, [cont4, n, f]];
   }
  };
  const sub4 = function(cont3, a) {
   const a1 = a[0];
   const i = a[1];
   const cont4 = function(tmp13) {
    if (tmp13) {
     return [false, tmp6, [cont3, [a1, i]]];
    } else {
     throw _Subscript;
    }
   };
   if (0 <= i) {
    const cont5 = function(tmp13) {
     return [false, cont4, [i < tmp13]];
    };
    return [false, length1, [cont5, a1]];
   } else {
    return [false, cont4, [false]];
   }
  };
  const update3 = function(cont3, a) {
   const a1 = a[0];
   const i = a[1];
   const x = a[2];
   const cont4 = function(tmp13) {
    if (tmp13) {
     return [false, tmp7, [cont3, [a1, i, x]]];
    } else {
     throw _Subscript;
    }
   };
   if (0 <= i) {
    const cont5 = function(tmp13) {
     return [false, cont4, [i < tmp13]];
    };
    return [false, length1, [cont5, a1]];
   } else {
    return [false, cont4, [false]];
   }
  };
  const copy = function(cont3, a) {
   const src = a.src;
   const dst = a.dst;
   const di = a.di;
   const cont4 = function(srcLen) {
    const cont5 = function(tmp13) {
     if (tmp13) {
      let cont6;
      cont6 = function(a1) {
       if (a1 >= srcLen) {
	return [false, cont3, [undefined]];
       } else {
	const tmp14 = _Int54_add(di, a1);
	const cont7 = function(tmp15) {
	 const cont8 = function(tmp16) {
	  return [false, cont6, [_Int54_add(a1, 1)]];
	 };
	 return [false, tmp7, [cont8, [dst, tmp14, tmp15]]];
	};
	return [false, tmp6, [cont7, [src, a1]]];
       }
      };
      return [false, cont6, [0]];
     } else {
      throw _Subscript;
     }
    };
    if (0 <= di) {
     const tmp13 = _Int54_add(di, srcLen);
     const cont6 = function(tmp14) {
      return [false, cont5, [tmp13 <= tmp14]];
     };
     return [false, length1, [cont6, dst]];
    } else {
     return [false, cont5, [false]];
    }
   };
   return [false, length1, [cont4, src]];
  };
  const copyVec = function(cont3, a) {
   const src = a.src;
   const dst = a.dst;
   const di = a.di;
   const cont4 = function(srcLen) {
    const cont5 = function(tmp13) {
     if (tmp13) {
      let cont6;
      cont6 = function(a1) {
       if (a1 >= srcLen) {
	return [false, cont3, [undefined]];
       } else {
	const tmp14 = _Int54_add(di, a1);
	const cont7 = function(tmp15) {
	 const cont8 = function(tmp16) {
	  return [false, cont6, [_Int54_add(a1, 1)]];
	 };
	 return [false, tmp7, [cont8, [dst, tmp14, tmp15]]];
	};
	return [false, tmp12, [cont7, [src, a1]]];
       }
      };
      return [false, cont6, [0]];
     } else {
      throw _Subscript;
     }
    };
    if (0 <= di) {
     const tmp13 = _Int54_add(di, srcLen);
     const cont6 = function(tmp14) {
      return [false, cont5, [tmp13 <= tmp14]];
     };
     return [false, length1, [cont6, dst]];
    } else {
     return [false, cont5, [false]];
    }
   };
   return [false, length2, [cont4, src]];
  };
  const appi2 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const cont5 = function(n) {
     let cont6;
     cont6 = function(a2) {
      if (a2 >= n) {
       return [false, cont4, [undefined]];
      } else {
       const cont7 = function(tmp13) {
	const cont8 = function(tmp14) {
	 return [false, cont6, [_Int54_add(a2, 1)]];
	};
	return [false, a, [cont8, [a2, tmp13]]];
       };
       return [false, tmp6, [cont7, [a1, a2]]];
      }
     };
     return [false, cont6, [0]];
    };
    return [false, length1, [cont5, a1]];
   }]];
  };
  const app2 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const cont5 = function(n) {
     let cont6;
     cont6 = function(a2) {
      if (a2 >= n) {
       return [false, cont4, [undefined]];
      } else {
       const cont7 = function(tmp13) {
	const cont8 = function(tmp14) {
	 return [false, cont6, [_Int54_add(a2, 1)]];
	};
	return [false, a, [cont8, tmp13]];
       };
       return [false, tmp6, [cont7, [a1, a2]]];
      }
     };
     return [false, cont6, [0]];
    };
    return [false, length1, [cont5, a1]];
   }]];
  };
  const modifyi = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const cont5 = function(n) {
     let cont6;
     cont6 = function(a2) {
      if (a2 >= n) {
       return [false, cont4, [undefined]];
      } else {
       const cont7 = function(x) {
	const cont8 = function(y) {
	 const cont9 = function(tmp13) {
	  return [false, cont6, [_Int54_add(a2, 1)]];
	 };
	 return [false, tmp7, [cont9, [a1, a2, y]]];
	};
	return [false, a, [cont8, [a2, x]]];
       };
       return [false, tmp6, [cont7, [a1, a2]]];
      }
     };
     return [false, cont6, [0]];
    };
    return [false, length1, [cont5, a1]];
   }]];
  };
  const modify = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const cont5 = function(n) {
     let cont6;
     cont6 = function(a2) {
      if (a2 >= n) {
       return [false, cont4, [undefined]];
      } else {
       const cont7 = function(x) {
	const cont8 = function(y) {
	 const cont9 = function(tmp13) {
	  return [false, cont6, [_Int54_add(a2, 1)]];
	 };
	 return [false, tmp7, [cont9, [a1, a2, y]]];
	};
	return [false, a, [cont8, x]];
       };
       return [false, tmp6, [cont7, [a1, a2]]];
      }
     };
     return [false, cont6, [0]];
    };
    return [false, length1, [cont5, a1]];
   }]];
  };
  const foldli2 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    return [false, cont4, [function(cont5, a2) {
     const cont6 = function(n) {
      let cont7;
      cont7 = function(i, acc) {
       if (i >= n) {
	return [false, cont5, [acc]];
       } else {
	const tmp13 = _Int54_add(i, 1);
	const cont8 = function(tmp14) {
	 const cont9 = function(tmp15) {
	  return [false, cont7, [tmp13, tmp15]];
	 };
	 return [false, a, [cont9, [i, tmp14, acc]]];
	};
	return [false, tmp6, [cont8, [a2, i]]];
       }
      };
      return [false, cont7, [0, a1]];
     };
     return [false, length1, [cont6, a2]];
    }]];
   }]];
  };
  const foldri2 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    return [false, cont4, [function(cont5, a2) {
     const cont6 = function(tmp13) {
      let cont7;
      cont7 = function(i, acc) {
       if (i < 0) {
	return [false, cont5, [acc]];
       } else {
	const tmp14 = _Int54_sub(i, 1);
	const cont8 = function(tmp15) {
	 const cont9 = function(tmp16) {
	  return [false, cont7, [tmp14, tmp16]];
	 };
	 return [false, a, [cont9, [i, tmp15, acc]]];
	};
	return [false, tmp6, [cont8, [a2, i]]];
       }
      };
      return [false, cont7, [_Int54_sub(tmp13, 1), a1]];
     };
     return [false, length1, [cont6, a2]];
    }]];
   }]];
  };
  const foldl2 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    return [false, cont4, [function(cont5, a2) {
     const cont6 = function(n) {
      let cont7;
      cont7 = function(i, acc) {
       if (i >= n) {
	return [false, cont5, [acc]];
       } else {
	const tmp13 = _Int54_add(i, 1);
	const cont8 = function(tmp14) {
	 const cont9 = function(tmp15) {
	  return [false, cont7, [tmp13, tmp15]];
	 };
	 return [false, a, [cont9, [tmp14, acc]]];
	};
	return [false, tmp6, [cont8, [a2, i]]];
       }
      };
      return [false, cont7, [0, a1]];
     };
     return [false, length1, [cont6, a2]];
    }]];
   }]];
  };
  const foldr3 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    return [false, cont4, [function(cont5, a2) {
     const cont6 = function(tmp13) {
      let cont7;
      cont7 = function(i, acc) {
       if (i < 0) {
	return [false, cont5, [acc]];
       } else {
	const tmp14 = _Int54_sub(i, 1);
	const cont8 = function(tmp15) {
	 const cont9 = function(tmp16) {
	  return [false, cont7, [tmp14, tmp16]];
	 };
	 return [false, a, [cont9, [tmp15, acc]]];
	};
	return [false, tmp6, [cont8, [a2, i]]];
       }
      };
      return [false, cont7, [_Int54_sub(tmp13, 1), a1]];
     };
     return [false, length1, [cont6, a2]];
    }]];
   }]];
  };
  const findi2 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const cont5 = function(n) {
     let cont6;
     cont6 = function(a2) {
      if (a2 >= n) {
       return [false, cont4, [NONE]];
      } else {
       const cont7 = function(x) {
	const cont8 = function(tmp13) {
	 if (tmp13) {
	  return [false, cont4, [{tag: "SOME", payload: [a2, x]}]];
	 } else {
	  return [false, cont6, [_Int54_add(a2, 1)]];
	 }
	};
	return [false, a, [cont8, [a2, x]]];
       };
       return [false, tmp6, [cont7, [a1, a2]]];
      }
     };
     return [false, cont6, [0]];
    };
    return [false, length1, [cont5, a1]];
   }]];
  };
  const find2 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const cont5 = function(n) {
     let cont6;
     cont6 = function(a2) {
      if (a2 >= n) {
       return [false, cont4, [NONE]];
      } else {
       const cont7 = function(x) {
	const cont8 = function(tmp13) {
	 if (tmp13) {
	  return [false, cont4, [{tag: "SOME", payload: x}]];
	 } else {
	  return [false, cont6, [_Int54_add(a2, 1)]];
	 }
	};
	return [false, a, [cont8, x]];
       };
       return [false, tmp6, [cont7, [a1, a2]]];
      }
     };
     return [false, cont6, [0]];
    };
    return [false, length1, [cont5, a1]];
   }]];
  };
  const exists2 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const cont5 = function(n) {
     let cont6;
     cont6 = function(a2) {
      if (a2 >= n) {
       return [false, cont4, [false]];
      } else {
       const cont7 = function(tmp13) {
	const cont8 = function(tmp14) {
	 if (tmp14) {
	  return [false, cont4, [true]];
	 } else {
	  return [false, cont6, [_Int54_add(a2, 1)]];
	 }
	};
	return [false, a, [cont8, tmp13]];
       };
       return [false, tmp6, [cont7, [a1, a2]]];
      }
     };
     return [false, cont6, [0]];
    };
    return [false, length1, [cont5, a1]];
   }]];
  };
  const all2 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const cont5 = function(n) {
     let cont6;
     cont6 = function(a2) {
      if (a2 >= n) {
       return [false, cont4, [true]];
      } else {
       const cont7 = function(tmp13) {
	const cont8 = function(tmp14) {
	 if (tmp14) {
	  return [false, cont6, [_Int54_add(a2, 1)]];
	 } else {
	  return [false, cont4, [false]];
	 }
	};
	return [false, a, [cont8, tmp13]];
       };
       return [false, tmp6, [cont7, [a1, a2]]];
      }
     };
     return [false, cont6, [0]];
    };
    return [false, length1, [cont5, a1]];
   }]];
  };
  const collate2 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const xs = a1[0];
    const ys = a1[1];
    const cont5 = function(xl) {
     const cont6 = function(yl) {
      let cont7;
      cont7 = function(a2) {
       const tmp13 = xl <= a2;
       const tmp14 = yl <= a2;
       let tmp15;
       cont: {
	tmp15 = tmp13 && tmp14;
	break cont;
       }
       if (tmp15) {
	return [false, cont4, [EQUAL]];
       } else {
	let tmp16;
	cont: {
	 tmp16 = tmp13 && ! tmp14;
	 break cont;
	}
	if (tmp16) {
	 return [false, cont4, [LESS]];
	} else if (! tmp13 && tmp14) {
	 return [false, cont4, [GREATER]];
	} else if (! tmp13 && ! tmp14) {
	 const cont8 = function(tmp17) {
	  const cont9 = function(tmp18) {
	   const cont10 = function(exp) {
	    if (exp === "EQUAL") {
	     return [false, cont7, [_Int54_add(a2, 1)]];
	    } else {
	     return [false, cont4, [exp]];
	    }
	   };
	   return [false, a, [cont10, [tmp17, tmp18]]];
	  };
	  return [false, tmp6, [cont9, [ys, a2]]];
	 };
	 return [false, tmp6, [cont8, [xs, a2]]];
	} else {
	 throw _Match;
	}
       }
      };
      return [false, cont7, [0]];
     };
     return [false, length1, [cont6, ys]];
    };
    return [false, length1, [cont5, xs]];
   }]];
  };
  const toList1 = function(cont3, a) {
   const cont4 = function(tmp13) {
    const cont5 = function(tmp14) {
     return [false, tmp14, [cont3, a]];
    };
    return [false, tmp13, [cont5, null]];
   };
   return [false, foldr3, [cont4, $COLON$COLON]];
  };
  const vector1 = function(cont3, a) {
   const cont4 = function(tmp13) {
    const cont5 = function(tmp14) {
     return [false, tmp10, [cont3, [tmp13, tmp14]]];
    };
    const cont6 = function(tmp14) {
     const cont7 = function(tmp15) {
      return [false, tmp15, [cont5, a]];
     };
     return [false, tmp14, [cont7, null]];
    };
    return [false, foldr3, [cont6, $COLON$COLON]];
   };
   return [false, length1, [cont4, a]];
  };
  const fromVector = function(cont3, a) {
   const cont4 = function(tmp13) {
    const cont5 = function(tmp14) {
     return [false, tmp5, [cont3, [tmp13, tmp14]]];
    };
    const cont6 = function(tmp14) {
     const cont7 = function(tmp15) {
      return [false, tmp15, [cont5, a]];
     };
     return [false, tmp14, [cont7, null]];
    };
    return [false, foldr1, [cont6, $COLON$COLON]];
   };
   return [false, length2, [cont4, a]];
  };
  const length4 = function(cont3, tmp13) {
   return [false, cont3, [tmp13.length]];
  };
  const sub5 = function(cont3, a) {
   const base2 = a[0].base;
   const start = a[0].start;
   const length5 = a[0].length;
   const i = a[1];
   if (0 <= i && i < length5) {
    return [false, tmp6, [cont3, [base2, _Int54_add(start, i)]]];
   } else {
    throw _Subscript;
   }
  };
  const update4 = function(cont3, a) {
   const base2 = a[0].base;
   const start = a[0].start;
   const length5 = a[0].length;
   const i = a[1];
   const x = a[2];
   if (0 <= i && i < length5) {
    return [false, tmp7, [cont3, [base2, _Int54_add(start, i), x]]];
   } else {
    throw _Subscript;
   }
  };
  const full1 = function(cont3, a) {
   const cont4 = function(tmp13) {
    return [false, cont3, [{base: a, length: tmp13, start: 0}]];
   };
   return [false, length1, [cont4, a]];
  };
  const slice1 = function(cont3, a) {
   if (a[2].tag === "NONE") {
    const v = a[0];
    const i = a[1];
    const cont4 = function(n) {
     if (0 <= i && i <= n) {
      return [false, cont3, [{base: v, length: _Int54_sub(n, i), start: i}]];
     } else {
      throw _Subscript;
     }
    };
    return [false, length1, [cont4, v]];
   } else if (a[2].tag === "SOME") {
    const v = a[0];
    const i = a[1];
    const n = a[2].payload;
    const cont4 = function(tmp13) {
     if (tmp13) {
      return [false, cont3, [{base: v, length: n, start: i}]];
     } else {
      throw _Subscript;
     }
    };
    if (0 <= i) {
     if (0 <= n) {
      const tmp13 = _Int54_add(i, n);
      const cont5 = function(tmp14) {
       return [false, cont4, [tmp13 <= tmp14]];
      };
      return [false, length1, [cont5, v]];
     } else {
      return [false, cont4, [false]];
     }
    } else {
     return [false, cont4, [false]];
    }
   } else {
    throw _Match;
   }
  };
  const subslice1 = function(cont3, a) {
   if (a[2].tag === "NONE") {
    const base2 = a[0].base;
    const start = a[0].start;
    const length5 = a[0].length;
    const i = a[1];
    if (0 <= i && i <= length5) {
     const tmp13 = _Int54_add(start, i);
     return [false, cont3, [{base: base2, length: _Int54_sub(length5, i), start: tmp13}]];
    } else {
     throw _Subscript;
    }
   } else if (a[2].tag === "SOME") {
    const base2 = a[0].base;
    const start = a[0].start;
    const length5 = a[0].length;
    const i = a[1];
    const n = a[2].payload;
    if (0 <= i && (0 <= n && _Int54_add(i, n) <= length5)) {
     return [false, cont3, [{base: base2, length: n, start: _Int54_add(start, i)}]];
    } else {
     throw _Subscript;
    }
   } else {
    throw _Match;
   }
  };
  const base1 = function(cont3, a) {
   const b = a.base;
   const start = a.start;
   return [false, cont3, [[b, start, a.length]]];
  };
  const copy1 = function(cont3, a) {
   const base2 = a.src.base;
   const start = a.src.start;
   const length5 = a.src.length;
   const dst = a.dst;
   const di = a.di;
   const cont4 = function(tmp13) {
    if (tmp13) {
     throw _Subscript;
    } else if (start >= di) {
     let cont5;
     cont5 = function(a1) {
      if (a1 >= length5) {
       return [false, cont3, [undefined]];
      } else {
       const tmp14 = _Int54_add(di, a1);
       const cont6 = function(tmp15) {
	const cont7 = function(tmp16) {
	 return [false, cont5, [_Int54_add(a1, 1)]];
	};
	return [false, tmp7, [cont7, [dst, tmp14, tmp15]]];
       };
       return [false, tmp6, [cont6, [base2, _Int54_add(start, a1)]]];
      }
     };
     return [false, cont5, [0]];
    } else {
     let cont5;
     cont5 = function(a1) {
      if (a1 < 0) {
       return [false, cont3, [undefined]];
      } else {
       const tmp14 = _Int54_add(di, a1);
       const cont6 = function(tmp15) {
	const cont7 = function(tmp16) {
	 return [false, cont5, [_Int54_sub(a1, 1)]];
	};
	return [false, tmp7, [cont7, [dst, tmp14, tmp15]]];
       };
       return [false, tmp6, [cont6, [base2, _Int54_add(start, a1)]]];
      }
     };
     return [false, cont5, [_Int54_sub(length5, 1)]];
    }
   };
   if (di < 0) {
    return [false, cont4, [true]];
   } else {
    const cont5 = function(tmp13) {
     return [false, cont4, [tmp13 < _Int54_add(di, length5)]];
    };
    return [false, length1, [cont5, dst]];
   }
  };
  const copyVec1 = function(cont3, a) {
   const base2 = a.src.base;
   const start = a.src.start;
   const length5 = a.src.length;
   const dst = a.dst;
   const di = a.di;
   if (di < 0) {
    throw _Subscript;
   } else {
    const cont4 = function(tmp13) {
     if (tmp13 < _Int54_add(di, length5)) {
      throw _Subscript;
     } else {
      let cont5;
      cont5 = function(a1) {
       if (a1 >= length5) {
	return [false, cont3, [undefined]];
       } else {
	const tmp14 = _Int54_add(di, a1);
	const cont6 = function(tmp15) {
	 const cont7 = function(tmp16) {
	  return [false, cont5, [_Int54_add(a1, 1)]];
	 };
	 return [false, tmp7, [cont7, [dst, tmp14, tmp15]]];
	};
	return [false, tmp12, [cont6, [base2, _Int54_add(start, a1)]]];
       }
      };
      return [false, cont5, [0]];
     }
    };
    return [false, length1, [cont4, dst]];
   }
  };
  const isEmpty1 = function(cont3, a) {
   return [false, cont3, [a.length === 0]];
  };
  const getItem1 = function(cont3, a) {
   const base2 = a.base;
   const start = a.start;
   const length5 = a.length;
   if (length5 > 0) {
    const cont4 = function(tmp13) {
     const tmp14 = _Int54_add(start, 1);
     return [false, cont3, [{tag: "SOME", payload: [tmp13, {base: base2, length: _Int54_sub(length5, 1), start: tmp14}]}]];
    };
    return [false, tmp6, [cont4, [base2, start]]];
   } else {
    return [false, cont3, [NONE]];
   }
  };
  const appi3 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const length5 = a1.length;
    let cont5;
    cont5 = function(a2) {
     if (a2 >= length5) {
      return [false, cont4, [undefined]];
     } else {
      const cont6 = function(tmp13) {
       const cont7 = function(tmp14) {
	return [false, cont5, [_Int54_add(a2, 1)]];
       };
       return [false, a, [cont7, [a2, tmp13]]];
      };
      return [false, tmp6, [cont6, [base2, _Int54_add(start, a2)]]];
     }
    };
    return [false, cont5, [0]];
   }]];
  };
  const app3 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const tmp13 = _Int54_add(start, a1.length);
    let cont5;
    cont5 = function(a2) {
     if (a2 >= tmp13) {
      return [false, cont4, [undefined]];
     } else {
      const cont6 = function(tmp14) {
       const cont7 = function(tmp15) {
	return [false, cont5, [_Int54_add(a2, 1)]];
       };
       return [false, a, [cont7, tmp14]];
      };
      return [false, tmp6, [cont6, [base2, a2]]];
     }
    };
    return [false, cont5, [start]];
   }]];
  };
  const modifyi1 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const length5 = a1.length;
    let cont5;
    cont5 = function(a2) {
     if (a2 >= length5) {
      return [false, cont4, [undefined]];
     } else {
      const tmp13 = _Int54_add(start, a2);
      const cont6 = function(x) {
       const cont7 = function(y) {
	const cont8 = function(tmp14) {
	 return [false, cont5, [_Int54_add(a2, 1)]];
	};
	return [false, tmp7, [cont8, [base2, tmp13, y]]];
       };
       return [false, a, [cont7, [a2, x]]];
      };
      return [false, tmp6, [cont6, [base2, tmp13]]];
     }
    };
    return [false, cont5, [0]];
   }]];
  };
  const modify1 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const tmp13 = _Int54_add(start, a1.length);
    let cont5;
    cont5 = function(a2) {
     if (a2 >= tmp13) {
      return [false, cont4, [undefined]];
     } else {
      const cont6 = function(x) {
       const cont7 = function(y) {
	const cont8 = function(tmp14) {
	 return [false, cont5, [_Int54_add(a2, 1)]];
	};
	return [false, tmp7, [cont8, [base2, a2, y]]];
       };
       return [false, a, [cont7, x]];
      };
      return [false, tmp6, [cont6, [base2, a2]]];
     }
    };
    return [false, cont5, [start]];
   }]];
  };
  const foldli3 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    return [false, cont4, [function(cont5, a2) {
     const base2 = a2.base;
     const start = a2.start;
     const length5 = a2.length;
     let cont6;
     cont6 = function(i, acc) {
      if (i >= length5) {
       return [false, cont5, [acc]];
      } else {
       const tmp13 = _Int54_add(i, 1);
       const cont7 = function(tmp14) {
	const cont8 = function(tmp15) {
	 return [false, cont6, [tmp13, tmp15]];
	};
	return [false, a, [cont8, [i, tmp14, acc]]];
       };
       return [false, tmp6, [cont7, [base2, _Int54_add(start, i)]]];
      }
     };
     return [false, cont6, [0, a1]];
    }]];
   }]];
  };
  const foldri3 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    return [false, cont4, [function(cont5, a2) {
     const base2 = a2.base;
     const start = a2.start;
     let cont6;
     cont6 = function(i, acc) {
      if (i < 0) {
       return [false, cont5, [acc]];
      } else {
       const tmp13 = _Int54_sub(i, 1);
       const cont7 = function(tmp14) {
	const cont8 = function(tmp15) {
	 return [false, cont6, [tmp13, tmp15]];
	};
	return [false, a, [cont8, [i, tmp14, acc]]];
       };
       return [false, tmp6, [cont7, [base2, _Int54_add(start, i)]]];
      }
     };
     return [false, cont6, [_Int54_sub(a2.length, 1), a1]];
    }]];
   }]];
  };
  const foldl3 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    return [false, cont4, [function(cont5, a2) {
     const base2 = a2.base;
     const start = a2.start;
     const tmp13 = _Int54_add(start, a2.length);
     let cont6;
     cont6 = function(i, acc) {
      if (i >= tmp13) {
       return [false, cont5, [acc]];
      } else {
       const tmp14 = _Int54_add(i, 1);
       const cont7 = function(tmp15) {
	const cont8 = function(tmp16) {
	 return [false, cont6, [tmp14, tmp16]];
	};
	return [false, a, [cont8, [tmp15, acc]]];
       };
       return [false, tmp6, [cont7, [base2, i]]];
      }
     };
     return [false, cont6, [start, a1]];
    }]];
   }]];
  };
  const foldr4 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    return [false, cont4, [function(cont5, a2) {
     const base2 = a2.base;
     const start = a2.start;
     let cont6;
     cont6 = function(i, acc) {
      if (i < start) {
       return [false, cont5, [acc]];
      } else {
       const tmp13 = _Int54_sub(i, 1);
       const cont7 = function(tmp14) {
	const cont8 = function(tmp15) {
	 return [false, cont6, [tmp13, tmp15]];
	};
	return [false, a, [cont8, [tmp14, acc]]];
       };
       return [false, tmp6, [cont7, [base2, i]]];
      }
     };
     return [false, cont6, [_Int54_sub(_Int54_add(start, a2.length), 1), a1]];
    }]];
   }]];
  };
  const findi3 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const length5 = a1.length;
    let cont5;
    cont5 = function(a2) {
     if (a2 >= length5) {
      return [false, cont4, [NONE]];
     } else {
      const cont6 = function(x) {
       const cont7 = function(tmp13) {
	if (tmp13) {
	 return [false, cont4, [{tag: "SOME", payload: [a2, x]}]];
	} else {
	 return [false, cont5, [_Int54_add(a2, 1)]];
	}
       };
       return [false, a, [cont7, [a2, x]]];
      };
      return [false, tmp6, [cont6, [base2, _Int54_add(start, a2)]]];
     }
    };
    return [false, cont5, [0]];
   }]];
  };
  const find3 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const tmp13 = _Int54_add(start, a1.length);
    let cont5;
    cont5 = function(a2) {
     if (a2 >= tmp13) {
      return [false, cont4, [NONE]];
     } else {
      const cont6 = function(x) {
       const cont7 = function(tmp14) {
	if (tmp14) {
	 return [false, cont4, [{tag: "SOME", payload: x}]];
	} else {
	 return [false, cont5, [_Int54_add(a2, 1)]];
	}
       };
       return [false, a, [cont7, x]];
      };
      return [false, tmp6, [cont6, [base2, a2]]];
     }
    };
    return [false, cont5, [start]];
   }]];
  };
  const exists3 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const tmp13 = _Int54_add(start, a1.length);
    let cont5;
    cont5 = function(a2) {
     if (a2 >= tmp13) {
      return [false, cont4, [false]];
     } else {
      const cont6 = function(tmp14) {
       const cont7 = function(tmp15) {
	if (tmp15) {
	 return [false, cont4, [true]];
	} else {
	 return [false, cont5, [_Int54_add(a2, 1)]];
	}
       };
       return [false, a, [cont7, tmp14]];
      };
      return [false, tmp6, [cont6, [base2, a2]]];
     }
    };
    return [false, cont5, [start]];
   }]];
  };
  const all3 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const tmp13 = _Int54_add(start, a1.length);
    let cont5;
    cont5 = function(a2) {
     if (a2 >= tmp13) {
      return [false, cont4, [true]];
     } else {
      const cont6 = function(tmp14) {
       const cont7 = function(tmp15) {
	if (tmp15) {
	 return [false, cont5, [_Int54_add(a2, 1)]];
	} else {
	 return [false, cont4, [false]];
	}
       };
       return [false, a, [cont7, tmp14]];
      };
      return [false, tmp6, [cont6, [base2, a2]]];
     }
    };
    return [false, cont5, [start]];
   }]];
  };
  const collate3 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1[0].base;
    const start = a1[0].start;
    const length5 = a1[0].length;
    const base$PRIME = a1[1].base;
    const start$PRIME = a1[1].start;
    const length$PRIME = a1[1].length;
    const tmp13 = _Int54_add(start, length5);
    const tmp14 = _Int54_add(start$PRIME, length$PRIME);
    let cont5;
    cont5 = function(i, j) {
     const tmp15 = tmp13 <= i;
     const tmp16 = tmp14 <= j;
     let tmp17;
     cont: {
      tmp17 = tmp15 && tmp16;
      break cont;
     }
     if (tmp17) {
      return [false, cont4, [EQUAL]];
     } else {
      let tmp18;
      cont: {
       tmp18 = tmp15 && ! tmp16;
       break cont;
      }
      if (tmp18) {
       return [false, cont4, [LESS]];
      } else if (! tmp15 && tmp16) {
       return [false, cont4, [GREATER]];
      } else if (! tmp15 && ! tmp16) {
       const cont6 = function(tmp19) {
	const cont7 = function(tmp20) {
	 const cont8 = function(exp) {
	  if (exp === "EQUAL") {
	   return [false, cont5, [_Int54_add(i, 1), _Int54_add(j, 1)]];
	  } else {
	   return [false, cont4, [exp]];
	  }
	 };
	 return [false, a, [cont8, [tmp19, tmp20]]];
	};
	return [false, tmp6, [cont7, [base$PRIME, j]]];
       };
       return [false, tmp6, [cont6, [base2, i]]];
      } else {
       throw _Match;
      }
     }
    };
    return [false, cont5, [start, start$PRIME]];
   }]];
  };
  const vector2 = function(cont3, a) {
   const tmp13 = a.length;
   const cont4 = function(tmp14) {
    const cont5 = function(tmp15) {
     const cont6 = function(tmp16) {
      return [false, tmp10, [cont3, [tmp13, tmp16]]];
     };
     return [false, tmp15, [cont6, a]];
    };
    return [false, tmp14, [cont5, null]];
   };
   return [false, foldr4, [cont4, $COLON$COLON]];
  };
  const UnsafeMonoVector = {sub: tmp12};
  const UnsafeMonoArray = {create: create, sub: tmp6, update: tmp7};
  const MonoVectorSlice = {all: all1, app: app1, appi: appi1, base: base, collate: collate1, concat: concat1, exists: exists1, find: find1, findi: findi1, foldl: foldl1, foldli: foldli1, foldr: foldr2, foldri: foldri1, full: full, getItem: getItem, isEmpty: isEmpty, length: length3, map: map2, mapi: mapi1, slice: slice, sub: sub3, subslice: subslice, vector: vector};
  const MonoVector = {all: all, app: app, append: append, appi: appi, collate: collate, concat: tmp8, exists: exists, find: find, findi: findi, foldl: foldl, foldli: foldli, foldr: foldr1, foldri: foldri, fromList: fromList2, length: length2, map: map1, mapi: mapi, maxLen: maxLen1, prepend: prepend, sub: sub2, tabulate: tabulate1, toList: toList, update: update2};
  const MonoArraySlice = {all: all3, app: app3, appi: appi3, base: base1, collate: collate3, copy: copy1, copyVec: copyVec1, exists: exists3, find: find3, findi: findi3, foldl: foldl3, foldli: foldli3, foldr: foldr4, foldri: foldri3, full: full1, getItem: getItem1, isEmpty: isEmpty1, length: length4, modify: modify1, modifyi: modifyi1, slice: slice1, sub: sub5, subslice: subslice1, update: update4, vector: vector2};
  return [false, cont2, [{_MonoArray: {all: all2, app: app2, appi: appi2, array: array1, collate: collate2, copy: copy, copyVec: copyVec, exists: exists2, find: find2, findi: findi2, foldl: foldl2, foldli: foldli2, foldr: foldr3, foldri: foldri2, fromList: fromList1, fromVector: fromVector, length: length1, maxLen: maxLen, modify: modify, modifyi: modifyi, sub: sub4, tabulate: tabulate2, toList: toList1, toVector: vector1, update: update3, vector: vector1}, _MonoArraySlice: MonoArraySlice, _MonoVector: MonoVector, _MonoVectorSlice: MonoVectorSlice, _UnsafeMonoArray: UnsafeMonoArray, _UnsafeMonoVector: UnsafeMonoVector}]];
 };
 const unsafeFromListN = function(cont2, a) {
  return [false, cont2, [_String_implode(a[1])]];
 };
 const unsafeFromListRevN = function(cont2, a) {
  const cont3 = function(tmp4) {
   return [false, cont2, [_String_implode(tmp4)]];
  };
  return [false, revAppend, [cont3, a[1], null]];
 };
 const sliceToVector = function(cont2, a) {
  const base = a.base;
  const start = a.start;
  return [false, substring, [cont2, base, start, a.length]];
 };
 const unsafeCreateWithZero = function(cont2, a) {
  return [false, cont2, [_Array_array(a, 0)]];
 };
 const cont1 = function(Base) {
  const CharVector = Base._MonoVector;
  const tmp4 = Base._MonoVectorSlice.full;
  const tmp5 = CharVector.appi;
  const unsafeSub = function(cont3, a) {
   const v = a[0];
   const cont4 = function(tmp6) {
    return [false, cont3, [(tmp6 >>> 0 & 0xFF) >>> 0]];
   };
   return [false, sub1, [cont4, [v, a[1]]]];
  };
  const fromList1 = function(cont3, a) {
   const cont4 = function(tmp6) {
    const cont5 = function(tmp7) {
     return [false, cont3, [_String_implode(tmp7)]];
    };
    return [false, tmp6, [cont5, a]];
   };
   return [false, map, [cont4, function(cont5, a1) {
    if (a1 >= 0x80000000) {
     throw _Overflow;
    } else if (a1 < 0) {
     throw Chr;
    } else if (a1 > 255) {
     throw Chr;
    } else {
     return [false, cont5, [a1]];
    }
   }]];
  };
  const unsafeFromListN1 = function(cont3, a) {
   return [false, fromList1, [cont3, a[1]]];
  };
  const unsafeFromListRevN1 = function(cont3, a) {
   const xs = a[1];
   const cont4 = function(tmp6) {
    const cont5 = function(tmp7) {
     const cont6 = function(tmp8) {
      return [false, cont3, [_String_implode(tmp8)]];
     };
     return [false, revAppend, [cont6, tmp7, null]];
    };
    return [false, tmp6, [cont5, xs]];
   };
   return [false, map, [cont4, function(cont5, a1) {
    if (a1 >= 0x80000000) {
     throw _Overflow;
    } else if (a1 < 0) {
     throw Chr;
    } else if (a1 > 255) {
     throw Chr;
    } else {
     return [false, cont5, [a1]];
    }
   }]];
  };
  const sliceToVector1 = function(cont3, a) {
   const base = a.base;
   const start = a.start;
   return [false, substring, [cont3, base, start, a.length]];
  };
  const unsafeCreateWithZero1 = function(cont3, a) {
   return [false, cont3, [_Array_array(a, 0x0)]];
  };
  const cont2 = function(Base1) {
   const Word8Vector = Base1._MonoVector;
   const Word8Array = Base1._MonoArray;
   const tmp6 = Word8Vector.fromList;
   const tmp7 = Word8Vector.tabulate;
   const tmp8 = Word8Array.array;
   const tmp9 = Word8Array.maxLen;
   const tmp10 = Word8Array.sub;
   const tmp11 = Word8Array.update;
   const Io_tag = function(payload) {
    this.payload = payload;
   };
   Io_tag.prototype.name = "Io";
   const BlockingNotSupported_tag = function() {
   };
   BlockingNotSupported_tag.prototype.name = "BlockingNotSupported";
   const BlockingNotSupported = new BlockingNotSupported_tag();
   const BLOCK_BUF = "BLOCK_BUF";
   const outputAndFlush = function(cont4, tmp17, content) {
    if (tmp17.tag === "JS_WRITABLE") {
     const writable = tmp17.payload.writable;
     return [false, _withSubCont, [cont4, _topLevel, function(cont5, a) {
      writable.write(content, null, _function(function(cont6, args) {
       const cont7 = function(tmp18) {
	return [false, cont6, [undefined]];
       };
       return [false, _pushPrompt, [cont7, _topLevel, function(cont8, a1) {
	return [false, _pushSubCont, [cont8, a, function(cont9, a2) {
	 return [false, cont9, [undefined]];
	}]];
       }]];
      }));
      return [false, cont5, [undefined]];
     }]];
    } else if (tmp17.tag === "PRIM_WRITER") {
     const name = tmp17.payload.writer.name;
     const writeVec = tmp17.payload.writer.writeVec;
     const buffer = tmp17.payload.buffer;
     const cont5 = function(tmp18) {
      const tmp19 = _String_concat(tmp18);
      if (writeVec.tag === "SOME") {
       const writeVec1 = writeVec.payload;
       const cont6 = function(tmp20) {
	const cont7 = function(tmp21) {
	 buffer[0] = null;
	 return [false, cont4, [undefined]];
	};
	return [false, writeVec1, [cont7, tmp20]];
       };
       return [false, tmp4, [cont6, tmp19]];
      } else if (writeVec.tag === "NONE") {
       throw new Io_tag({cause: BlockingNotSupported, "function": Uint8Array.of(111, 117, 116, 112, 117, 116), name: name});
      } else {
       throw _Match;
      }
     };
     return [false, revAppend, [cont5, [content, buffer[0]], null]];
    } else {
     throw _Match;
    }
   };
   const tmp12 = {tag: "JS_WRITABLE", payload: {buffer_mode: [BLOCK_BUF], name: Uint8Array.of(60, 115, 116, 100, 111, 117, 116, 62), writable: stdout}};
   const tmp13 = tmp1(0xCBF29CE5);
   const tmp14 = tmp2(64, tmp13 * tmp1(0xFFFFFFFF));
   tmp2(64, tmp14 + tmp1(0x5014C00A));
   const tmp15 = tmp1(0x100);
   const tmp16 = tmp2(64, tmp15 * tmp1(0xFFFFFFFF));
   tmp2(64, tmp16 + tmp1(0x2B3));
   _Array_array(64, null);
   let f;
   f = function(cont4, a) {
    const tmp17 = _exh;
    _exh = function(exn) {
     _exh = tmp17;
     if (exn instanceof _Overflow_tag) {
      return [false, cont4, [a]];
     } else {
      throw exn;
     }
    };
    const cont5 = function(tmp19) {
     _exh = tmp17;
     return [false, cont4, [tmp19]];
    };
    const tmp18 = _Int54_add(a, a);
    if (tmp18 < 4294967295) {
     return [false, f, [cont5, tmp18]];
    } else {
     return [false, cont5, [a]];
    }
   };
   const cont3 = function(tmp17) {
    const cont4 = function(tbl) {
     const cont5 = function(tmp18) {
      const cont6 = function(tmp19) {
       const cont7 = function(tmp20) {
	const cont8 = function(tmp21) {
	 const cont9 = function(tmp22) {
	  const cont10 = function(tmp23) {
	   const cont11 = function(tmp24) {
	    const cont12 = function(tbl1) {
	     const cont13 = function(tmp25) {
	      const cont14 = function(tmp26) {
	       const cont15 = function(tmp27) {
		const cont16 = function(tmp28) {
		 const cont17 = function(tmp29) {
		  const cont18 = function(tmp32) {
		   48 >>> 0;
		   57 >>> 0;
		   65 >>> 0;
		   70 >>> 0;
		   97 >>> 0;
		   102 >>> 0;
		   tmp(tmp2(32, 2147483646n));
		   tmp(2147483647n);
		   tmp1(0x72646E64);
		   let q;
		   cont: {
		    if (0xBC8F === 0x0) {
		     throw _Div;
		    } else {
		     q = 0x7FFFFFFF / 0xBC8F >>> 0;
		     break cont;
		    }
		   }
		   let r;
		   cont: {
		    if (0xBC8F === 0x0) {
		     throw _Div;
		    } else {
		     r = 0x7FFFFFFF % 0xBC8F;
		     break cont;
		    }
		   }
		   const cont19 = function() {
		    const write_to_file = function(cont22, filename, content, file_type) {
		     const tmp49 = _exh;
		     _exh = function(exn) {
		      _exh = tmp49;
		      if (exn instanceof Io_tag) {
		       return [false, outputAndFlush, [cont22, tmp12, _String_append(Uint8Array.of(10, 99, 111, 117, 108, 100, 32, 110, 111, 116, 32, 119, 114, 105, 116, 101, 32, 116, 111, 32, 102, 105, 108, 101, 58, 32), filename)]];
		      } else {
		       throw exn;
		      }
		     };
		     const cont23 = function(tmp51) {
		      _exh = tmp49;
		      return [false, cont22, [tmp51]];
		     };
		     const tmp50 = createWriteStream(filename);
		     const cont24 = function(tmp51) {
		      const cont25 = function(tmp52) {
		       if (_String_EQUAL(file_type, Uint8Array.of(98, 105, 116))) {
			return [false, outputAndFlush, [cont23, tmp12, _String_append(Uint8Array.of(10, 66, 105, 116, 32, 115, 116, 114, 101, 97, 109, 32, 104, 97, 115, 32, 98, 101, 101, 110, 32, 119, 114, 105, 116, 116, 101, 110, 32, 116, 111, 32, 100, 105, 115, 107, 32, 117, 110, 100, 101, 114, 32, 110, 97, 109, 101, 58, 32, 32), filename)]];
		       } else {
			return [false, outputAndFlush, [cont23, tmp12, _String_append(Uint8Array.of(10, 66, 121, 116, 101, 32, 115, 116, 114, 101, 97, 109, 32, 104, 97, 115, 32, 98, 101, 101, 110, 32, 119, 114, 105, 116, 116, 101, 110, 32, 116, 111, 32, 100, 105, 115, 107, 32, 117, 110, 100, 101, 114, 32, 110, 97, 109, 101, 58, 32), filename)]];
		       }
		      };
		      return [false, _withSubCont, [cont25, _topLevel, function(cont26, a) {
		       tmp50.end(_function(function(cont27, args) {
			const cont28 = function(tmp52) {
			 return [false, cont27, [undefined]];
			};
			return [false, _pushPrompt, [cont28, _topLevel, function(cont29, a1) {
			 return [false, _pushSubCont, [cont29, a, function(cont30, a2) {
			  return [false, cont30, [undefined]];
			 }]];
			}]];
		       }));
		       return [false, cont26, [undefined]];
		      }]];
		     };
		     if (! tmp50.write(content)) {
		      return [false, _withSubCont, [cont24, _topLevel, function(cont25, a) {
		       tmp50.once("drain", _function(function(cont26, args) {
			const cont27 = function(tmp51) {
			 return [false, cont26, [undefined]];
			};
			return [false, _pushPrompt, [cont27, _topLevel, function(cont28, a1) {
			 return [false, _pushSubCont, [cont28, a, function(cont29, a2) {
			  return [false, cont29, [undefined]];
			 }]];
			}]];
		       }));
		       return [false, cont25, [undefined]];
		      }]];
		     } else {
		      return [false, cont24, [undefined]];
		     }
		    };
		    const tmp36 = tmp1(0xFFF1);
		    const tmp37 = _Array_array(62500, 0);
		    const tmp38 = _Array_array(62500, Uint8Array.of(48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48));
		    const tmp39 = _Array_array(62500, Uint8Array.of(48, 48, 48, 48));
		    const tmp40 = tmp1(0xCBF29CE5);
		    const tmp41 = tmp2(64, tmp40 * tmp1(0xFFFFFFFF));
		    const tmp42 = tmp2(64, tmp41 + tmp1(0x5014C00A));
		    const tmp43 = tmp1(0x100);
		    const tmp44 = tmp2(64, tmp43 * tmp1(0xFFFFFFFF));
		    const tmp45 = tmp2(64, tmp44 + tmp1(0x2B3));
		    const cont21 = function(secs, usecs) {
		     const tmp49 = tmp2(64, (tmp42 ^ tmp2(64, secs)) * tmp45);
		     const tmp50 = tmp2(64, (tmp49 ^ tmp2(64, usecs)) * tmp45);
		     const tmp51 = tmp2(64, tmp36 - tmp1(0x2));
		     let tmp52;
		     cont: {
		      if (tmp51 === 0n) {
		       throw _Div;
		      } else {
		       tmp52 = tmp50 % tmp51;
		       break cont;
		      }
		     }
		     const tmp53 = tmp2(64, tmp52 + tmp1(0x1));
		     let start_seed;
		     cont: {
		      if (- 9007199254740992n <= tmp53) {
		       if (tmp53 <= 9007199254740991n) {
			start_seed = Number(tmp53);
			break cont;
		       } else {
			throw _Overflow;
		       }
		      } else {
		       throw _Overflow;
		      }
		     }
		     const cont22 = function(tmp54) {
		      const cont23 = function() {
		       const cont25 = function(tmp55) {
			const cont26 = function(bits_x_rev) {
			 const tmp56 = _String_concat(bits_x_rev);
			 const cont27 = function(tmp57) {
			  const cont28 = function(bits_hex_rev) {
			   const tmp58 = _String_concat(bits_hex_rev);
			   const cont29 = function(tmp59) {
			    const cont30 = function(tmp60) {
			     const cont31 = function(tmp61) {
			      return [false, cont, []];
			     };
			     return [false, outputAndFlush, [cont31, tmp12, Uint8Array.of(10)]];
			    };
			    return [false, write_to_file, [cont30, Uint8Array.of(114, 97, 110, 100, 111, 109, 95, 98, 105, 116, 115, 116, 114, 105, 110, 103, 46, 98, 121, 116, 101), tmp58, Uint8Array.of(98, 121, 116, 101)]];
			   };
			   return [false, write_to_file, [cont29, Uint8Array.of(114, 97, 110, 100, 111, 109, 95, 98, 105, 116, 115, 116, 114, 105, 110, 103, 46, 98, 105, 110), tmp56, Uint8Array.of(98, 105, 116)]];
			  };
			  return [false, tmp57, [cont28, tmp39]];
			 };
			 return [false, foldr, [cont27, function(cont28, a) {
			  return [false, cont28, [[a[0], a[1]]]];
			 }, null]];
			};
			return [false, tmp55, [cont26, tmp38]];
		       };
		       return [false, foldr, [cont25, function(cont26, a) {
			return [false, cont26, [[a[0], a[1]]]];
		       }, null]];
		      };
		      let cont24;
		      cont24 = function(i, seed) {
		       const tmp55 = _Int54_mod(_Int54_mul(17364, seed), 65521);
		       const cont25 = function(tmp56) {
			const cont26 = function(bits_x_str) {
			 const cont28 = function(bits_hex_str) {
			  const cont30 = function(tmp57) {
			   const cont31 = function(tmp58) {
			    if (i < 62499) {
			     return [false, cont24, [_Int54_add(i, 1), tmp55]];
			    } else {
			     return [false, cont23, []];
			    }
			   };
			   return [false, update1, [cont31, tmp39, i, bits_hex_str]];
			  };
			  return [false, update1, [cont30, tmp38, i, bits_x_str]];
			 };
			 let toHex;
			 toHex = function(cont30, a) {
			  if (a === 0) {
			   return [false, cont30, [Uint8Array.of()]];
			  } else {
			   const cont31 = function(tmp57) {
			    const tmp58 = _Int54_mod(a, 16);
			    if (tmp58 === 0) {
			     return [false, cont30, [_String_append(tmp57, Uint8Array.of(48))]];
			    } else if (tmp58 === 1) {
			     return [false, cont30, [_String_append(tmp57, Uint8Array.of(49))]];
			    } else if (tmp58 === 2) {
			     return [false, cont30, [_String_append(tmp57, Uint8Array.of(50))]];
			    } else if (tmp58 === 3) {
			     return [false, cont30, [_String_append(tmp57, Uint8Array.of(51))]];
			    } else if (tmp58 === 4) {
			     return [false, cont30, [_String_append(tmp57, Uint8Array.of(52))]];
			    } else if (tmp58 === 5) {
			     return [false, cont30, [_String_append(tmp57, Uint8Array.of(53))]];
			    } else if (tmp58 === 6) {
			     return [false, cont30, [_String_append(tmp57, Uint8Array.of(54))]];
			    } else if (tmp58 === 7) {
			     return [false, cont30, [_String_append(tmp57, Uint8Array.of(55))]];
			    } else if (tmp58 === 8) {
			     return [false, cont30, [_String_append(tmp57, Uint8Array.of(56))]];
			    } else if (tmp58 === 9) {
			     return [false, cont30, [_String_append(tmp57, Uint8Array.of(57))]];
			    } else if (tmp58 === 10) {
			     return [false, cont30, [_String_append(tmp57, Uint8Array.of(97))]];
			    } else if (tmp58 === 11) {
			     return [false, cont30, [_String_append(tmp57, Uint8Array.of(98))]];
			    } else if (tmp58 === 12) {
			     return [false, cont30, [_String_append(tmp57, Uint8Array.of(99))]];
			    } else if (tmp58 === 13) {
			     return [false, cont30, [_String_append(tmp57, Uint8Array.of(100))]];
			    } else if (tmp58 === 14) {
			     return [false, cont30, [_String_append(tmp57, Uint8Array.of(101))]];
			    } else if (tmp58 === 15) {
			     return [false, cont30, [_String_append(tmp57, Uint8Array.of(102))]];
			    } else {
			     return [false, cont30, [_String_append(tmp57, Uint8Array.of())]];
			    }
			   };
			   return [false, toHex, [cont31, 0 + Math.floor(a / 16)]];
			  }
			 };
			 const cont29 = function(hexadec) {
			  const cont30 = function(tmp57) {
			   return [false, tmp57, [cont28, hexadec]];
			  };
			  return [false, padLeft, [cont30, 48, 4]];
			 };
			 return [false, toHex, [cont29, tmp55]];
			};
			let toBinary;
			toBinary = function(cont28, a) {
			 if (a === 0) {
			  return [false, cont28, [Uint8Array.of()]];
			 } else {
			  const cont29 = function(tmp57) {
			   if (_Int54_mod(a, 2) === 0) {
			    return [false, cont28, [_String_append(tmp57, Uint8Array.of(48))]];
			   } else {
			    return [false, cont28, [_String_append(tmp57, Uint8Array.of(49))]];
			   }
			  };
			  return [false, toBinary, [cont29, 0 + Math.floor(a / 2)]];
			 }
			};
			const cont27 = function(binary) {
			 const cont28 = function(tmp57) {
			  return [false, tmp57, [cont26, binary]];
			 };
			 return [false, padLeft, [cont28, 48, 16]];
			};
			return [false, toBinary, [cont27, tmp55]];
		       };
		       return [false, update1, [cont25, tmp37, i, tmp55]];
		      };
		      return [false, cont24, [0, start_seed]];
		     };
		     return [false, outputAndFlush, [cont22, tmp12, Uint8Array.of(10, 103, 101, 110, 101, 114, 97, 116, 105, 110, 103, 32, 97, 32, 114, 97, 110, 100, 111, 109, 32, 98, 105, 116, 32, 115, 116, 114, 101, 97, 109, 46, 46, 46)]];
		    };
		    const tmp46 = BigInt(tmp3()) * 1000n;
		    const tmp47 = tmp46 <= 0n && false;
		    let tmp48;
		    cont: {
		     tmp48 = tmp47 || tmp46 >= 0n;
		     break cont;
		    }
		    if (tmp48) {
		     const tmp49 = tmp46 / 1000000n;
		     return [false, cont21, [tmp49, tmp46 % 1000000n]];
		    } else {
		     const q1 = tmp46 / 1000000n;
		     const r1 = tmp46 % 1000000n;
		     if (r1 === 0n) {
		      return [false, cont21, [q1, r1]];
		     } else {
		      const tmp49 = q1 - 1n;
		      return [false, cont21, [tmp49, 1000000n + r1]];
		     }
		    }
		   };
		   const tmp33 = (123 >>> 0 & 0x7FFFFFFF) >>> 0;
		   let tmp34;
		   cont: {
		    if (0x1 >= 0x20) {
		     tmp34 = 0x0;
		     break cont;
		    } else {
		     tmp34 = tmp33 << 0x1 >>> 0;
		     break cont;
		    }
		   }
		   const tmp35 = tmp34 + 0x1 >>> 0;
		   let cont20;
		   cont20 = function(tmp36, seeds, congx, shrgx) {
		    if (tmp36 === 0) {
		     return [false, cont19, []];
		    } else {
		     let cont21;
		     cont21 = function(tmp37, seed, congx$PRIME, shrgx$PRIME) {
		      if (tmp37 === 0) {
		       const tmp38 = _Int54_sub(tmp36, 1);
		       return [false, cont20, [tmp38, [seed, seeds], congx$PRIME, shrgx$PRIME]];
		      } else {
		       const tmp38 = _Int54_sub(tmp37, 1);
		       const cont22 = function(c$PRIME) {
			let tmp43;
			cont: {
			 if (0x12 >= 0x20) {
			  tmp43 = 0x0;
			  break cont;
			 } else {
			  tmp43 = shrgx$PRIME << 0x12 >>> 0;
			  break cont;
			 }
			}
			const tmp44 = (shrgx$PRIME ^ tmp43) >>> 0;
			let tmp45;
			cont: {
			 if (0xD >= 0x20) {
			  tmp45 = 0x0;
			  break cont;
			 } else {
			  tmp45 = tmp44 >>> 0xD;
			  break cont;
			 }
			}
			const tmp46 = (tmp44 ^ tmp45) >>> 0;
			let tmp47;
			cont: {
			 if (0x1 >= 0x20) {
			  tmp47 = 0x0;
			  break cont;
			 } else {
			  tmp47 = seed >>> 0x1;
			  break cont;
			 }
			}
			const tmp48 = (0x3FFFFFFF & tmp47) >>> 0;
			return [false, cont21, [tmp38, (tmp48 | (0x40000000 & (c$PRIME ^ tmp46) >>> 0) >>> 0) >>> 0, c$PRIME, tmp46]];
		       };
		       let tmp39;
		       cont: {
			if (q === 0x0) {
			 throw _Div;
			} else {
			 tmp39 = congx$PRIME % q;
			 break cont;
			}
		       }
		       const tmp40 = Math_imul(0xBC8F, tmp39) >>> 0;
		       let tmp41;
		       cont: {
			if (q === 0x0) {
			 throw _Div;
			} else {
			 tmp41 = congx$PRIME / q >>> 0;
			 break cont;
			}
		       }
		       const tmp42 = Math_imul(r, tmp41) >>> 0;
		       if (tmp40 > tmp42) {
			return [false, cont22, [tmp40 - tmp42 >>> 0]];
		       } else {
			return [false, cont22, [(0x7FFFFFFF - tmp42 >>> 0) + tmp40 >>> 0]];
		       }
		      }
		     };
		     return [false, cont21, [31, 0x0, congx, shrgx]];
		    }
		   };
		   return [false, cont20, [48, null, tmp35, 73256 >>> 0]];
		  };
		  const tmp30 = _exh;
		  _exh = function(exn) {
		   _exh = tmp30;
		   if (exn instanceof _Overflow_tag) {
		    return [false, cont18, [undefined]];
		   } else {
		    throw exn;
		   }
		  };
		  let tmp31;
		  cont: {
		   _Int54_mul(8, tmp9);
		   tmp31 = undefined;
		   break cont;
		  }
		  _exh = tmp30;
		  return [false, cont18, [tmp31]];
		 };
		 return [false, tmp6, [cont17, _list([0xFF, 0xFE, 0xFC, 0xF8, 0xF0, 0xE0, 0xC0, 0x80, 0x0])]];
		};
		return [false, tmp6, [cont16, _list([0x0, 0x1, 0x3, 0x7, 0xF, 0x1F, 0x3F, 0x7F, 0xFF])]];
	       };
	       return [false, tmp7, [cont15, [256, function(cont16, i) {
		return [false, tmp10, [cont16, [tbl1, i]]];
	       }]]];
	      };
	      return [false, tmp25, [cont14, Uint8Array.of(65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 43, 47)]];
	     };
	     return [false, tmp5, [cont13, function(cont14, a) {
	      const i = a[0];
	      const c = a[1];
	      return [false, tmp11, [cont14, [tbl1, c, (i >>> 0 & 0xFF) >>> 0]]];
	     }]];
	    };
	    return [false, tmp8, [cont12, [256, 0xFF]]];
	   };
	   return [false, tmp7, [cont11, [256, function(cont12, i) {
	    return [false, tmp10, [cont12, [tbl, i]]];
	   }]]];
	  };
	  return [false, tmp22, [cont10, Uint8Array.of(65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 43, 47)]];
	 };
	 return [false, tmp5, [cont9, function(cont10, a) {
	  const i = a[0];
	  const c = a[1];
	  return [false, tmp11, [cont10, [tbl, c, (i >>> 0 & 0xFF) >>> 0]]];
	 }]];
	};
	return [false, tmp11, [cont8, [tbl, 32, 0x41]]];
       };
       return [false, tmp11, [cont7, [tbl, 13, 0x41]]];
      };
      return [false, tmp11, [cont6, [tbl, 10, 0x41]]];
     };
     return [false, tmp11, [cont5, [tbl, 9, 0x41]]];
    };
    return [false, tmp8, [cont4, [256, 0xFF]]];
   };
   return [false, f, [cont3, 65536]];
  };
  return [false, MonoSequence, [cont2, fromList, length, 4294967295, array, unsafeCreateWithZero1, function(cont3, a) {
   return [false, cont3, [_VectorOrArray_fromList(a[1])]];
  }, sub, update, concat, fromList1, size, 2147483647, sliceToVector1, sliceToVector1, unsafeFromListN1, unsafeFromListRevN1, unsafeSub]];
 };
 return [false, MonoSequence, [cont1, fromList, length, 4294967295, array, unsafeCreateWithZero, function(cont2, a) {
  return [false, cont2, [_VectorOrArray_fromList(a[1])]];
 }, sub, update, concat, implode, size, 2147483647, sliceToVector, sliceToVector, unsafeFromListN, unsafeFromListRevN, sub1]];
}, true);
