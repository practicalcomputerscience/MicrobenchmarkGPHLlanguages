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
 const Domain_tag = function() {
 };
 Domain_tag.prototype.name = "Domain";
 const Domain = new Domain_tag();
 const LESS = "LESS";
 const EQUAL = "EQUAL";
 const GREATER = "GREATER";
 const NONE = {tag: "NONE"};
 const revAppend = function(cont2, tmp6, ys) {
  let tmp7 = tmp6, tmp8 = ys;
  tmp: for (;;) {
   const tmp9 = tmp7, ys1 = tmp8;
   if (tmp9 === null) {
    return [false, cont2, [ys1]];
   } else if (! (tmp9 === null)) {
    const tmp10 = tmp9[0];
    const tmp11 = tmp9[1];
    [tmp7, tmp8] = [tmp11, [tmp10, ys1]];
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
    const tmp6 = a1[0];
    const tmp7 = a1[1];
    const cont4 = function(tmp8) {
     const cont5 = function(tmp9) {
      const cont6 = function(tmp10) {
       return [false, cont3, [[tmp8, tmp10]]];
      };
      return [false, tmp9, [cont6, tmp7]];
     };
     return [false, map, [cont5, a]];
    };
    return [false, a, [cont4, tmp6]];
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
     const cont4 = function(tmp6) {
      const cont5 = function(tmp7) {
       return [false, cont3, [[tmp6, tmp7]]];
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
     const tmp6 = _Int54_add(i, 1);
     const cont4 = function(tmp7) {
      return [false, cont3, [tmp6, [tmp7, acc]]];
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
 const tmp1 = tmp.isNaN;
 const tmp2 = globalThis.Math.trunc;
 const tmp3 = globalThis.BigInt;
 const tmp4 = tmp3.asUintN;
 const tmp5 = globalThis.Date.now;
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
 const sub2 = function(cont2, arr, i) {
  if (i < 0 || arr.length <= i) {
   throw _Subscript;
  } else {
   return [false, cont2, [arr[i]]];
  }
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
 const resultToInt = function(cont2, a) {
  if (tmp1(a)) {
   throw Domain;
  } else if (a < - 2.147483648e9 || a > 2.147483647e9) {
   throw _Overflow;
  } else {
   return [false, cont2, [a | 0]];
  }
 };
 const substring = function(cont2, s, i, j) {
  let tmp6;
  cont: {
   if (i < 0) {
    tmp6 = true;
    break cont;
   } else if (j < 0) {
    tmp6 = true;
    break cont;
   } else {
    const tmp7 = s.length;
    tmp6 = tmp7 < _Int54_add(i, j);
    break cont;
   }
  }
  if (tmp6) {
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
    const tmp6 = Uint8Array.of(a);
    let tmp7 = _Int54_sub(a1, a2.length), tmp8 = _list([a2]);
    tmp: for (;;) {
     const j = tmp7, acc = tmp8;
     if (j <= 0) {
      return [false, cont3, [_String_concat(acc)]];
     } else {
      [tmp7, tmp8] = [_Int54_sub(j, 1), [tmp6, acc]];
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
     const tmp6 = _Int54_sub(i, 1);
     const cont5 = function(tmp7) {
      return [false, cont4, [tmp6, tmp7]];
     };
     return [false, a, [cont5, [a2[i], acc]]];
    }
   };
   return [false, cont4, [_Int54_sub(a2.length, 1), a1]];
  }]];
 };
 const MonoSequence = function(cont2, fromList1, length1, maxLen, tmp6, create, tmp7, tmp8, tmp9, tmp10, fromList2, length2, maxLen1, tmp11, vector, tmp12, tmp13, tmp14) {
  const tabulate1 = function(cont3, a) {
   const n = a[0];
   const cont4 = function(tmp15) {
    return [false, tmp12, [cont3, [n, tmp15]]];
   };
   return [false, tabulate, [cont4, n, a[1]]];
  };
  const sub3 = function(cont3, a) {
   const v = a[0];
   const i = a[1];
   const cont4 = function(tmp15) {
    if (tmp15) {
     return [false, tmp14, [cont3, [v, i]]];
    } else {
     throw _Subscript;
    }
   };
   if (0 <= i) {
    const cont5 = function(tmp15) {
     return [false, cont4, [i < tmp15]];
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
   const cont4 = function(tmp15) {
    const cont5 = function(tmp16) {
     const tmp17 = _Int54_add(i, 1);
     const cont6 = function(tmp18) {
      const cont7 = function(tmp19) {
       return [false, tmp10, [cont3, _list([tmp15, tmp16, tmp19])]];
      };
      return [false, tmp11, [cont7, {base: v, length: _Int54_sub(_Int54_sub(tmp18, i), 1), start: tmp17}]];
     };
     return [false, length2, [cont6, v]];
    };
    return [false, fromList2, [cont5, _list([x])]];
   };
   return [false, tmp11, [cont4, {base: v, length: i, start: 0}]];
  };
  const appi = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const cont5 = function(n) {
     let cont6;
     cont6 = function(a2) {
      if (a2 >= n) {
       return [false, cont4, [undefined]];
      } else {
       const cont7 = function(tmp15) {
	const cont8 = function(tmp16) {
	 return [false, cont6, [_Int54_add(a2, 1)]];
	};
	return [false, a, [cont8, [a2, tmp15]]];
       };
       return [false, tmp14, [cont7, [a1, a2]]];
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
       const cont7 = function(tmp15) {
	const cont8 = function(tmp16) {
	 return [false, cont6, [_Int54_add(a2, 1)]];
	};
	return [false, a, [cont8, tmp15]];
       };
       return [false, tmp14, [cont7, [a1, a2]]];
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
     const cont6 = function(tmp15) {
      return [false, tmp12, [cont4, [n, tmp15]]];
     };
     return [false, tabulate, [cont6, n, function(cont7, i) {
      const cont8 = function(tmp15) {
       return [false, a, [cont7, [i, tmp15]]];
      };
      return [false, tmp14, [cont8, [a1, i]]];
     }]];
    };
    return [false, length2, [cont5, a1]];
   }]];
  };
  const map1 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const cont5 = function(n) {
     const cont6 = function(tmp15) {
      return [false, tmp12, [cont4, [n, tmp15]]];
     };
     return [false, tabulate, [cont6, n, function(cont7, i) {
      const cont8 = function(tmp15) {
       return [false, a, [cont7, tmp15]];
      };
      return [false, tmp14, [cont8, [a1, i]]];
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
	const tmp15 = _Int54_add(i, 1);
	const cont8 = function(tmp16) {
	 const cont9 = function(tmp17) {
	  return [false, cont7, [tmp15, tmp17]];
	 };
	 return [false, a, [cont9, [i, tmp16, acc]]];
	};
	return [false, tmp14, [cont8, [a2, i]]];
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
     const cont6 = function(tmp15) {
      let cont7;
      cont7 = function(i, acc) {
       if (i < 0) {
	return [false, cont5, [acc]];
       } else {
	const tmp16 = _Int54_sub(i, 1);
	const cont8 = function(tmp17) {
	 const cont9 = function(tmp18) {
	  return [false, cont7, [tmp16, tmp18]];
	 };
	 return [false, a, [cont9, [i, tmp17, acc]]];
	};
	return [false, tmp14, [cont8, [a2, i]]];
       }
      };
      return [false, cont7, [_Int54_sub(tmp15, 1), a1]];
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
	const tmp15 = _Int54_add(i, 1);
	const cont8 = function(tmp16) {
	 const cont9 = function(tmp17) {
	  return [false, cont7, [tmp15, tmp17]];
	 };
	 return [false, a, [cont9, [tmp16, acc]]];
	};
	return [false, tmp14, [cont8, [a2, i]]];
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
     const cont6 = function(tmp15) {
      let cont7;
      cont7 = function(i, acc) {
       if (i < 0) {
	return [false, cont5, [acc]];
       } else {
	const tmp16 = _Int54_sub(i, 1);
	const cont8 = function(tmp17) {
	 const cont9 = function(tmp18) {
	  return [false, cont7, [tmp16, tmp18]];
	 };
	 return [false, a, [cont9, [tmp17, acc]]];
	};
	return [false, tmp14, [cont8, [a2, i]]];
       }
      };
      return [false, cont7, [_Int54_sub(tmp15, 1), a1]];
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
	const cont8 = function(tmp15) {
	 if (tmp15) {
	  return [false, cont4, [{tag: "SOME", payload: [a2, x]}]];
	 } else {
	  return [false, cont6, [_Int54_add(a2, 1)]];
	 }
	};
	return [false, a, [cont8, [a2, x]]];
       };
       return [false, tmp14, [cont7, [a1, a2]]];
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
	const cont8 = function(tmp15) {
	 if (tmp15) {
	  return [false, cont4, [{tag: "SOME", payload: x}]];
	 } else {
	  return [false, cont6, [_Int54_add(a2, 1)]];
	 }
	};
	return [false, a, [cont8, x]];
       };
       return [false, tmp14, [cont7, [a1, a2]]];
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
       const cont7 = function(tmp15) {
	const cont8 = function(tmp16) {
	 if (tmp16) {
	  return [false, cont4, [true]];
	 } else {
	  return [false, cont6, [_Int54_add(a2, 1)]];
	 }
	};
	return [false, a, [cont8, tmp15]];
       };
       return [false, tmp14, [cont7, [a1, a2]]];
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
       const cont7 = function(tmp15) {
	const cont8 = function(tmp16) {
	 if (tmp16) {
	  return [false, cont6, [_Int54_add(a2, 1)]];
	 } else {
	  return [false, cont4, [false]];
	 }
	};
	return [false, a, [cont8, tmp15]];
       };
       return [false, tmp14, [cont7, [a1, a2]]];
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
       const tmp15 = xl <= a2;
       const tmp16 = yl <= a2;
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
	 const cont8 = function(tmp19) {
	  const cont9 = function(tmp20) {
	   const cont10 = function(exp) {
	    if (exp === "EQUAL") {
	     return [false, cont7, [_Int54_add(a2, 1)]];
	    } else {
	     return [false, cont4, [exp]];
	    }
	   };
	   return [false, a, [cont10, [tmp19, tmp20]]];
	  };
	  return [false, tmp14, [cont9, [ys, a2]]];
	 };
	 return [false, tmp14, [cont8, [xs, a2]]];
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
   const cont4 = function(tmp15) {
    const cont5 = function(tmp16) {
     return [false, tmp16, [cont3, a]];
    };
    return [false, tmp15, [cont5, null]];
   };
   return [false, foldr1, [cont4, $COLON$COLON]];
  };
  const append = function(cont3, a) {
   const v = a[0];
   const cont4 = function(tmp15) {
    return [false, tmp10, [cont3, _list([v, tmp15])]];
   };
   return [false, fromList2, [cont4, _list([a[1]])]];
  };
  const prepend = function(cont3, a) {
   const x = a[0];
   const v = a[1];
   const cont4 = function(tmp15) {
    return [false, tmp10, [cont3, _list([tmp15, v])]];
   };
   return [false, fromList2, [cont4, _list([x])]];
  };
  const length3 = function(cont3, tmp15) {
   return [false, cont3, [tmp15.length]];
  };
  const sub4 = function(cont3, a) {
   const base2 = a[0].base;
   const start = a[0].start;
   const length5 = a[0].length;
   const i = a[1];
   if (0 <= i && i < length5) {
    return [false, tmp14, [cont3, [base2, _Int54_add(start, i)]]];
   } else {
    throw _Subscript;
   }
  };
  const full = function(cont3, a) {
   const cont4 = function(tmp15) {
    return [false, cont3, [{base: a, length: tmp15, start: 0}]];
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
    const cont4 = function(tmp15) {
     if (tmp15) {
      return [false, cont3, [{base: v, length: n, start: i}]];
     } else {
      throw _Subscript;
     }
    };
    if (0 <= i) {
     if (0 <= n) {
      const tmp15 = _Int54_add(i, n);
      const cont5 = function(tmp16) {
       return [false, cont4, [tmp15 <= tmp16]];
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
     const tmp15 = _Int54_add(start, i);
     return [false, cont3, [{base: base2, length: _Int54_sub(length5, i), start: tmp15}]];
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
   const cont4 = function(tmp15) {
    const cont5 = function(tmp16) {
     return [false, tmp10, [cont3, tmp16]];
    };
    return [false, tmp15, [cont5, a]];
   };
   return [false, map, [cont4, tmp11]];
  };
  const isEmpty = function(cont3, a) {
   return [false, cont3, [a.length === 0]];
  };
  const getItem = function(cont3, a) {
   const base2 = a.base;
   const start = a.start;
   const length5 = a.length;
   if (length5 > 0) {
    const cont4 = function(tmp15) {
     const tmp16 = _Int54_add(start, 1);
     return [false, cont3, [{tag: "SOME", payload: [tmp15, {base: base2, length: _Int54_sub(length5, 1), start: tmp16}]}]];
    };
    return [false, tmp14, [cont4, [base2, start]]];
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
      const cont6 = function(tmp15) {
       const cont7 = function(tmp16) {
	return [false, cont5, [_Int54_add(a2, 1)]];
       };
       return [false, a, [cont7, [a2, tmp15]]];
      };
      return [false, tmp14, [cont6, [base2, _Int54_add(start, a2)]]];
     }
    };
    return [false, cont5, [0]];
   }]];
  };
  const app1 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const tmp15 = _Int54_add(start, a1.length);
    let cont5;
    cont5 = function(a2) {
     if (a2 >= tmp15) {
      return [false, cont4, [undefined]];
     } else {
      const cont6 = function(tmp16) {
       const cont7 = function(tmp17) {
	return [false, cont5, [_Int54_add(a2, 1)]];
       };
       return [false, a, [cont7, tmp16]];
      };
      return [false, tmp14, [cont6, [base2, a2]]];
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
      return [false, tmp13, [cont4, [length5, acc]]];
     } else {
      const tmp15 = _Int54_add(i, 1);
      const cont6 = function(tmp16) {
       const cont7 = function(tmp17) {
	return [false, cont5, [tmp15, [tmp17, acc]]];
       };
       return [false, a, [cont7, [i, tmp16]]];
      };
      return [false, tmp14, [cont6, [base2, _Int54_add(start, i)]]];
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
    const tmp15 = _Int54_add(start, length5);
    let cont5;
    cont5 = function(i, acc) {
     if (i >= tmp15) {
      return [false, tmp13, [cont4, [length5, acc]]];
     } else {
      const tmp16 = _Int54_add(i, 1);
      const cont6 = function(tmp17) {
       const cont7 = function(tmp18) {
	return [false, cont5, [tmp16, [tmp18, acc]]];
       };
       return [false, a, [cont7, tmp17]];
      };
      return [false, tmp14, [cont6, [base2, i]]];
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
       const tmp15 = _Int54_add(i, 1);
       const cont7 = function(tmp16) {
	const cont8 = function(tmp17) {
	 return [false, cont6, [tmp15, tmp17]];
	};
	return [false, a, [cont8, [i, tmp16, acc]]];
       };
       return [false, tmp14, [cont7, [base2, _Int54_add(start, i)]]];
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
       const tmp15 = _Int54_sub(i, 1);
       const cont7 = function(tmp16) {
	const cont8 = function(tmp17) {
	 return [false, cont6, [tmp15, tmp17]];
	};
	return [false, a, [cont8, [i, tmp16, acc]]];
       };
       return [false, tmp14, [cont7, [base2, _Int54_add(start, i)]]];
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
     const tmp15 = _Int54_add(start, a2.length);
     let cont6;
     cont6 = function(i, acc) {
      if (i >= tmp15) {
       return [false, cont5, [acc]];
      } else {
       const tmp16 = _Int54_add(i, 1);
       const cont7 = function(tmp17) {
	const cont8 = function(tmp18) {
	 return [false, cont6, [tmp16, tmp18]];
	};
	return [false, a, [cont8, [tmp17, acc]]];
       };
       return [false, tmp14, [cont7, [base2, i]]];
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
       const tmp15 = _Int54_sub(i, 1);
       const cont7 = function(tmp16) {
	const cont8 = function(tmp17) {
	 return [false, cont6, [tmp15, tmp17]];
	};
	return [false, a, [cont8, [tmp16, acc]]];
       };
       return [false, tmp14, [cont7, [base2, i]]];
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
       const cont7 = function(tmp15) {
	if (tmp15) {
	 return [false, cont4, [{tag: "SOME", payload: [a2, x]}]];
	} else {
	 return [false, cont5, [_Int54_add(a2, 1)]];
	}
       };
       return [false, a, [cont7, [a2, x]]];
      };
      return [false, tmp14, [cont6, [base2, _Int54_add(start, a2)]]];
     }
    };
    return [false, cont5, [0]];
   }]];
  };
  const find1 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const tmp15 = _Int54_add(start, a1.length);
    let cont5;
    cont5 = function(a2) {
     if (a2 >= tmp15) {
      return [false, cont4, [NONE]];
     } else {
      const cont6 = function(x) {
       const cont7 = function(tmp16) {
	if (tmp16) {
	 return [false, cont4, [{tag: "SOME", payload: x}]];
	} else {
	 return [false, cont5, [_Int54_add(a2, 1)]];
	}
       };
       return [false, a, [cont7, x]];
      };
      return [false, tmp14, [cont6, [base2, a2]]];
     }
    };
    return [false, cont5, [start]];
   }]];
  };
  const exists1 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const tmp15 = _Int54_add(start, a1.length);
    let cont5;
    cont5 = function(a2) {
     if (a2 >= tmp15) {
      return [false, cont4, [false]];
     } else {
      const cont6 = function(tmp16) {
       const cont7 = function(tmp17) {
	if (tmp17) {
	 return [false, cont4, [true]];
	} else {
	 return [false, cont5, [_Int54_add(a2, 1)]];
	}
       };
       return [false, a, [cont7, tmp16]];
      };
      return [false, tmp14, [cont6, [base2, a2]]];
     }
    };
    return [false, cont5, [start]];
   }]];
  };
  const all1 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const tmp15 = _Int54_add(start, a1.length);
    let cont5;
    cont5 = function(a2) {
     if (a2 >= tmp15) {
      return [false, cont4, [true]];
     } else {
      const cont6 = function(tmp16) {
       const cont7 = function(tmp17) {
	if (tmp17) {
	 return [false, cont5, [_Int54_add(a2, 1)]];
	} else {
	 return [false, cont4, [false]];
	}
       };
       return [false, a, [cont7, tmp16]];
      };
      return [false, tmp14, [cont6, [base2, a2]]];
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
    const tmp15 = _Int54_add(start, length5);
    const tmp16 = _Int54_add(start$PRIME, length$PRIME);
    let cont5;
    cont5 = function(i, j) {
     const tmp17 = tmp15 <= i;
     const tmp18 = tmp16 <= j;
     let tmp19;
     cont: {
      tmp19 = tmp17 && tmp18;
      break cont;
     }
     if (tmp19) {
      return [false, cont4, [EQUAL]];
     } else {
      let tmp20;
      cont: {
       tmp20 = tmp17 && ! tmp18;
       break cont;
      }
      if (tmp20) {
       return [false, cont4, [LESS]];
      } else if (! tmp17 && tmp18) {
       return [false, cont4, [GREATER]];
      } else if (! tmp17 && ! tmp18) {
       const cont6 = function(tmp21) {
	const cont7 = function(tmp22) {
	 const cont8 = function(exp) {
	  if (exp === "EQUAL") {
	   return [false, cont5, [_Int54_add(i, 1), _Int54_add(j, 1)]];
	  } else {
	   return [false, cont4, [exp]];
	  }
	 };
	 return [false, a, [cont8, [tmp21, tmp22]]];
	};
	return [false, tmp14, [cont7, [base$PRIME, j]]];
       };
       return [false, tmp14, [cont6, [base2, i]]];
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
    return [false, tmp6, [cont3, [n, init]]];
   }
  };
  const tabulate2 = function(cont3, a) {
   const n = a[0];
   const f = a[1];
   if (maxLen < n) {
    throw _Size;
   } else {
    const cont4 = function(tmp15) {
     return [false, tmp7, [cont3, [n, tmp15]]];
    };
    return [false, tabulate, [cont4, n, f]];
   }
  };
  const sub5 = function(cont3, a) {
   const a1 = a[0];
   const i = a[1];
   const cont4 = function(tmp15) {
    if (tmp15) {
     return [false, tmp8, [cont3, [a1, i]]];
    } else {
     throw _Subscript;
    }
   };
   if (0 <= i) {
    const cont5 = function(tmp15) {
     return [false, cont4, [i < tmp15]];
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
   const cont4 = function(tmp15) {
    if (tmp15) {
     return [false, tmp9, [cont3, [a1, i, x]]];
    } else {
     throw _Subscript;
    }
   };
   if (0 <= i) {
    const cont5 = function(tmp15) {
     return [false, cont4, [i < tmp15]];
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
    const cont5 = function(tmp15) {
     if (tmp15) {
      let cont6;
      cont6 = function(a1) {
       if (a1 >= srcLen) {
	return [false, cont3, [undefined]];
       } else {
	const tmp16 = _Int54_add(di, a1);
	const cont7 = function(tmp17) {
	 const cont8 = function(tmp18) {
	  return [false, cont6, [_Int54_add(a1, 1)]];
	 };
	 return [false, tmp9, [cont8, [dst, tmp16, tmp17]]];
	};
	return [false, tmp8, [cont7, [src, a1]]];
       }
      };
      return [false, cont6, [0]];
     } else {
      throw _Subscript;
     }
    };
    if (0 <= di) {
     const tmp15 = _Int54_add(di, srcLen);
     const cont6 = function(tmp16) {
      return [false, cont5, [tmp15 <= tmp16]];
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
    const cont5 = function(tmp15) {
     if (tmp15) {
      let cont6;
      cont6 = function(a1) {
       if (a1 >= srcLen) {
	return [false, cont3, [undefined]];
       } else {
	const tmp16 = _Int54_add(di, a1);
	const cont7 = function(tmp17) {
	 const cont8 = function(tmp18) {
	  return [false, cont6, [_Int54_add(a1, 1)]];
	 };
	 return [false, tmp9, [cont8, [dst, tmp16, tmp17]]];
	};
	return [false, tmp14, [cont7, [src, a1]]];
       }
      };
      return [false, cont6, [0]];
     } else {
      throw _Subscript;
     }
    };
    if (0 <= di) {
     const tmp15 = _Int54_add(di, srcLen);
     const cont6 = function(tmp16) {
      return [false, cont5, [tmp15 <= tmp16]];
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
       const cont7 = function(tmp15) {
	const cont8 = function(tmp16) {
	 return [false, cont6, [_Int54_add(a2, 1)]];
	};
	return [false, a, [cont8, [a2, tmp15]]];
       };
       return [false, tmp8, [cont7, [a1, a2]]];
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
       const cont7 = function(tmp15) {
	const cont8 = function(tmp16) {
	 return [false, cont6, [_Int54_add(a2, 1)]];
	};
	return [false, a, [cont8, tmp15]];
       };
       return [false, tmp8, [cont7, [a1, a2]]];
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
	 const cont9 = function(tmp15) {
	  return [false, cont6, [_Int54_add(a2, 1)]];
	 };
	 return [false, tmp9, [cont9, [a1, a2, y]]];
	};
	return [false, a, [cont8, [a2, x]]];
       };
       return [false, tmp8, [cont7, [a1, a2]]];
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
	 const cont9 = function(tmp15) {
	  return [false, cont6, [_Int54_add(a2, 1)]];
	 };
	 return [false, tmp9, [cont9, [a1, a2, y]]];
	};
	return [false, a, [cont8, x]];
       };
       return [false, tmp8, [cont7, [a1, a2]]];
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
	const tmp15 = _Int54_add(i, 1);
	const cont8 = function(tmp16) {
	 const cont9 = function(tmp17) {
	  return [false, cont7, [tmp15, tmp17]];
	 };
	 return [false, a, [cont9, [i, tmp16, acc]]];
	};
	return [false, tmp8, [cont8, [a2, i]]];
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
     const cont6 = function(tmp15) {
      let cont7;
      cont7 = function(i, acc) {
       if (i < 0) {
	return [false, cont5, [acc]];
       } else {
	const tmp16 = _Int54_sub(i, 1);
	const cont8 = function(tmp17) {
	 const cont9 = function(tmp18) {
	  return [false, cont7, [tmp16, tmp18]];
	 };
	 return [false, a, [cont9, [i, tmp17, acc]]];
	};
	return [false, tmp8, [cont8, [a2, i]]];
       }
      };
      return [false, cont7, [_Int54_sub(tmp15, 1), a1]];
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
	const tmp15 = _Int54_add(i, 1);
	const cont8 = function(tmp16) {
	 const cont9 = function(tmp17) {
	  return [false, cont7, [tmp15, tmp17]];
	 };
	 return [false, a, [cont9, [tmp16, acc]]];
	};
	return [false, tmp8, [cont8, [a2, i]]];
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
     const cont6 = function(tmp15) {
      let cont7;
      cont7 = function(i, acc) {
       if (i < 0) {
	return [false, cont5, [acc]];
       } else {
	const tmp16 = _Int54_sub(i, 1);
	const cont8 = function(tmp17) {
	 const cont9 = function(tmp18) {
	  return [false, cont7, [tmp16, tmp18]];
	 };
	 return [false, a, [cont9, [tmp17, acc]]];
	};
	return [false, tmp8, [cont8, [a2, i]]];
       }
      };
      return [false, cont7, [_Int54_sub(tmp15, 1), a1]];
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
	const cont8 = function(tmp15) {
	 if (tmp15) {
	  return [false, cont4, [{tag: "SOME", payload: [a2, x]}]];
	 } else {
	  return [false, cont6, [_Int54_add(a2, 1)]];
	 }
	};
	return [false, a, [cont8, [a2, x]]];
       };
       return [false, tmp8, [cont7, [a1, a2]]];
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
	const cont8 = function(tmp15) {
	 if (tmp15) {
	  return [false, cont4, [{tag: "SOME", payload: x}]];
	 } else {
	  return [false, cont6, [_Int54_add(a2, 1)]];
	 }
	};
	return [false, a, [cont8, x]];
       };
       return [false, tmp8, [cont7, [a1, a2]]];
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
       const cont7 = function(tmp15) {
	const cont8 = function(tmp16) {
	 if (tmp16) {
	  return [false, cont4, [true]];
	 } else {
	  return [false, cont6, [_Int54_add(a2, 1)]];
	 }
	};
	return [false, a, [cont8, tmp15]];
       };
       return [false, tmp8, [cont7, [a1, a2]]];
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
       const cont7 = function(tmp15) {
	const cont8 = function(tmp16) {
	 if (tmp16) {
	  return [false, cont6, [_Int54_add(a2, 1)]];
	 } else {
	  return [false, cont4, [false]];
	 }
	};
	return [false, a, [cont8, tmp15]];
       };
       return [false, tmp8, [cont7, [a1, a2]]];
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
       const tmp15 = xl <= a2;
       const tmp16 = yl <= a2;
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
	 const cont8 = function(tmp19) {
	  const cont9 = function(tmp20) {
	   const cont10 = function(exp) {
	    if (exp === "EQUAL") {
	     return [false, cont7, [_Int54_add(a2, 1)]];
	    } else {
	     return [false, cont4, [exp]];
	    }
	   };
	   return [false, a, [cont10, [tmp19, tmp20]]];
	  };
	  return [false, tmp8, [cont9, [ys, a2]]];
	 };
	 return [false, tmp8, [cont8, [xs, a2]]];
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
   const cont4 = function(tmp15) {
    const cont5 = function(tmp16) {
     return [false, tmp16, [cont3, a]];
    };
    return [false, tmp15, [cont5, null]];
   };
   return [false, foldr3, [cont4, $COLON$COLON]];
  };
  const vector1 = function(cont3, a) {
   const cont4 = function(tmp15) {
    const cont5 = function(tmp16) {
     return [false, tmp12, [cont3, [tmp15, tmp16]]];
    };
    const cont6 = function(tmp16) {
     const cont7 = function(tmp17) {
      return [false, tmp17, [cont5, a]];
     };
     return [false, tmp16, [cont7, null]];
    };
    return [false, foldr3, [cont6, $COLON$COLON]];
   };
   return [false, length1, [cont4, a]];
  };
  const fromVector = function(cont3, a) {
   const cont4 = function(tmp15) {
    const cont5 = function(tmp16) {
     return [false, tmp7, [cont3, [tmp15, tmp16]]];
    };
    const cont6 = function(tmp16) {
     const cont7 = function(tmp17) {
      return [false, tmp17, [cont5, a]];
     };
     return [false, tmp16, [cont7, null]];
    };
    return [false, foldr1, [cont6, $COLON$COLON]];
   };
   return [false, length2, [cont4, a]];
  };
  const length4 = function(cont3, tmp15) {
   return [false, cont3, [tmp15.length]];
  };
  const sub6 = function(cont3, a) {
   const base2 = a[0].base;
   const start = a[0].start;
   const length5 = a[0].length;
   const i = a[1];
   if (0 <= i && i < length5) {
    return [false, tmp8, [cont3, [base2, _Int54_add(start, i)]]];
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
    return [false, tmp9, [cont3, [base2, _Int54_add(start, i), x]]];
   } else {
    throw _Subscript;
   }
  };
  const full1 = function(cont3, a) {
   const cont4 = function(tmp15) {
    return [false, cont3, [{base: a, length: tmp15, start: 0}]];
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
    const cont4 = function(tmp15) {
     if (tmp15) {
      return [false, cont3, [{base: v, length: n, start: i}]];
     } else {
      throw _Subscript;
     }
    };
    if (0 <= i) {
     if (0 <= n) {
      const tmp15 = _Int54_add(i, n);
      const cont5 = function(tmp16) {
       return [false, cont4, [tmp15 <= tmp16]];
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
     const tmp15 = _Int54_add(start, i);
     return [false, cont3, [{base: base2, length: _Int54_sub(length5, i), start: tmp15}]];
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
   const cont4 = function(tmp15) {
    if (tmp15) {
     throw _Subscript;
    } else if (start >= di) {
     let cont5;
     cont5 = function(a1) {
      if (a1 >= length5) {
       return [false, cont3, [undefined]];
      } else {
       const tmp16 = _Int54_add(di, a1);
       const cont6 = function(tmp17) {
	const cont7 = function(tmp18) {
	 return [false, cont5, [_Int54_add(a1, 1)]];
	};
	return [false, tmp9, [cont7, [dst, tmp16, tmp17]]];
       };
       return [false, tmp8, [cont6, [base2, _Int54_add(start, a1)]]];
      }
     };
     return [false, cont5, [0]];
    } else {
     let cont5;
     cont5 = function(a1) {
      if (a1 < 0) {
       return [false, cont3, [undefined]];
      } else {
       const tmp16 = _Int54_add(di, a1);
       const cont6 = function(tmp17) {
	const cont7 = function(tmp18) {
	 return [false, cont5, [_Int54_sub(a1, 1)]];
	};
	return [false, tmp9, [cont7, [dst, tmp16, tmp17]]];
       };
       return [false, tmp8, [cont6, [base2, _Int54_add(start, a1)]]];
      }
     };
     return [false, cont5, [_Int54_sub(length5, 1)]];
    }
   };
   if (di < 0) {
    return [false, cont4, [true]];
   } else {
    const cont5 = function(tmp15) {
     return [false, cont4, [tmp15 < _Int54_add(di, length5)]];
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
    const cont4 = function(tmp15) {
     if (tmp15 < _Int54_add(di, length5)) {
      throw _Subscript;
     } else {
      let cont5;
      cont5 = function(a1) {
       if (a1 >= length5) {
	return [false, cont3, [undefined]];
       } else {
	const tmp16 = _Int54_add(di, a1);
	const cont6 = function(tmp17) {
	 const cont7 = function(tmp18) {
	  return [false, cont5, [_Int54_add(a1, 1)]];
	 };
	 return [false, tmp9, [cont7, [dst, tmp16, tmp17]]];
	};
	return [false, tmp14, [cont6, [base2, _Int54_add(start, a1)]]];
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
    const cont4 = function(tmp15) {
     const tmp16 = _Int54_add(start, 1);
     return [false, cont3, [{tag: "SOME", payload: [tmp15, {base: base2, length: _Int54_sub(length5, 1), start: tmp16}]}]];
    };
    return [false, tmp8, [cont4, [base2, start]]];
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
      const cont6 = function(tmp15) {
       const cont7 = function(tmp16) {
	return [false, cont5, [_Int54_add(a2, 1)]];
       };
       return [false, a, [cont7, [a2, tmp15]]];
      };
      return [false, tmp8, [cont6, [base2, _Int54_add(start, a2)]]];
     }
    };
    return [false, cont5, [0]];
   }]];
  };
  const app3 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const tmp15 = _Int54_add(start, a1.length);
    let cont5;
    cont5 = function(a2) {
     if (a2 >= tmp15) {
      return [false, cont4, [undefined]];
     } else {
      const cont6 = function(tmp16) {
       const cont7 = function(tmp17) {
	return [false, cont5, [_Int54_add(a2, 1)]];
       };
       return [false, a, [cont7, tmp16]];
      };
      return [false, tmp8, [cont6, [base2, a2]]];
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
      const tmp15 = _Int54_add(start, a2);
      const cont6 = function(x) {
       const cont7 = function(y) {
	const cont8 = function(tmp16) {
	 return [false, cont5, [_Int54_add(a2, 1)]];
	};
	return [false, tmp9, [cont8, [base2, tmp15, y]]];
       };
       return [false, a, [cont7, [a2, x]]];
      };
      return [false, tmp8, [cont6, [base2, tmp15]]];
     }
    };
    return [false, cont5, [0]];
   }]];
  };
  const modify1 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const tmp15 = _Int54_add(start, a1.length);
    let cont5;
    cont5 = function(a2) {
     if (a2 >= tmp15) {
      return [false, cont4, [undefined]];
     } else {
      const cont6 = function(x) {
       const cont7 = function(y) {
	const cont8 = function(tmp16) {
	 return [false, cont5, [_Int54_add(a2, 1)]];
	};
	return [false, tmp9, [cont8, [base2, a2, y]]];
       };
       return [false, a, [cont7, x]];
      };
      return [false, tmp8, [cont6, [base2, a2]]];
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
       const tmp15 = _Int54_add(i, 1);
       const cont7 = function(tmp16) {
	const cont8 = function(tmp17) {
	 return [false, cont6, [tmp15, tmp17]];
	};
	return [false, a, [cont8, [i, tmp16, acc]]];
       };
       return [false, tmp8, [cont7, [base2, _Int54_add(start, i)]]];
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
       const tmp15 = _Int54_sub(i, 1);
       const cont7 = function(tmp16) {
	const cont8 = function(tmp17) {
	 return [false, cont6, [tmp15, tmp17]];
	};
	return [false, a, [cont8, [i, tmp16, acc]]];
       };
       return [false, tmp8, [cont7, [base2, _Int54_add(start, i)]]];
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
     const tmp15 = _Int54_add(start, a2.length);
     let cont6;
     cont6 = function(i, acc) {
      if (i >= tmp15) {
       return [false, cont5, [acc]];
      } else {
       const tmp16 = _Int54_add(i, 1);
       const cont7 = function(tmp17) {
	const cont8 = function(tmp18) {
	 return [false, cont6, [tmp16, tmp18]];
	};
	return [false, a, [cont8, [tmp17, acc]]];
       };
       return [false, tmp8, [cont7, [base2, i]]];
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
       const tmp15 = _Int54_sub(i, 1);
       const cont7 = function(tmp16) {
	const cont8 = function(tmp17) {
	 return [false, cont6, [tmp15, tmp17]];
	};
	return [false, a, [cont8, [tmp16, acc]]];
       };
       return [false, tmp8, [cont7, [base2, i]]];
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
       const cont7 = function(tmp15) {
	if (tmp15) {
	 return [false, cont4, [{tag: "SOME", payload: [a2, x]}]];
	} else {
	 return [false, cont5, [_Int54_add(a2, 1)]];
	}
       };
       return [false, a, [cont7, [a2, x]]];
      };
      return [false, tmp8, [cont6, [base2, _Int54_add(start, a2)]]];
     }
    };
    return [false, cont5, [0]];
   }]];
  };
  const find3 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const tmp15 = _Int54_add(start, a1.length);
    let cont5;
    cont5 = function(a2) {
     if (a2 >= tmp15) {
      return [false, cont4, [NONE]];
     } else {
      const cont6 = function(x) {
       const cont7 = function(tmp16) {
	if (tmp16) {
	 return [false, cont4, [{tag: "SOME", payload: x}]];
	} else {
	 return [false, cont5, [_Int54_add(a2, 1)]];
	}
       };
       return [false, a, [cont7, x]];
      };
      return [false, tmp8, [cont6, [base2, a2]]];
     }
    };
    return [false, cont5, [start]];
   }]];
  };
  const exists3 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const tmp15 = _Int54_add(start, a1.length);
    let cont5;
    cont5 = function(a2) {
     if (a2 >= tmp15) {
      return [false, cont4, [false]];
     } else {
      const cont6 = function(tmp16) {
       const cont7 = function(tmp17) {
	if (tmp17) {
	 return [false, cont4, [true]];
	} else {
	 return [false, cont5, [_Int54_add(a2, 1)]];
	}
       };
       return [false, a, [cont7, tmp16]];
      };
      return [false, tmp8, [cont6, [base2, a2]]];
     }
    };
    return [false, cont5, [start]];
   }]];
  };
  const all3 = function(cont3, a) {
   return [false, cont3, [function(cont4, a1) {
    const base2 = a1.base;
    const start = a1.start;
    const tmp15 = _Int54_add(start, a1.length);
    let cont5;
    cont5 = function(a2) {
     if (a2 >= tmp15) {
      return [false, cont4, [true]];
     } else {
      const cont6 = function(tmp16) {
       const cont7 = function(tmp17) {
	if (tmp17) {
	 return [false, cont5, [_Int54_add(a2, 1)]];
	} else {
	 return [false, cont4, [false]];
	}
       };
       return [false, a, [cont7, tmp16]];
      };
      return [false, tmp8, [cont6, [base2, a2]]];
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
    const tmp15 = _Int54_add(start, length5);
    const tmp16 = _Int54_add(start$PRIME, length$PRIME);
    let cont5;
    cont5 = function(i, j) {
     const tmp17 = tmp15 <= i;
     const tmp18 = tmp16 <= j;
     let tmp19;
     cont: {
      tmp19 = tmp17 && tmp18;
      break cont;
     }
     if (tmp19) {
      return [false, cont4, [EQUAL]];
     } else {
      let tmp20;
      cont: {
       tmp20 = tmp17 && ! tmp18;
       break cont;
      }
      if (tmp20) {
       return [false, cont4, [LESS]];
      } else if (! tmp17 && tmp18) {
       return [false, cont4, [GREATER]];
      } else if (! tmp17 && ! tmp18) {
       const cont6 = function(tmp21) {
	const cont7 = function(tmp22) {
	 const cont8 = function(exp) {
	  if (exp === "EQUAL") {
	   return [false, cont5, [_Int54_add(i, 1), _Int54_add(j, 1)]];
	  } else {
	   return [false, cont4, [exp]];
	  }
	 };
	 return [false, a, [cont8, [tmp21, tmp22]]];
	};
	return [false, tmp8, [cont7, [base$PRIME, j]]];
       };
       return [false, tmp8, [cont6, [base2, i]]];
      } else {
       throw _Match;
      }
     }
    };
    return [false, cont5, [start, start$PRIME]];
   }]];
  };
  const vector2 = function(cont3, a) {
   const tmp15 = a.length;
   const cont4 = function(tmp16) {
    const cont5 = function(tmp17) {
     const cont6 = function(tmp18) {
      return [false, tmp12, [cont3, [tmp15, tmp18]]];
     };
     return [false, tmp17, [cont6, a]];
    };
    return [false, tmp16, [cont5, null]];
   };
   return [false, foldr4, [cont4, $COLON$COLON]];
  };
  const UnsafeMonoVector = {sub: tmp14};
  const UnsafeMonoArray = {create: create, sub: tmp8, update: tmp9};
  const MonoVectorSlice = {all: all1, app: app1, appi: appi1, base: base, collate: collate1, concat: concat1, exists: exists1, find: find1, findi: findi1, foldl: foldl1, foldli: foldli1, foldr: foldr2, foldri: foldri1, full: full, getItem: getItem, isEmpty: isEmpty, length: length3, map: map2, mapi: mapi1, slice: slice, sub: sub4, subslice: subslice, vector: vector};
  const MonoVector = {all: all, app: app, append: append, appi: appi, collate: collate, concat: tmp10, exists: exists, find: find, findi: findi, foldl: foldl, foldli: foldli, foldr: foldr1, foldri: foldri, fromList: fromList2, length: length2, map: map1, mapi: mapi, maxLen: maxLen1, prepend: prepend, sub: sub3, tabulate: tabulate1, toList: toList, update: update2};
  const MonoArraySlice = {all: all3, app: app3, appi: appi3, base: base1, collate: collate3, copy: copy1, copyVec: copyVec1, exists: exists3, find: find3, findi: findi3, foldl: foldl3, foldli: foldli3, foldr: foldr4, foldri: foldri3, full: full1, getItem: getItem1, isEmpty: isEmpty1, length: length4, modify: modify1, modifyi: modifyi1, slice: slice1, sub: sub6, subslice: subslice1, update: update4, vector: vector2};
  return [false, cont2, [{_MonoArray: {all: all2, app: app2, appi: appi2, array: array1, collate: collate2, copy: copy, copyVec: copyVec, exists: exists2, find: find2, findi: findi2, foldl: foldl2, foldli: foldli2, foldr: foldr3, foldri: foldri2, fromList: fromList1, fromVector: fromVector, length: length1, maxLen: maxLen, modify: modify, modifyi: modifyi, sub: sub5, tabulate: tabulate2, toList: toList1, toVector: vector1, update: update3, vector: vector1}, _MonoArraySlice: MonoArraySlice, _MonoVector: MonoVector, _MonoVectorSlice: MonoVectorSlice, _UnsafeMonoArray: UnsafeMonoArray, _UnsafeMonoVector: UnsafeMonoVector}]];
 };
 const unsafeFromListN = function(cont2, a) {
  return [false, cont2, [_String_implode(a[1])]];
 };
 const unsafeFromListRevN = function(cont2, a) {
  const cont3 = function(tmp6) {
   return [false, cont2, [_String_implode(tmp6)]];
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
  const tmp6 = Base._MonoVectorSlice.full;
  const tmp7 = CharVector.appi;
  const unsafeSub = function(cont3, a) {
   const v = a[0];
   const cont4 = function(tmp8) {
    return [false, cont3, [(tmp8 >>> 0 & 0xFF) >>> 0]];
   };
   return [false, sub1, [cont4, [v, a[1]]]];
  };
  const fromList1 = function(cont3, a) {
   const cont4 = function(tmp8) {
    const cont5 = function(tmp9) {
     return [false, cont3, [_String_implode(tmp9)]];
    };
    return [false, tmp8, [cont5, a]];
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
   const cont4 = function(tmp8) {
    const cont5 = function(tmp9) {
     const cont6 = function(tmp10) {
      return [false, cont3, [_String_implode(tmp10)]];
     };
     return [false, revAppend, [cont6, tmp9, null]];
    };
    return [false, tmp8, [cont5, xs]];
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
   const tmp8 = Word8Vector.fromList;
   const tmp9 = Word8Vector.tabulate;
   const tmp10 = Word8Array.array;
   const tmp11 = Word8Array.maxLen;
   const tmp12 = Word8Array.sub;
   const tmp13 = Word8Array.update;
   const Io_tag = function(payload) {
    this.payload = payload;
   };
   Io_tag.prototype.name = "Io";
   const BlockingNotSupported_tag = function() {
   };
   BlockingNotSupported_tag.prototype.name = "BlockingNotSupported";
   const BlockingNotSupported = new BlockingNotSupported_tag();
   const BLOCK_BUF = "BLOCK_BUF";
   const outputAndFlush = function(cont4, tmp19, content) {
    if (tmp19.tag === "JS_WRITABLE") {
     const writable = tmp19.payload.writable;
     return [false, _withSubCont, [cont4, _topLevel, function(cont5, a) {
      writable.write(content, null, _function(function(cont6, args) {
       const cont7 = function(tmp20) {
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
    } else if (tmp19.tag === "PRIM_WRITER") {
     const name = tmp19.payload.writer.name;
     const writeVec = tmp19.payload.writer.writeVec;
     const buffer = tmp19.payload.buffer;
     const cont5 = function(tmp20) {
      const tmp21 = _String_concat(tmp20);
      if (writeVec.tag === "SOME") {
       const writeVec1 = writeVec.payload;
       const cont6 = function(tmp22) {
	const cont7 = function(tmp23) {
	 buffer[0] = null;
	 return [false, cont4, [undefined]];
	};
	return [false, writeVec1, [cont7, tmp22]];
       };
       return [false, tmp6, [cont6, tmp21]];
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
   const tmp14 = {tag: "JS_WRITABLE", payload: {buffer_mode: [BLOCK_BUF], name: Uint8Array.of(60, 115, 116, 100, 111, 117, 116, 62), writable: stdout}};
   const tmp15 = tmp3(0xCBF29CE5);
   const tmp16 = tmp4(64, tmp15 * tmp3(0xFFFFFFFF));
   tmp4(64, tmp16 + tmp3(0x5014C00A));
   const tmp17 = tmp3(0x100);
   const tmp18 = tmp4(64, tmp17 * tmp3(0xFFFFFFFF));
   tmp4(64, tmp18 + tmp3(0x2B3));
   _Array_array(64, null);
   let f;
   f = function(cont4, a) {
    const tmp19 = _exh;
    _exh = function(exn) {
     _exh = tmp19;
     if (exn instanceof _Overflow_tag) {
      return [false, cont4, [a]];
     } else {
      throw exn;
     }
    };
    const cont5 = function(tmp21) {
     _exh = tmp19;
     return [false, cont4, [tmp21]];
    };
    const tmp20 = _Int54_add(a, a);
    if (tmp20 < 4294967295) {
     return [false, f, [cont5, tmp20]];
    } else {
     return [false, cont5, [a]];
    }
   };
   const cont3 = function(tmp19) {
    const cont4 = function(tbl) {
     const cont5 = function(tmp20) {
      const cont6 = function(tmp21) {
       const cont7 = function(tmp22) {
	const cont8 = function(tmp23) {
	 const cont9 = function(tmp24) {
	  const cont10 = function(tmp25) {
	   const cont11 = function(tmp26) {
	    const cont12 = function(tbl1) {
	     const cont13 = function(tmp27) {
	      const cont14 = function(tmp28) {
	       const cont15 = function(tmp29) {
		const cont16 = function(tmp30) {
		 const cont17 = function(tmp31) {
		  const cont18 = function(tmp34) {
		   48 >>> 0;
		   57 >>> 0;
		   65 >>> 0;
		   70 >>> 0;
		   97 >>> 0;
		   102 >>> 0;
		   tmp(tmp4(32, 2147483646n));
		   tmp(2147483647n);
		   const tmp35 = 1.0;
		   const tmp36 = tmp35 / (32768 * 32768);
		   tmp3(0x72646E64);
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
		   const lcg = function(cont20, a) {
		    let tmp37;
		    cont: {
		     if (q === 0x0) {
		      throw _Div;
		     } else {
		      tmp37 = a % q;
		      break cont;
		     }
		    }
		    const tmp38 = Math_imul(0xBC8F, tmp37) >>> 0;
		    let tmp39;
		    cont: {
		     if (q === 0x0) {
		      throw _Div;
		     } else {
		      tmp39 = a / q >>> 0;
		      break cont;
		     }
		    }
		    const tmp40 = Math_imul(r, tmp39) >>> 0;
		    if (tmp38 > tmp40) {
		     return [false, cont20, [tmp38 - tmp40 >>> 0]];
		    } else {
		     return [false, cont20, [(0x7FFFFFFF - tmp40 >>> 0) + tmp38 >>> 0]];
		    }
		   };
		   const rand = function(cont20, congy, shrgx) {
		    const tmp37 = (congy >>> 0 & 0x7FFFFFFF) >>> 0;
		    let tmp38;
		    cont: {
		     if (0x1 >= 0x20) {
		      tmp38 = 0x0;
		      break cont;
		     } else {
		      tmp38 = tmp37 << 0x1 >>> 0;
		      break cont;
		     }
		    }
		    const tmp39 = tmp38 + 0x1 >>> 0;
		    let cont21;
		    cont21 = function(tmp40, seeds, congx, shrgx1) {
		     if (tmp40 === 0) {
		      const tmp41 = _VectorOrArray_fromList(seeds);
		      const tmp42 = [0];
		      const tmp43 = [congx];
		      return [false, cont20, [{borrow: [false], congx: tmp43, index: tmp42, vals: tmp41}]];
		     } else {
		      let cont22;
		      cont22 = function(tmp41, seed, congx$PRIME, shrgx$PRIME) {
		       if (tmp41 === 0) {
			const tmp42 = _Int54_sub(tmp40, 1);
			return [false, cont21, [tmp42, [seed, seeds], congx$PRIME, shrgx$PRIME]];
		       } else {
			const tmp42 = _Int54_sub(tmp41, 1);
			const cont23 = function(c$PRIME) {
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
			 return [false, cont22, [tmp42, (tmp48 | (0x40000000 & (c$PRIME ^ tmp46) >>> 0) >>> 0) >>> 0, c$PRIME, tmp46]];
			};
			return [false, lcg, [cont23, congx$PRIME]];
		       }
		      };
		      return [false, cont22, [31, 0x0, congx, shrgx1]];
		     }
		    };
		    return [false, cont21, [48, null, tmp39, shrgx >>> 0]];
		   };
		   const randWord = function(cont20, a) {
		    const vals = a.vals;
		    const index = a.index;
		    const congx = a.congx;
		    const x = index[0];
		    if (x === 48) {
		     const cont21 = function(tmp37) {
		      const cont24 = function(c) {
		       congx[0] = c;
		       return [false, cont20, [(tmp37 ^ c) >>> 0]];
		      };
		      return [false, lcg, [cont24, congx[0]]];
		     };
		     const vals1 = a.vals;
		     const index1 = a.index;
		     const borrow = a.borrow;
		     const update2 = function(cont24, ix, iy, b) {
		      const cont25 = function(tmp37) {
		       const cont26 = function(tmp38) {
			let z, b$PRIME;
			cont: {
			 if (! b) {
			  const tmp39 = tmp37 - tmp38 >>> 0;
			  [z, b$PRIME] = [tmp39, tmp38 > tmp37];
			  break cont;
			 } else if (b) {
			  const tmp39 = (tmp37 - tmp38 >>> 0) - 0x1 >>> 0;
			  [z, b$PRIME] = [tmp39, tmp38 >= tmp37];
			  break cont;
			 } else {
			  throw _Match;
			 }
			}
			const cont27 = function(tmp39) {
			 return [false, cont24, [b$PRIME]];
			};
			return [false, update1, [cont27, vals1, iy, z]];
		       };
		       return [false, sub2, [cont26, vals1, iy]];
		      };
		      return [false, sub2, [cont25, vals1, ix]];
		     };
		     const cont22 = function(tmp37) {
		      let cont24;
		      cont24 = function(i, b) {
		       if (i === 48) {
			borrow[0] = b;
			index1[0] = 1;
			return [false, sub2, [cont21, vals1, 0]];
		       } else {
			const tmp38 = _Int54_add(i, 1);
			const cont25 = function(tmp39) {
			 return [false, cont24, [tmp38, tmp39]];
			};
			return [false, update2, [cont25, _Int54_sub(i, 8), i, b]];
		       }
		      };
		      return [false, cont24, [8, tmp37]];
		     };
		     let cont23;
		     cont23 = function(i, b) {
		      if (i === 8) {
		       return [false, cont22, [b]];
		      } else {
		       const tmp37 = _Int54_add(i, 1);
		       const cont24 = function(tmp38) {
			return [false, cont23, [tmp37, tmp38]];
		       };
		       return [false, update2, [cont24, _Int54_add(i, 40), i, b]];
		      }
		     };
		     return [false, cont23, [0, borrow[0]]];
		    } else {
		     const cont21 = function(tmp37) {
		      const cont22 = function(c) {
		       congx[0] = c;
		       const tmp38 = (tmp37 ^ c) >>> 0;
		       index[0] = _Int54_add(x, 1);
		       return [false, cont20, [tmp38]];
		      };
		      return [false, lcg, [cont22, congx[0]]];
		     };
		     return [false, sub2, [cont21, vals, x]];
		    }
		   };
		   const cont19 = function(tmp37) {
		    const write_to_file = function(cont21, filename, content, file_type) {
		     const tmp44 = _exh;
		     _exh = function(exn) {
		      _exh = tmp44;
		      if (exn instanceof Io_tag) {
		       return [false, outputAndFlush, [cont21, tmp14, _String_append(Uint8Array.of(10, 99, 111, 117, 108, 100, 32, 110, 111, 116, 32, 119, 114, 105, 116, 101, 32, 116, 111, 32, 102, 105, 108, 101, 58, 32), filename)]];
		      } else {
		       throw exn;
		      }
		     };
		     const cont22 = function(tmp46) {
		      _exh = tmp44;
		      return [false, cont21, [tmp46]];
		     };
		     const tmp45 = createWriteStream(filename);
		     const cont23 = function(tmp46) {
		      const cont24 = function(tmp47) {
		       if (_String_EQUAL(file_type, Uint8Array.of(98, 105, 116))) {
			return [false, outputAndFlush, [cont22, tmp14, _String_append(Uint8Array.of(10, 66, 105, 116, 32, 115, 116, 114, 101, 97, 109, 32, 104, 97, 115, 32, 98, 101, 101, 110, 32, 119, 114, 105, 116, 116, 101, 110, 32, 116, 111, 32, 100, 105, 115, 107, 32, 117, 110, 100, 101, 114, 32, 110, 97, 109, 101, 58, 32, 32), filename)]];
		       } else {
			return [false, outputAndFlush, [cont22, tmp14, _String_append(Uint8Array.of(10, 66, 121, 116, 101, 32, 115, 116, 114, 101, 97, 109, 32, 104, 97, 115, 32, 98, 101, 101, 110, 32, 119, 114, 105, 116, 116, 101, 110, 32, 116, 111, 32, 100, 105, 115, 107, 32, 117, 110, 100, 101, 114, 32, 110, 97, 109, 101, 58, 32), filename)]];
		       }
		      };
		      return [false, _withSubCont, [cont24, _topLevel, function(cont25, a) {
		       tmp45.end(_function(function(cont26, args) {
			const cont27 = function(tmp47) {
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
		     };
		     if (! tmp45.write(content)) {
		      return [false, _withSubCont, [cont23, _topLevel, function(cont24, a) {
		       tmp45.once("drain", _function(function(cont25, args) {
			const cont26 = function(tmp46) {
			 return [false, cont25, [undefined]];
			};
			return [false, _pushPrompt, [cont26, _topLevel, function(cont27, a1) {
			 return [false, _pushSubCont, [cont27, a, function(cont28, a2) {
			  return [false, cont28, [undefined]];
			 }]];
			}]];
		       }));
		       return [false, cont24, [undefined]];
		      }]];
		     } else {
		      return [false, cont23, [undefined]];
		     }
		    };
		    const tmp38 = _Array_array(62500, 0);
		    const tmp39 = _Array_array(62500, Uint8Array.of(48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48));
		    const tmp40 = _Array_array(62500, Uint8Array.of(48, 48, 48, 48));
		    const cont20 = function(secs, usecs) {
		     const cont21 = function(tmp46) {
		      let tmp47;
		      cont: {
		       if (- 9007199254740992n <= tmp46) {
			if (tmp46 <= 9007199254740991n) {
			 tmp47 = Number(tmp46);
			 break cont;
			} else {
			 throw _Overflow;
			}
		       } else {
			throw _Overflow;
		       }
		      }
		      let tmp48;
		      cont: {
		       if (- 9007199254740992n <= usecs) {
			if (usecs <= 9007199254740991n) {
			 tmp48 = Number(usecs);
			 break cont;
			} else {
			 throw _Overflow;
			}
		       } else {
			throw _Overflow;
		       }
		      }
		      const cont22 = function(r1) {
		       const cont23 = function(tmp51) {
			const cont24 = function(start_seed) {
			 const cont25 = function(tmp52) {
			  const cont26 = function() {
			   const cont28 = function(tmp53) {
			    const cont29 = function(bits_xx_el) {
			     const tmp54 = _String_concat(bits_xx_el);
			     const cont30 = function(tmp55) {
			      const cont31 = function(bits_hexx_el) {
			       const tmp56 = _String_concat(bits_hexx_el);
			       const cont32 = function(tmp57) {
				const cont33 = function(tmp58) {
				 const cont34 = function(tmp59) {
				  return [false, cont, []];
				 };
				 return [false, outputAndFlush, [cont34, tmp14, Uint8Array.of(10)]];
				};
				return [false, write_to_file, [cont33, Uint8Array.of(114, 97, 110, 100, 111, 109, 95, 98, 105, 116, 115, 116, 114, 105, 110, 103, 46, 98, 121, 116, 101), tmp56, Uint8Array.of(98, 121, 116, 101)]];
			       };
			       return [false, write_to_file, [cont32, Uint8Array.of(114, 97, 110, 100, 111, 109, 95, 98, 105, 116, 115, 116, 114, 105, 110, 103, 46, 98, 105, 110), tmp54, Uint8Array.of(98, 105, 116)]];
			      };
			      return [false, tmp55, [cont31, tmp40]];
			     };
			     return [false, foldr, [cont30, function(cont31, a) {
			      return [false, cont31, [[a[0], a[1]]]];
			     }, null]];
			    };
			    return [false, tmp53, [cont29, tmp39]];
			   };
			   return [false, foldr, [cont28, function(cont29, a) {
			    return [false, cont29, [[a[0], a[1]]]];
			   }, null]];
			  };
			  let cont27;
			  cont27 = function(i, seed) {
			   const tmp53 = _Int54_mod(_Int54_mul(17364, seed), 65521);
			   const cont28 = function(tmp54) {
			    const cont29 = function(bits_x_str) {
			     const cont31 = function(bits_hex_str) {
			      const cont33 = function(tmp55) {
			       const cont34 = function(tmp56) {
				if (i < 62499) {
				 return [false, cont27, [_Int54_add(i, 1), tmp53]];
				} else {
				 return [false, cont26, []];
				}
			       };
			       return [false, update1, [cont34, tmp40, i, bits_hex_str]];
			      };
			      return [false, update1, [cont33, tmp39, i, bits_x_str]];
			     };
			     let toHex;
			     toHex = function(cont33, a) {
			      if (a === 0) {
			       return [false, cont33, [Uint8Array.of()]];
			      } else {
			       const cont34 = function(tmp55) {
				const tmp56 = _Int54_mod(a, 16);
				if (tmp56 === 0) {
				 return [false, cont33, [_String_append(tmp55, Uint8Array.of(48))]];
				} else if (tmp56 === 1) {
				 return [false, cont33, [_String_append(tmp55, Uint8Array.of(49))]];
				} else if (tmp56 === 2) {
				 return [false, cont33, [_String_append(tmp55, Uint8Array.of(50))]];
				} else if (tmp56 === 3) {
				 return [false, cont33, [_String_append(tmp55, Uint8Array.of(51))]];
				} else if (tmp56 === 4) {
				 return [false, cont33, [_String_append(tmp55, Uint8Array.of(52))]];
				} else if (tmp56 === 5) {
				 return [false, cont33, [_String_append(tmp55, Uint8Array.of(53))]];
				} else if (tmp56 === 6) {
				 return [false, cont33, [_String_append(tmp55, Uint8Array.of(54))]];
				} else if (tmp56 === 7) {
				 return [false, cont33, [_String_append(tmp55, Uint8Array.of(55))]];
				} else if (tmp56 === 8) {
				 return [false, cont33, [_String_append(tmp55, Uint8Array.of(56))]];
				} else if (tmp56 === 9) {
				 return [false, cont33, [_String_append(tmp55, Uint8Array.of(57))]];
				} else if (tmp56 === 10) {
				 return [false, cont33, [_String_append(tmp55, Uint8Array.of(97))]];
				} else if (tmp56 === 11) {
				 return [false, cont33, [_String_append(tmp55, Uint8Array.of(98))]];
				} else if (tmp56 === 12) {
				 return [false, cont33, [_String_append(tmp55, Uint8Array.of(99))]];
				} else if (tmp56 === 13) {
				 return [false, cont33, [_String_append(tmp55, Uint8Array.of(100))]];
				} else if (tmp56 === 14) {
				 return [false, cont33, [_String_append(tmp55, Uint8Array.of(101))]];
				} else if (tmp56 === 15) {
				 return [false, cont33, [_String_append(tmp55, Uint8Array.of(102))]];
				} else {
				 return [false, cont33, [_String_append(tmp55, Uint8Array.of())]];
				}
			       };
			       return [false, toHex, [cont34, 0 + Math.floor(a / 16)]];
			      }
			     };
			     const cont32 = function(hexadec) {
			      const cont33 = function(tmp55) {
			       return [false, tmp55, [cont31, hexadec]];
			      };
			      return [false, padLeft, [cont33, 48, 4]];
			     };
			     return [false, toHex, [cont32, tmp53]];
			    };
			    let toBinary;
			    toBinary = function(cont31, a) {
			     if (a === 0) {
			      return [false, cont31, [Uint8Array.of()]];
			     } else {
			      const cont32 = function(tmp55) {
			       if (_Int54_mod(a, 2) === 0) {
				return [false, cont31, [_String_append(tmp55, Uint8Array.of(48))]];
			       } else {
				return [false, cont31, [_String_append(tmp55, Uint8Array.of(49))]];
			       }
			      };
			      return [false, toBinary, [cont32, 0 + Math.floor(a / 2)]];
			     }
			    };
			    const cont30 = function(binary) {
			     const cont31 = function(tmp55) {
			      return [false, tmp55, [cont29, binary]];
			     };
			     return [false, padLeft, [cont31, 48, 16]];
			    };
			    return [false, toBinary, [cont30, tmp53]];
			   };
			   return [false, update1, [cont28, tmp38, i, tmp53]];
			  };
			  return [false, cont27, [0, start_seed]];
			 };
			 return [false, outputAndFlush, [cont25, tmp14, Uint8Array.of(10, 103, 101, 110, 101, 114, 97, 116, 105, 110, 103, 32, 97, 32, 114, 97, 110, 100, 111, 109, 32, 98, 105, 116, 32, 115, 116, 114, 101, 97, 109, 46, 46, 46)]];
			};
			return [false, tmp51, [cont24, r1]];
		       };
		       const tmp49 = _exh;
		       _exh = function(exn) {
			_exh = tmp49;
			const tmp51 = 65521 - 1;
			const tmp52 = tmp51 + 1.0;
			return [false, cont23, [function(cont24, s) {
			 const cont25 = function(tmp53) {
			  const cont27 = function(tmp54) {
			   return [false, resultToInt, [cont24, tmp2(1 + tmp52 * ((tmp53 + ((tmp54 & 0x3FFFFFFF) >>> 0 | 0) * tmp36) * tmp36))]];
			  };
			  return [false, randWord, [cont27, s]];
			 };
			 const cont26 = function(tmp53) {
			  return [false, cont25, [(tmp53 & 0x3FFFFFFF) >>> 0 | 0]];
			 };
			 return [false, randWord, [cont26, s]];
			}]];
		       };
		       let tmp50;
		       cont: {
			const tmp51 = tmp36 * 65521;
			tmp50 = function(cont24, s) {
			 const cont25 = function(tmp52) {
			  const cont26 = function(tmp53) {
			   return [false, cont24, [_Int54_add(1, tmp53)]];
			  };
			  return [false, resultToInt, [cont26, tmp2(tmp51 * ((tmp52 & 0x3FFFFFFF) >>> 0 | 0))]];
			 };
			 return [false, randWord, [cont25, s]];
			};
			break cont;
		       }
		       _exh = tmp49;
		       return [false, cont23, [tmp50]];
		      };
		      return [false, rand, [cont22, tmp47, tmp48]];
		     };
		     const tmp44 = secs <= 0n && false;
		     let tmp45;
		     cont: {
		      tmp45 = tmp44 || secs >= 0n;
		      break cont;
		     }
		     if (tmp45) {
		      return [false, cont21, [secs % 9007199254740992n]];
		     } else {
		      const r1 = secs % 9007199254740992n;
		      if (r1 === 0n) {
		       return [false, cont21, [r1]];
		      } else {
		       return [false, cont21, [9007199254740992n + r1]];
		      }
		     }
		    };
		    const tmp41 = BigInt(tmp5()) * 1000n;
		    const tmp42 = tmp41 <= 0n && false;
		    let tmp43;
		    cont: {
		     tmp43 = tmp42 || tmp41 >= 0n;
		     break cont;
		    }
		    if (tmp43) {
		     const tmp44 = tmp41 / 1000000n;
		     return [false, cont20, [tmp44, tmp41 % 1000000n]];
		    } else {
		     const q1 = tmp41 / 1000000n;
		     const r1 = tmp41 % 1000000n;
		     if (r1 === 0n) {
		      return [false, cont20, [q1, r1]];
		     } else {
		      const tmp44 = q1 - 1n;
		      return [false, cont20, [tmp44, 1000000n + r1]];
		     }
		    }
		   };
		   return [false, rand, [cont19, 123, 73256]];
		  };
		  const tmp32 = _exh;
		  _exh = function(exn) {
		   _exh = tmp32;
		   if (exn instanceof _Overflow_tag) {
		    return [false, cont18, [undefined]];
		   } else {
		    throw exn;
		   }
		  };
		  let tmp33;
		  cont: {
		   _Int54_mul(8, tmp11);
		   tmp33 = undefined;
		   break cont;
		  }
		  _exh = tmp32;
		  return [false, cont18, [tmp33]];
		 };
		 return [false, tmp8, [cont17, _list([0xFF, 0xFE, 0xFC, 0xF8, 0xF0, 0xE0, 0xC0, 0x80, 0x0])]];
		};
		return [false, tmp8, [cont16, _list([0x0, 0x1, 0x3, 0x7, 0xF, 0x1F, 0x3F, 0x7F, 0xFF])]];
	       };
	       return [false, tmp9, [cont15, [256, function(cont16, i) {
		return [false, tmp12, [cont16, [tbl1, i]]];
	       }]]];
	      };
	      return [false, tmp27, [cont14, Uint8Array.of(65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 43, 47)]];
	     };
	     return [false, tmp7, [cont13, function(cont14, a) {
	      const i = a[0];
	      const c = a[1];
	      return [false, tmp13, [cont14, [tbl1, c, (i >>> 0 & 0xFF) >>> 0]]];
	     }]];
	    };
	    return [false, tmp10, [cont12, [256, 0xFF]]];
	   };
	   return [false, tmp9, [cont11, [256, function(cont12, i) {
	    return [false, tmp12, [cont12, [tbl, i]]];
	   }]]];
	  };
	  return [false, tmp24, [cont10, Uint8Array.of(65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 43, 47)]];
	 };
	 return [false, tmp7, [cont9, function(cont10, a) {
	  const i = a[0];
	  const c = a[1];
	  return [false, tmp13, [cont10, [tbl, c, (i >>> 0 & 0xFF) >>> 0]]];
	 }]];
	};
	return [false, tmp13, [cont8, [tbl, 32, 0x41]]];
       };
       return [false, tmp13, [cont7, [tbl, 13, 0x41]]];
      };
      return [false, tmp13, [cont6, [tbl, 10, 0x41]]];
     };
     return [false, tmp13, [cont5, [tbl, 9, 0x41]]];
    };
    return [false, tmp10, [cont4, [256, 0xFF]]];
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
