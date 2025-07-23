https://www.lua.org/

---

#### On LuaJIT

I also tested the scripts with _LuaJIT_, a Just-In-Time compiler: https://luajit.org/

```
$ luajit -v
LuaJIT 2.1.1748459687 -- Copyright (C) 2005-2025 Mike Pall. https://luajit.org/
$
```

However, even with running the scripts about 30% faster - and both programs still working correctly, I won't list the LuaJIT results as my official ones, only the Lua results. The reason is simply this: 

> LuaJIT proved to be a success in several popular products, which helped increase the wide use of Lua. Unfortunately, LuaJIT chose to stay mostly on Lua 5.1. In particular, LuaJIT
refused to implement the new lexical scheme for globals described in §3.2, thus breaking the compatibility with future versions of Lua.

from:

The evolution of Lua, continued

Roberto Ierusalimschya, Luiz Henrique de Figueiredob, Waldemar Celesa, Department of Computer Science, PUC-Rio, Rio de Janeiro, Brazil, IMPA, Rio de Janeiro, Brazil, Preprint submitted to Journal of Computer Languages March 25, 2025: https://www.sciencedirect.com/science/article/abs/pii/S2590118425000127

<br/>

#### Lua coding tips

I declared variables, functions, libraries etc. as _**local**_, that is for stack usage (https://en.wikipedia.org/wiki/Stack_(abstract_data_type)), throughout the scripts. It helps running a Lua script faster, and according to my experience by at least 15%:

> To optimize memory usage, prefer local variables over global ones. Local variables are stored in the stack, which is faster and automatically cleaned up when no longer needed. This practice minimizes the need for garbage collection.

from: https://dev.to/cristianalex_17/how-to-manage-memory-in-lua-in-2025-2a5b

<br/>

##_end
