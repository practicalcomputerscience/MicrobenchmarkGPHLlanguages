# Lua

https://www.lua.org/

<br/>

Lua script [random_bitstring_and_flexible_password_generator.lua](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Lua/random_bitstring_and_flexible_password_generator.lua) may have a weakness at these string pattern matchings:

```
...
if string.match(char_set, "[%"..char0.."]") then  -- match also non-printable chars literally!
...
if string.match(char_set, "[%"..char1.."]") and string.len(pw_chars) < n_char then
...
```

..though I tested it now _statistically_ extensively without any problems.

---

### Installation tips

```
$ curl -L -R -O https://www.lua.org/ftp/lua-5.4.8.tar.gz
$ tar zxf lua-5.4.8.tar.gz
$ cd lua-5.4.8
$ make all test
$ sudo make install
$ lua -v
Lua 5.4.8 Copyright (C) 1994-2025 Lua.org, PUC-Rio
$
```

<br/>

### LuaJIT

An indicator for my suspicion as stated at the top ("weakness") is this: when I run this script with _LuaJIT_, a Just-In-Time compiler (https://luajit.org/):

```
$ luajit random_bitstring_and_flexible_password_generator.lua
```

..with a long password length of for example 99 characters and including special characters, then there's a high chance that it will fail:

```
...
luajit: random_bitstring_and_flexible_password_generator.lua:198: malformed pattern (missing ']')
stack traceback:
	[C]: in function 'match'
	random_bitstring_and_flexible_password_generator.lua:198: in main chunk
	[C]: at 0x5b8d7ee2d340
$
```

Here with version:

```
$ luajit -v
LuaJIT 2.1.1748459687 -- Copyright (C) 2005-2025 Mike Pall. https://luajit.org/
$
```

So, even with running the scripts about 30% faster, I won't list the LuaJIT results as my official ones, only the Lua results.

However, there's another, quasi official reason for ignoring LuaJIT here: 

> LuaJIT proved to be a success in several popular products, which helped increase the wide use of Lua. Unfortunately, LuaJIT chose to stay mostly on Lua 5.1. In particular, LuaJIT
refused to implement the new lexical scheme for globals described in §3.2, thus breaking the compatibility with future versions of Lua.

from:

The evolution of Lua, continued

Roberto Ierusalimschya, Luiz Henrique de Figueiredob, Waldemar Celesa, Department of Computer Science, PUC-Rio, Rio de Janeiro, Brazil, IMPA, Rio de Janeiro, Brazil, Preprint submitted to Journal of Computer Languages March 25, 2025: https://www.sciencedirect.com/science/article/abs/pii/S2590118425000127

<br/>

### Lua coding tips

I declared variables, functions, libraries etc. as _**local**_, that is for stack usage (https://en.wikipedia.org/wiki/Stack_(abstract_data_type)), throughout the scripts. It helps running a Lua script faster, and according to my experience by at least 15%:

> To optimize memory usage, prefer local variables over global ones. Local variables are stored in the stack, which is faster and automatically cleaned up when no longer needed. This practice minimizes the need for garbage collection.

from: https://dev.to/cristianalex_17/how-to-manage-memory-in-lua-in-2025-2a5b

<br/>

##_end
