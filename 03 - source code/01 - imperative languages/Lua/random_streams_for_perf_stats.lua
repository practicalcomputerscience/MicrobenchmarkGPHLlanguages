-- random_streams_for_perf_stats.lua
--
-- 2025-07-13/14
--
-- run in Ubuntu 24 LTS: $ time lua random_streams_for_perf_stats.lua
--                       $ sudo perf stat -r 20 lua ./random_streams_for_perf_stats.lua
--                       
--                       $ time luajit random_streams_for_perf_stats.lua
--                       $ sudo perf stat -r 20 luajit ./random_streams_for_perf_stats.lua
--
-- $ lua -v
-- Lua 5.4.8  Copyright (C) 1994-2025 Lua.org, PUC-Rio
-- $ 
--
-- $ luajit -v
-- LuaJIT 2.1.1748459687 -- Copyright (C) 2005-2025 Mike Pall. https://luajit.org/
-- $


local math = require("math")
-- To optimize memory usage, prefer local variables over global ones.
-- Local variables are stored in the stack, which is faster and automatically cleaned up when no longer needed.
-- This practice minimizes the need for garbage collection.
-- https://dev.to/cristianalex_17/how-to-manage-memory-in-lua-in-2025-2a5b


------------------------------------------------------------------------
--
-- user defined functions

local STR_LENGTH_BIN = 16

local function Integer_to_bin_string(N)
    local bin_str = ""
    local j = STR_LENGTH_BIN
    local k = N
    while k > 0 and j >= 1 do
        if k % 2 == 0 then
            bin_str = '0'..bin_str
        else
            bin_str = '1'..bin_str
        end
        k = math.floor(k / 2)
        j = j - 1
    end
    bin_str = ('0'):rep(16-#bin_str)..bin_str
    return bin_str
end

-- end of user defined functions
--
------------------------------------------------------------------------


local END = 62500  -- 62500 for exactly 1M binary digits
-- local END = 10  -- for testing

local m = 65521  -- = 2^16 - 15
local a = 17364
local c = 0

local file_bits_x   = 'random_bitstring.bin'
local file_bits_hex = 'random_bitstring.byte'

local x = {}  -- also needed for the password later

math.randomseed(os.time())
x[1] = math.random(0, m)  -- exclusive of m

local bits_x   = {}  -- array of strings for the bit stream:  "0"'s + "1"'s
local bits_hex = {}  -- array of strings for the byte stream: [0..9][a..f]'s


print("\ngenerating a random bit stream...")

for i = 1, END do
  x[i+1] = (a * x[i] + c) % m
  -- print("\nx[i] = " .. x[i+1])  -- for testing

  local bits_x_str = Integer_to_bin_string(x[i+1])
  -- print("bits_x_str = " .. bits_x_str)  -- for testing
  bits_x[i] = bits_x_str

  local bits_hex_str = string.format("%04x", x[i+1])
  -- print("bits_hex_str = "..bits_hex_str)  -- for testing
  bits_hex[i] = bits_hex_str
end

local bits_x_str_total   = table.concat(bits_x, "")
local bits_hex_str_total = table.concat(bits_hex, "")
-- print("\nbits_x_str_total = "..bits_x_str_total)  -- for testing
-- print("\nbits_hex_str_total = "..bits_hex_str_total)  -- for testing

-- writing streams to files:
local file1 = io.open(file_bits_x, "w")
if file1 then
    file1:write(bits_x_str_total)
    file1:close()
    print("Bit stream has been written to disk under name:  "..file_bits_x)
else
    print("could not write to file: "..file_bits_x)
end

local file2 = io.open(file_bits_hex, "w")
if file2 then
    file2:write(bits_hex_str_total)
    file2:close()
    print("Byte stream has been written to disk under name: "..file_bits_hex)
else
    print("could not write to file: "..file_bits_hex)
end

-- end of random_streams_for_perf_stats.lua
