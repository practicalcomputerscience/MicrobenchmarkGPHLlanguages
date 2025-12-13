--[[ random_bitstring_and_flexible_password_generator.lua

2025-07-14, 2025-07-23: fixing the string matching when creating the password; 2025-12-13: see below

run in Ubuntu 24 LTS:  $ lua random_bitstring_and_flexible_password_generator.lua


$ lua -v
Lua 5.4.8  Copyright (C) 1994-2025 Lua.org, PUC-Rio
$

]]


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


local function safeStringToInt(str)
  return pcall(function()
    local num = tonumber(str)
    if not num then
      error("enter an integer number >= 8 or 'y'")
    elseif num and math.floor(num) == num then
      return num
    else
      error("enter an integer number >= 8 or 'y'")
    end
  end)
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
x[1] = math.random(1, m)  -- exclusive of m; 2025-12-13: (0, m) --> (1, m)

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

-- write bit stream to disk:
local file1 = io.open(file_bits_x, "w")
if file1 then
    file1:write(bits_x_str_total)
    file1:close()
    print("Bit stream has been written to disk under name:  "..file_bits_x)
else
    print("could not write to file: "..file_bits_x)
end

-- write byte stream to disk:
local file2 = io.open(file_bits_hex, "w")
if file2 then
    file2:write(bits_hex_str_total)
    file2:close()
    print("Byte stream has been written to disk under name: "..file_bits_hex)
else
    print("could not write to file: "..file_bits_hex)
end


-- make a password of n_char printable chars: user input requested here
local n_char = 12
local answer = false
local answer_str = ""

while answer == false do
  io.write("\nPassword of "..n_char.." printable chars OK? 'y' or another integer number >= 8: ")
  answer_str = io.read("*l")  -- read a line

  if answer_str == "y" then
    answer = true
  else
    local success, n_char_ = safeStringToInt(answer_str)
    if success then
      if n_char_ < 8 then
        print("enter an integer number >= 8 or 'y'")
      else
        n_char = n_char_
        answer = true
      end
    else
      print("enter an integer number >= 8 or 'y'")
   end
  end
end
-- print("n_char = "..n_char)  -- for testing


local with_special_chars = true
answer = false

while answer == false do
  io.write("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ")
  answer_str = io.read("*l")  -- read a line

  if answer_str == "y" then
    answer = true
  else
    with_special_chars = false
    answer = true
  end
end
-- print("with_special_chars = "..tostring(with_special_chars))  -- for testing


local char_set = ""
if with_special_chars then
  for i = 33, 127 do  -- codepoints
    char_set = char_set .. string.char(i)
  end
else
  char_set = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
end
-- print ("char_set = "..char_set)  -- for testing


local i = 0
local j = 1
local pw_chars = ""
local bin0, bin0_0, bin0_1  = "", "", ""
local char0, char1 = "", ""

while i < n_char do
  bin0 = Integer_to_bin_string(x[j])
  -- print("\nbin0 = "..bin0)  -- for testing

  bin0_0 = string.sub(bin0, 1, 8)
  bin0_1 = string.sub(bin0, 9, 16)
  -- print("  bin0_0 = "..bin0_0.." -- bin0_1 = "..bin0_1)  -- for testing

  char0 = string.char(tonumber(bin0_0, 2))
  char1 = string.char(tonumber(bin0_1, 2))
  -- print("  char0 = "..char0.." -- char1 = "..char1)

  if string.match(char_set, "[%"..char0.."]") then  -- match also non-printable chars literally!
    pw_chars = pw_chars..char0
    i = i + 1
  end

  if string.match(char_set, "[%"..char1.."]") and string.len(pw_chars) < n_char then
    pw_chars = pw_chars..char1
    i = i + 1
  end

  j = j + 1
end

print ("\nYour password of "..n_char.." characters is: "..pw_chars)

-- end of random_bitstring_and_flexible_password_generator.lua

