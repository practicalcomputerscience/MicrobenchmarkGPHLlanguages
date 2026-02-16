# random_bitstring_and_flexible_password_generator.coffee
#
# 2026-02-15/16
#
# build on Ubuntu 24 LTS: $ npm install prompt-sync  # install, if still missing
#                         $ coffee -c ./random_bitstring_and_flexible_password_generator.coffee
#
# run on Ubuntu 24 LTS:   $ node ./random_bitstring_and_flexible_password_generator.js
#
#
# $ node -v
# v24.13.0
# $ npm ls coffeescript -g
# ...
# └── coffeescript@2.7.0
# ...
# $
#
#
# mostly transpiled from random_bitstring_and_flexible_password_generator.dart with Duck.ai
#

fs = require 'fs'
readline = require 'readline'

class random_bitstring_and_flexible_password_generator
  @main = (args) ->  # 2026-02-16: -> is for a function definition: function_name = -> function_body
                     # -> becomes in JavaScript: (function() {});
                     # see: https://www.tutorialspoint.com/coffeescript/coffeescript_functions.htm
    END = 62501  # 62501 for exactly 1M binary digits
    # END = 10  # for testing

    m = 65521  # = 2^16 - 15
    a = 17364
    c = 0

    fileBitsX   = 'random_bitstring.bin'
    fileBitsHex = 'random_bitstring.byte'

    x = new Array(END)

    x[0] = Math.floor(Math.random() * (m - 1)) + 1

    bitsX   = ''
    bitsHex = ''

    console.log '\ngenerating a random bit stream...'
    for i in [1...END]
      x[i] = (a * x[i - 1] + c) % m

      bitsXStr = x[i].toString(2).padStart(16, '0')
      bitsX   += bitsXStr

      bitsHexStr = x[i].toString(16).padStart(4, '0')
      bitsHex   += bitsHexStr


     # write bit stream to disk:
     try
       fs.writeFileSync fileBitsX, bitsX
       console.log "Bit stream has been written to disk under name:  #{fileBitsX}"
     catch ex
       console.log "could not write to file: #{fileBitsX}! -- #{ex.message}"

     # write byte stream to disk:
     try
       fs.writeFileSync fileBitsHex, bitsHex
       console.log "Byte stream has been written to disk under name: #{fileBitsHex}"
     catch ex
       console.log "could not write to file: #{fileBitsHex}! -- #{ex.message}"


    # Make a password of N_CHAR printable chars: user input requested here
    N_CHAR = 12
    answer = false

    rl = readline.createInterface
      input: process.stdin
      output: process.stdout

    ask = (q) ->
      new Promise (resolve) ->
        rl.question q, (ans) -> resolve ans

    while not answer
      answerStr = await ask "\nPassword of #{N_CHAR} printable chars OK? 'y' or another integer >= 8: "

      if answerStr is 'y'
        answer = true
      else
        numberValue = (Number) answerStr

        if Number.isInteger(numberValue) and numberValue >= 8
          N_CHAR = numberValue
          answer = true
        else
          console.log "enter an integer number >= 8 or 'y'"
    # console.log "N_CHAR = #{N_CHAR}"  # for testing


    # Ask about special characters
    WITH_SPECIAL_CHARS = true
    answer = false
    while not answer
      answerStr = await ask "\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': "

      if answerStr is 'y'
        answer = true
      else
        WITH_SPECIAL_CHARS = false
        answer = true
    # console.log "WITH_SPECIAL_CHARS = #{WITH_SPECIAL_CHARS}"  # for testing


    # Build character set
    char_set = new Set()
    if WITH_SPECIAL_CHARS
      for code in [33..126]
        char_set.add String.fromCharCode code
    else
      for code in [97..122] then char_set.add String.fromCharCode code   # a–z
      for code in [65..90]  then char_set.add String.fromCharCode code   # A–Z
      for code in [48..57]  then char_set.add String.fromCharCode code   # 0–9
    # console.log(Array.from(char_set))  # for testing


    # Generate password
    i = 0  # char counter for the password
    j = 0  # counter for x
    pw = ""

    while i < N_CHAR
      bin = x[j].toString(2).padStart(16, '0')

      bin0 = bin[0...8]
      bin1 = bin[8...16]

      char0 = String.fromCharCode parseInt(bin0, 2)
      char1 = String.fromCharCode parseInt(bin1, 2)

      if char_set.has char0
        pw += char0
        i++

      if i < N_CHAR and char_set.has char1
        pw += char1
        i++

      j++

    console.log "\nYour password of #{N_CHAR} characters is: #{pw}"

    rl.close()  # in the Coffeescript variant, this is the last function call, compared to TypeScript/JavaScript

# Call the main function
random_bitstring_and_flexible_password_generator.main []

# end of random_bitstring_and_flexible_password_generator.coffee
