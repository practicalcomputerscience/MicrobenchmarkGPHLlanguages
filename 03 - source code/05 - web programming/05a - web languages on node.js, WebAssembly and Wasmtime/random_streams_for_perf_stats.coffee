# random_streams_for_perf_stats.coffee
#
# 2026-02-15
#
# build on Ubuntu 24 LTS: $ coffee -c ./random_streams_for_perf_stats.coffee
#
# run on Ubuntu 24 LTS:   $ node ./random_streams_for_perf_stats.js
#                         $ time node ./random_streams_for_perf_stats.js                                     => real	0m0.039s
#                         $ multitime -n 20 node ./random_streams_for_perf_stats.js
#                         =>
#                                       Mean        Std.Dev.
#                           real        0.040       0.002
#
#                         $ time deno --allow-write --unstable-detect-cjs ./random_streams_for_perf_stats.js => real	0m0.052s
#                         $ time bun ./random_streams_for_perf_stats.js                                      => real	0m0.026s
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
# (almost) completely transpiled from random_streams_for_perf_stats.groovy with Duck.ai
#


fs = require 'fs'

class random_streams_for_perf_stats
  @main = (args) ->
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

# Call the main function
random_streams_for_perf_stats.main []


# end of random_streams_for_perf_stats.coffee
