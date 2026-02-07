/*
random_streams_for_perf_stats_js.dart -- this is the version for compiling to JavaScript on node.js

2026-02-06

build on Ubuntu 24 LTS: <get config file pubspec.yaml ready in the working dir>
                        $ dart pub get  # download package node_interop
                        
                        $ dart compile js ./random_streams_for_perf_stats_js.dart -o ./random_streams_for_perf_stats_js.js

                        # add the preamble because:
                        #   standalone engines like Node.js lack browser-specific globals (like self or window)
                        #   that Dart's generated code expects.
                        # download preamble.js from:
                        #   https://github.com/mbullington/node_preamble.dart/blob/master/lib/preamble.js
                        $ cat preamble.js random_streams_for_perf_stats_js.js > random_streams_for_perf_stats_js.node.js


run on Ubuntu 24 LTS:   $ node ./random_streams_for_perf_stats_js.node.js
                        $ time node ./random_streams_for_perf_stats_js.node.js => real	0m0.045s <<<<<<<<<<<<<<<<<<<<<<<<<
                        $ multitime -n 20 node ./random_streams_for_perf_stats_js.node.js
                                      Mean        Std.Dev. 
                          real        0.045       0.001  
                        
                        $ dart compile js -O2 ./random_streams_for_perf_stats_js.dart -o ./random_streams_for_perf_stats_js_o2.js
                        $ cat preamble.js random_streams_for_perf_stats_js_o2.js > random_streams_for_perf_stats_js_o2.node.js
                        $ time node ./random_streams_for_perf_stats_js_o2.node.js => real	0m0.045s
                        
                        $ dart compile js -O3 ./random_streams_for_perf_stats_js.dart -o ./random_streams_for_perf_stats_js_o3.js
                        $ cat preamble.js random_streams_for_perf_stats_js_o3.js > random_streams_for_perf_stats_js_o3.node.js
                        $ time node ./random_streams_for_perf_stats_js_o3.node.js => real	0m0.043s

                        $ dart compile js -O4 ./random_streams_for_perf_stats_js.dart -o ./random_streams_for_perf_stats_js_o4.js
                        $ cat preamble.js random_streams_for_perf_stats_js_o4.js > random_streams_for_perf_stats_js_o4.node.js
                        $ time node ./random_streams_for_perf_stats_js_o4.node.js => real	0m0.045s

                        $ dart compile js -O0 ./random_streams_for_perf_stats_js.dart -o ./random_streams_for_perf_stats_js_o0.js
                        $ cat preamble.js random_streams_for_perf_stats_js_o0.js > random_streams_for_perf_stats_js_o0.node.js
                        $ time node ./random_streams_for_perf_stats_js_o0.node.js => real	0m0.053s <<<<<<<<<<<<<<<<<<<<<<<<<


$ dart --version
Dart SDK version: 3.10.8 (stable) (Tue Jan 27 00:02:04 2026 -0800) on "linux_x64"
$

*/


// import 'dart:io';
import 'package:node_interop/fs.dart';  // Direct fs bindings
import 'dart:math';


void main(List<String> args) {
  const int END = 62501;  // 62501 for exactly 1M binary digits

  const int m = 65521;  // = 2^16 - 15
  const int a = 17364;
  const int c = 0;

  String fileBitsX   = "random_bitstring.bin";
  String fileBitsHex = "random_bitstring.byte";

  List<int> x = List<int>.filled(END, 0);

  Random rnd = Random();
  x[0] = rnd.nextInt(m - 1) + 1;

  StringBuffer bitsX   = StringBuffer();
  StringBuffer bitsHex = StringBuffer();

  print("\ngenerating a random bit stream...");
  for (int i = 1; i < END; i++) {
    x[i] = (a * x[i - 1] + c) % m;

    String bitsXStr = x[i].toRadixString(2).padLeft(16, '0');
    bitsX.write(bitsXStr);

    String bitsHexStr = x[i].toRadixString(16).padLeft(4, '0');
    bitsHex.write(bitsHexStr);
  }


  // Write bit stream to disk:
  try {
    // Accessing node's fs module
    fs.writeFileSync(fileBitsX, bitsX.toString());
    print("Bit stream has been written to disk under name:  $fileBitsX");
  } catch (e) {
    print("could not write to file: $fileBitsX ! -- ${e.toString()}");
  }

  // Write byte stream to disk:
  try {
    fs.writeFileSync(fileBitsHex, bitsHex.toString());
    print("Byte stream has been written to disk under name: $fileBitsHex");
  } catch (e) {
    print("could not write to file: $fileBitsHex ! -- ${e.toString()}");
  }
}

// end of random_streams_for_perf_stats_js.dart
