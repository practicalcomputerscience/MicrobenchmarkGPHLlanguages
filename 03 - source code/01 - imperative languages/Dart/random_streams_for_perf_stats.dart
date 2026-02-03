/*
random_streams_for_perf_stats.dart

2026-02-02

run on Ubuntu 24 LTS: $ dart ./random_streams_for_perf_stats.dart
                      $ time dart ./random_streams_for_perf_stats.dart => real	0m0.151s
                      $ multitime -n 20 dart ./random_streams_for_perf_stats.dart
                      =>
                                    Mean        Std.Dev.
                        real        0.146       0.002    ==> so, this is the interpreted version, by the Dart VM

       JIT snapshot:  $ dart compile jit-snapshot random_streams_for_perf_stats.dart
                      $ dart run ./random_streams_for_perf_stats.jit
                      $ time dart run ./random_streams_for_perf_stats.jit => real	0m0.107s
                      
       AOT snapshot:  $ dart compile aot-snapshot ./random_streams_for_perf_stats.dart
                      $ dartaotruntime ./random_streams_for_perf_stats.aot
                      $ time dartaotruntime ./random_streams_for_perf_stats.aot => real	0m0.014s
                       

$ dart --version
Dart SDK version: 3.10.8 (stable) (Tue Jan 27 00:02:04 2026 -0800) on "linux_x64"
$

transpiled from random_streams_for_perf_stats.groovy with Duck.ai

*/


import 'dart:io';
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
    File(fileBitsX).writeAsStringSync(bitsX.toString());
    print("Bit stream has been written to disk under name:  $fileBitsX");
  } catch (e) {
    print("could not write to file: $fileBitsX ! -- ${e.toString()}");
  }

  // Write byte stream to disk:
  try {
    File(fileBitsHex).writeAsStringSync(bitsHex.toString());
    print("Byte stream has been written to disk under name: $fileBitsHex");
  } catch (e) {
    print("could not write to file: $fileBitsHex ! -- ${e.toString()}");
  }
}

// end of random_streams_for_perf_stats.dart
