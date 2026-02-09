/*
random_streams_for_perf_stats.dart

2026-02-08

run on Ubuntu 24 LTS: $ dart run ./random_bitstring_and_flexible_password_generator.dart  # JIT compilation


$ dart --version
Dart SDK version: 3.10.8 (stable) (Tue Jan 27 00:02:04 2026 -0800) on "linux_x64"
$

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


  // Make a password of N_CHAR printable chars: user input requested here
  int N_CHAR = 12;
  bool answer = false;
  while (!answer) {
    stdout.write("\nPassword of $N_CHAR printable chars OK? 'y' or another integer number >= 8: ");
    String? answerStr = stdin.readLineSync();  // ? indicates that the variable can hold a value of type String or be null

    if (answerStr == 'y') {
      answer = true;
    } else {
      try {
        N_CHAR = int.parse(answerStr!);
        if (N_CHAR < 8) {
          print("enter an integer number >= 8 or 'y'");
        } else {
          answer = true;
        }
      } catch (e) {
        print("enter an integer number >= 8 or 'y'");
      }
    }
  }
  // print("\nN_CHAR = $N_CHAR");  // for testing


  bool WITH_SPECIAL_CHARS = true;
  answer = false;
  while (!answer) {
    stdout.write("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ");
    String? answerStr = stdin.readLineSync();

    if (answerStr == 'y') {
      answer = true;
    } else {
      WITH_SPECIAL_CHARS = false;
      answer = true;
    }
  }
  // print("\nWITH_SPECIAL_CHARS = $WITH_SPECIAL_CHARS");  // for testing


  // Work with a set of chars
  Set<String> char_set = {};
  if (WITH_SPECIAL_CHARS) {
    for (int i = 33; i <= 126; i++) { // ASCII printable characters
      char_set.add(String.fromCharCode(i));
    }
  } else {
    for (int i = 97; i <= 122; i++) { // 'a' to 'z'
      char_set.add(String.fromCharCode(i));
    }
    for (int i = 65; i <= 90; i++) {  // 'A' to 'Z'
      char_set.add(String.fromCharCode(i));
    }
    for (int i = 48; i <= 57; i++) {  // '0' to '9'
      char_set.add(String.fromCharCode(i));
    }
  }
  // print("\nchar_set = $char_set");  // for testing


  int i = 0;  // char counter for the password
  int j = 0;  // counter for x
  String pw_chars = "";

  while (i < N_CHAR) {
    String bin0 = x[j].toRadixString(2).padLeft(16, '0');

    String bin0_0 = bin0.substring(0, 8);
    String bin0_1 = bin0.substring(8, 16);

    String char0 = String.fromCharCode(int.parse(bin0_0, radix: 2));
    String char1 = String.fromCharCode(int.parse(bin0_1, radix: 2));

    if (char_set.contains(char0)) {
      pw_chars += char0;
      i++;
    }

    if (char_set.contains(char1) && i < N_CHAR) {
      pw_chars += char1;
      i++;
    }

    j++;
  }

  print("\nYour password of $N_CHAR characters is: $pw_chars");
}

// end of random_bitstring_and_flexible_password_generator.dart

