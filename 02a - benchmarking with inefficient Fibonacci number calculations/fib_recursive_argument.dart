/* fib_recursive_argument.dart

2026-02-10

test on Ubuntu 24 LTS with Dart SDK version: 3.10.8 (stable): OK


output:
  1/ JIT compilation:
     $ dart run ./fib_recursive_argument.dart <n>

     n = 44 => fib = 701408733           => Time:  3.85s
     n = 45 => fib = 1134903170          => Time:  6.18s
     n = 46 => fib = 1836311903          => Time: 10.03s
     n = 47 => fib = 2971215073          => Time: 16.15s


  2/ AOT compiled:
     $ dart compile aot-snapshot ./fib_recursive_argument.dart
     $ dartaotruntime ./fib_recursive_argument.aot <n>

     n = 44 => fib = 701408733           => Time:  3.05s
     ...
     n = 47 => fib = 2971215073          => Time: 12.89s <<<<<<<<<<<<<

*/


import 'dart:io';

int fib(int n) {
  return n <= 1 ? n : fib(n - 1) + fib(n - 2);
}

class FibArgument {
  static void main(List<String> args) {
    if (args.isEmpty) return;

    int n = int.parse(args[0]);
    print('argument n = $n');

    var startTime = DateTime.now();
    print(fib(n));
    var elapsedTime = DateTime.now().difference(startTime).inMilliseconds / 1000.0;

    print('Time: ${elapsedTime}s');
  }
}

void main(List<String> args) {
  FibArgument.main(args);  // args is an array of command-line arguments.
}

// end of fib_recursive_argument.dart
