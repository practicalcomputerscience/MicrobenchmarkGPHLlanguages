/*
random_bitstring_and_flexible_password_generator.cs

2025-06-07/08; 2025-12-17: see below

build on Ubuntu 24 LTS: $ dotnet new console -n random_bitstring_and_flexible_password_generator --use-program-main
                        $ cd ./random_bitstring_and_flexible_password_generator
                        development candidate:
                        $ dotnet build

run on Ubuntu 24 LTS:   $ ./bin/Debug/net8.0/random_bitstring_and_flexible_password_generator

                        release candidate:
                        $ dotnet publish -c Release -r linux-x64 --no-self-contained
                        (self-contained == not requiring .NET Core runtime installed on host machine)

run on Ubuntu 24 LTS:   $ ./bin/Release/net8.0/linux-x64/random_bitstring_and_flexible_password_generator


Install .NET:
$ sudo apt install dotnet8

$ dotnet --list-runtimes
  Microsoft.AspNetCore.App 8.0.22 [/usr/lib/dotnet/shared/Microsoft.AspNetCore.App]
  Microsoft.NETCore.App 8.0.22 [/usr/lib/dotnet/shared/Microsoft.NETCore.App]
$ dotnet --list-sdks
8.0.122 [/usr/lib/dotnet/sdk]
$

*/

namespace random_bitstring_and_flexible_password_generator;

using System.Text;  // StringBuilder
using System.IO;    // File
using System.Text.RegularExpressions;


class Program
{
  static void Main(string[] args)
  {
    const int END = 62501;  // 62501 for exactly 1M binary digits
    // const int END  = 58;  // for testing
    const int M1   = END * 16;
    const int K250 = END * 4;

    const int m = 65521;  // = 2^16 - 15
    const int a = 17364;
    const int c = 0;

    const string file_bits_x   = "random_bitstring.bin";
    const string file_bits_hex = "random_bitstring.byte";


    int[] x = new int[END];  // arrays cannot be resized dynamically

    Random rand = new Random();
    x[0] = rand.Next(1, m - 1);  // 2025-12-17
    // Console.WriteLine("x[0] = " + x[0]);  // for testing

    StringBuilder bits_x   = new StringBuilder(M1);
    StringBuilder bits_hex = new StringBuilder(K250);


    Console.WriteLine("\ngenerating a random bit stream...");
    int i = 1;  // i is re-used below at creating of pw_chars
    while (i < END) {
      x[i] = (a * x[i-1] + c) % m;
      // Console.WriteLine("\n" + x[i]);  // for testing

      string bits_x_str = String.Format("{0,16:B16}", x[i]);
      // Console.WriteLine(bits_x_str);  // for testing
      bits_x.Append(bits_x_str);

      string bits_hex_str = String.Format("{0,4:x4}", x[i]);
      // Console.WriteLine(bits_hex_str);  // for testing
      bits_hex.Append(bits_hex_str);

      i += 1;
    }

    // write bit stream to disk:
    try {
      File.WriteAllText(file_bits_x, bits_x.ToString());
      Console.WriteLine("Bit stream has been written to disk under name:  " + file_bits_x);
    } catch (Exception ex) {
      Console.WriteLine($"could not write to file: {file_bits_x} ! -- {ex.Message}");
    }

    // write byte stream to disk:
    try {
      File.WriteAllText(file_bits_hex, bits_hex.ToString());
      Console.WriteLine("Byte stream has been written to disk under name: " + file_bits_hex);
    } catch (Exception ex) {
      Console.WriteLine($"could not write to file: {file_bits_hex} ! -- {ex.Message}");
    }



    // make a password of N_CHAR printable chars:
    int N_CHAR = 12;
    bool answer = false;
    string answer_str;
    while (!answer) {
      N_CHAR = 12;
      Console.Write("\nPassword of {0} printable chars OK? 'y' or another integer number >= 8: ", N_CHAR);
      answer_str = Console.ReadLine()!;  // ! --> Converting null literal or possible null value to non-nullable type.

      if (answer_str == "y") {
        answer = true;
      } else {
        try {
          N_CHAR = Int32.Parse(answer_str);
          if (N_CHAR >= 8) {
            answer = true;
          } else {
            Console.WriteLine("enter an integer number >= 8 or 'y'");
          }
        } catch {
          Console.WriteLine("enter an integer number >= 8 or 'y'");
        }
      }
    }
    // Console.WriteLine(N_CHAR); // for testing

    bool WITH_SPECIAL_CHARS = true;
    answer = false;
    while (!answer) {
      Console.Write("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ");
      answer_str = Console.ReadLine()!;
       if (answer_str == "y") {
        answer = true;
      } else {
        WITH_SPECIAL_CHARS = false;
        answer = true;
      }
    }

    string char_set = "";
    // https://learn.microsoft.com/de-de/dotnet/standard/base-types/character-classes-in-regular-expressions
    if (WITH_SPECIAL_CHARS) {
      char_set += "[!-~]";  // @"..." = take it literally
    } else {
      char_set += "[a-zA-Z0-9]";
    }
    // Console.WriteLine(char_set); // for testing


    i = 0;  // char counter for the password
    int j = 0;  // char counter for x
    string pw_chars = "";

    while (i < N_CHAR) {
      string bin0 = String.Format("{0,16:B16}", x[j]);
      // Console.WriteLine("\n{0}", bin0);  // for testing

      string bin0_0 = bin0.Substring(0,8);
      string bin0_1 = bin0.Substring(8,8);
      // Console.WriteLine("{0} -- {1}", bin0_0, bin0_1);  // for testing

      int char0a = Int32.Parse(bin0_0, System.Globalization.NumberStyles.BinaryNumber );
      int char0b = Int32.Parse(bin0_1, System.Globalization.NumberStyles.BinaryNumber );
      // Console.WriteLine("{0} -- {1}", char0a, char0b);  // for testing

      string char1a = Convert.ToString(Convert.ToChar(char0a));
      string char1b = Convert.ToString(Convert.ToChar(char0b));
      // Console.WriteLine("{0} -- {1}", char1a, char1b);  // for testing

      Match match1 = Regex.Match(char1a, char_set);
      Match match2 = Regex.Match(char1b, char_set);
      // https://learn.microsoft.com/de-de/dotnet/api/system.text.regularexpressions.match?view=net-9.0

      if (match1.Success) {
        pw_chars += char1a;
        i += 1;
      }

      if (match2.Success && i < N_CHAR) {
        pw_chars += char1b;
        i += 1;
      }

      j += 1;
    }

    Console.WriteLine("\nYour password of {0} characters is: {1}", N_CHAR, pw_chars);
  }
}

// end of random_bitstring_and_flexible_password_generator.cs
