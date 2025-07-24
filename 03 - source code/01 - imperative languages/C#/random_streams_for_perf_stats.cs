/*
random_streams_for_perf_stats.cs

2025-06-07

build on Ubuntu 24 LTS: $ dotnet new console -n random_streams_for_perf_stats --use-program-main
                        $ cd ./random_streams_for_perf_stats
                        development candidate:
                        $ dotnet build

run on Ubuntu 24 LTS:   $ ./bin/Debug/net8.0/random_streams_for_perf_stats

                        release candidate:
                    (A) $ dotnet publish -c Release -r linux-x64 --no-self-contained   <<<<<<<<<<<<<<

                    (B) $ dotnet publish -c Release -r linux-x64 --self-contained true -p:PublishSingleFile=true -p:PublishReadyToRun=true
                        self-contained == not requiring .NET Core runtime installed on host machine
                        => this makes a slower program: ~+20%


run on Ubuntu 24 LTS:   $ ./bin/Release/net8.0/linux-x64/random_streams_for_perf_stats
                        $ sudo perf stat -r 20 ./bin/Release/net8.0/linux-x64/random_streams_for_perf_stats


(Using AOT:             $ https://learn.microsoft.com/en-us/dotnet/core/deploying/native-aot/?tabs=linux-ubuntu%2Cnet8
                        --> add to: random_streams_for_perf_stats.csproj
                                    <PublishAot>true</PublishAot>

                    (A) $ dotnet publish -c Release -r linux-x64
                        $ ./bin/Release/net8.0/linux-x64/random_streams_for_perf_stats
                        $ sudo perf stat -r 20 ./bin/Release/net8.0/linux-x64/random_streams_for_perf_stats

                    (B) $ dotnet publish -c Release -r linux-x64 --self-contained true -p:PublishSingleFile=true -p:PublishReadyToRun=true
                        $ ./bin/Release/net8.0/linux-x64/random_streams_for_perf_stats
                        $ sudo perf stat -r 20 ./bin/Release/net8.0/linux-x64/random_streams_for_perf_stats
                        => this makes a slower program: ~+20%
)


(a) install the .NET runtime:
$ wget https://dot.net/v1/dotnet-install.sh -O dotnet-install.sh
$ ./dotnet-install.sh --version latest
https://learn.microsoft.com/en-us/dotnet/core/install/linux-scripted-manual#scripted-install

$ dotnet --list-runtimes
  Microsoft.AspNetCore.App 8.0.4 [/usr/lib/dotnet/shared/Microsoft.AspNetCore.App]
  Microsoft.NETCore.App 8.0.4 [/usr/lib/dotnet/shared/Microsoft.NETCore.App]

(b) install the .NET SDK:
$ sudo apt-get update && sudo apt-get install -y dotnet-sdk-8.0
$ dotnet --list-sdks
8.0.104 [/usr/lib/dotnet/sdk]


*/

namespace random_streams_for_perf_stats;

using System.Text;  // StringBuilder
using System.IO;    // File

class Program
{
  static void Main(string[] args)
  {
    const int END = 62501;  // 62501 for exactly 1M binary digits
    // const int END  = 15;  // for testing
    const int M1   = END * 16;
    const int K250 = END * 4;

    const int m = 65521;  // = 2^16 - 15
    const int a = 17364;
    const int c = 0;

    const string file_bits_x   = "random_bitstring.bin";
    const string file_bits_hex = "random_bitstring.byte";


    int[] x = new int[END];  // arrays cannot be resized dynamically

    Random rand = new Random();
    x[0] = rand.Next(1, m);
    // Console.WriteLine("x[0] = " + x[0]);  // for testing

    StringBuilder bits_x   = new StringBuilder(M1);
    StringBuilder bits_hex = new StringBuilder(K250);


    Console.WriteLine("\ngenerating a random bit stream...");
    for (int i = 1; i < END; i = i + 1) {
      x[i] = (a * x[i-1] + c) % m;
      // Console.WriteLine("\n" + x[i]);  // for testing

      string bits_x_str = String.Format("{0,16:B16}", x[i]);
      // Console.WriteLine(bits_x_str);  // for testing
      bits_x.Append(bits_x_str);

      string bits_hex_str = String.Format("{0,4:x4}", x[i]);
      // Console.WriteLine(bits_hex_str);  // for testing
      bits_hex.Append(bits_hex_str);
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

  }
}

// end of random_streams_for_perf_stats.c3.cs
