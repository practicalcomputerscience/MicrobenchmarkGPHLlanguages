# Kotlin

https://kotlinlang.org/

<br/>

Kotlin is the better Scala in my opinion.

If Scala from Switzerland (https://scala.epfl.ch/) doesn't ramp up its efforts, it could end up as an academic language in my opinion. Kotlin is also ready for enterprise level software development, think of Google Maps and Google Drive and their association with Kotlin:

- https://github.com/googlemaps-samples/codelab-maps-platform-101-android-kotlin/tree/main
- https://medium.com/@sergei.rybalkin/upload-file-to-google-drive-with-kotlin-931cec5252c1

<br/>

Kotlin also shines with a high quality language documentation in my opinion: https://kotlinlang.org/docs/home.html

<br/>

## Installation tips

First, I installed [SDKMAN!](https://sdkman.io/) (in Ubuntu 24 LTS): _$ curl -s "https://get.sdkman.io" | bash_

..and with it [Kotlin](https://sdkman.io/sdks/kotlin/): _$ sdk install kotlin_

But before starting to work with Kotlin, one has to have a JDK (Java Development Kit) environment installed.

There are many [JDK Distributions](https://sdkman.io/jdks/).

Now, one should have a running Kotlin environment (here, after the [GraalVM installation](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/04%20-%20GraalVM#sdkman-and-java-versions) and SDKMAN! being active):

```
$ java --version
java 24 2025-03-18
Java(TM) SE Runtime Environment Oracle GraalVM 24+36.1 (build 24+36-jvmci-b01)
Java HotSpot(TM) 64-Bit Server VM Oracle GraalVM 24+36.1 (build 24+36-jvmci-b01, mixed mode, sharing)
$ $HOME/.sdkman/candidates/kotlin/current/bin/kotlinc -version
info: kotlinc-jvm 2.3.0 (JRE 24+36-jvmci-b01)
$ 
```

See also at [On SDKMAN and Kotlin](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/20%20-%20language%20versions/README.md#on-sdkman-and-kotlin).

<br/>

## Does the JDK (Java Development Kit) version matter at Kotlin?

As long as it's somehow "recent", I guess not; at least not with versions from 2025. Program sizes are almost identical:

```
$ ls -al random_streams*.jar
-rw-rw-r-- 1 ... 5280546 Dec 25 00:10 random_streams_for_perf_stats_with_Java_SE_Oracle_GraalVM_24+36.1.jar
-rw-rw-r-- 1 ... 5280546 Dec 25 00:24 random_streams_for_perf_stats_with_OpenJDK_25.0.1+8-Ubuntu-124.04.jar
-rw-rw-r-- 1 ... 5280544 Dec 21 10:43 random_streams_for_perf_stats_with_openjdk_27-ea_2026-09-15.jar
$ 
```

...and execution speeds also:

```
$ time java -jar ./random_streams_for_perf_stats_with_Java_SE_Oracle_GraalVM_24+36.1.jar  # this came along with the installation of GraalVM, which has been installed with SDKMAN!

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.072s
user	0m0.163s
sys	0m0.024s
$ time java -jar ./random_streams_for_perf_stats_with_OpenJDK_25.0.1+8-Ubuntu-124.04.jar  # this came with what?

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.072s
user	0m0.163s
sys	0m0.029s
$ time java -jar ./random_streams_for_perf_stats_with_openjdk_27-ea_2026-09-15.jar  # this has been installed with SDKMAN!

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.073s
user	0m0.176s
sys	0m0.023s
$
```

However, I didn't make these tests for Scala and Clojure.

##_end
