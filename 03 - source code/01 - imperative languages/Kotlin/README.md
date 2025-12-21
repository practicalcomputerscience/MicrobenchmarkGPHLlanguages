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

But before starting to work with Kotlin, one has to have a JDK (Java Development Kit) environment installed. I installed the OpenJDK like [this](https://sdkman.io/jdks/open/): _$ sdk install java 27.ea.2-open_

There are many [JDK Distributions](https://sdkman.io/jdks/).

Now, one should have a running Kotlin environment:

```
$ java --version
openjdk 27-ea 2026-09-15
OpenJDK Runtime Environment (build 27-ea+2-79)
OpenJDK 64-Bit Server VM (build 27-ea+2-79, mixed mode, sharing)
$ $HOME/.sdkman/candidates/kotlin/current/bin/kotlinc -version
info: kotlinc-jvm 2.3.0 (JRE 27-ea+2-79)
$ 
```

Or like this directly, if SDKMAN! is active:

```
$ kotlinc -version
info: kotlinc-jvm 2.3.0 (JRE 27-ea+2-79)
$
```

See also at [On SDKMAN and Kotlin](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/20%20-%20language%20versions/README.md#on-sdkman-and-kotlin).

<br/>

##_end
