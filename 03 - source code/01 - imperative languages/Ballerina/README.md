2026-05-07: work in progress

to-do:

- test: $ bal graph ... to print the dependency graph in the console --> https://ballerina.io/learn/cli-commands/
- GraalVM for random_streams_for_perf_stats.bal: https://ballerina.io/learn/graalvm-executable-overview/
  - $ bal build --graalvm

<br/> 

# Ballerina aka jBallerina

https://ballerina.io/ (*)

https://github.com/ballerina-platform/ballerina-lang

https://github.com/ballerina-guides

https://en.wikipedia.org/wiki/Ballerina_(programming_language)

- JDK = Java Development Kit
- JRE = Java Runtime Environment
- JVM = Java Virtual Machine

---

<br/>

## Idea of Ballerina: simpler enterprise integration

Ballerina, "developed by WSO2 since 2016 and first released in February 2022" (*) makes a good job - at first - to hide the fact that it's another general purpose, high-level programming language to be natively executed on the Java Virtual Machine.

I only noticed it at chapter [Run the package](https://ballerina.io/learn/get-started/#run-the-package) on page [Get started](https://ballerina.io/learn/get-started/), where "generate an executable file" with the _$ bal build_ shell command is explained. It's very easy to check this out:

```
$ bal new hello_world
...
$ cd hello_world
$ bal build
...
$ java -jar ./target/bin/hello_world.jar
WARNING: Incompatible JRE version '25.0.2' found. This ballerina program supports running on JRE version '21.0.*'
Hello, World!
$ 
```
> Although Ballerina is not designed to be a JVM language, the current implementation, which targets the JVM, aka jBallerina, provides Java interoperability by adhering to the Ballerina language semantics.

from: [Call Java code from Ballerina](https://ballerina.io/learn/call-java-code-from-ballerina/)

Though, initially this was not the plan, but implementing a virtual machine of their own:

> Early in its development, the Ballerina team attempted to implement their own virtual machine, but experienced performance bottlenecks. Known as the Ballerina Virtual Machine (BVM), this VM executed Ballerina programs by interpreting BVM bytecode emitted by the Ballerina compiler. However, the Ballerina team ultimately decided that the BVM, despite having been implemented in Java, was not ready for production use, and decided in favor of including a compiler that targets the JVM with the release of version 1.0.

From: [Ballerina - An Open Source JVM Language and Platform for Cloud-Era Application Programmers](https://www.infoq.com/news/2020/01/wso2-releases-ballerina-1-1/) from Jan 29, 2020

However, the bigger idea of Ballerina is this:

> The high-level goal is to create a programming language and a platform co-designed together to make enterprise integration simpler, more agile and DevOps friendly by including cloud-native and middleware abstractions into a programming language in addition to the expected general purpose functionality.

..from the same source.

<br/>

## Installation and compilation tips

I downloaded Debian package _ballerina-2201.13.3-swan-lake-linux-x64.deb_ from here: https://ballerina.io/downloads/ and installed it like this on my target system (![On configuring building and execution environments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main#on-configuring-building-and-execution-environments)):

```
$ sudo dpkg -i ballerina-2201.13.3-swan-lake-linux-x64.deb
...
$ bal --version
Ballerina 2201.13.3 (Swan Lake Update 13)
Language specification 2024R1
Update Tool 1.5.1
$ 
```

<br/>

See other tips at the header comment block of program [random_streams_for_perf_stats.bal](./random_streams_for_perf_stats.bal).

<br/>

For a smooth program run without a JRE version warning of command:

```
$ java -jar ./target/bin/random_streams_for_perf_stats.jar
```

..I had to downgrade the active JRE version to version 21 with command:

```
$ sudo update-alternatives --config java
There are 3 choices for the alternative java (providing /usr/bin/java).

  Selection    Path                                            Priority   Status
------------------------------------------------------------
  0            /usr/lib/jvm/java-25-openjdk-amd64/bin/java      2511      auto mode
* 1            /usr/lib/jvm/java-21-openjdk-amd64/bin/java      2111      manual mode
  2            /usr/lib/jvm/java-25-openjdk-amd64/bin/java      2511      manual mode
  3            /usr/lib/jvm/java-8-openjdk-amd64/jre/bin/java   1081      manual mode

Press <enter> to keep the current choice[*], or type selection number: 
...
$ java --version
openjdk 21.0.10 2026-01-20
OpenJDK Runtime Environment (build 21.0.10+7-Ubuntu-124.04)
OpenJDK 64-Bit Server VM (build 21.0.10+7-Ubuntu-124.04, mixed mode, sharing)
$ 
```

If not, program execution also succeeds but emits this warning:

```
WARNING: Incompatible JRE version '25.0.2' found. This ballerina program supports running on JRE version '21.0.*'
```

On the other side, see at: [Does the JDK (Java Development Kit) version matter at Kotlin?](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Kotlin#does-the-jdk-java-development-kit-version-matter-at-kotlin)

<br/>

## Static code analysis

Ballerina features a quick to use tool, among tools, for [static code analysis](Scan tool):

```
$ bal tool pull scan  # install the tool
$ bal tool list  # see installed tools and their versions:
|TOOL ID               |VERSION                        |
|----------------------|-------------------------------|
|scan                  |0.11.0                         |
|asyncapi              |0.12.0                         |
|persist               |1.9.2                          |
|graphql               |0.14.0                         |
|openapi               |2.4.1                          |
|grpc                  |1.0.0                          |

6 active tool versions found.
$ bal scan --scan-report  # makes a HTML based report of all *.bal soource code files in current working directory

Running Scans
...
$
```

A report is located in directory ./target/report/index.html, and my example, at first, looked like this:

![Alt text](./Ballerina%20scan%20report%201a.png)

The report marks the lines of source code of concern:

...

![Alt text](./Ballerina%20scan%20report%201b.png)

...






<br/>

##_end
