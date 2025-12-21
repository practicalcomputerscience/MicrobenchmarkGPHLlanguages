# Common Lisp

https://lisp-lang.org/

- **SBCL = Steel Bank Common Lisp** (https://www.sbcl.org/)
- CCL = Clozure Common Lisp (Clozure CL: https://ccl.clozure.com/)
- ECL = Embeddable Common Lisp (https://ecl.common-lisp.dev/)
- ABCL = Armed Bear Common Lisp (https://abcl.org/)

<br/>

Overview of modern Lisp implementations: https://lisp-lang.org/wiki/article/implementations

<br/>

Table of contents:

- [A little exercise in Common Lisp](#a-little-exercise-in-common-lisp)
- [Installing Steel Bank Common Lisp](#installing-steel-bank-common-lisp)
- [Execution speed of a Lisp program as a standalone executable in Linux: Steel Bank Common Lisp](#execution-speed-of-a-lisp-program-as-a-standalone-executable-in-linux-steel-bank-common-lisp)
- [Common Lisp on the Java Virtual Machine (JVM) with Armed Bear Common Lisp (ABCL)](#common-lisp-on-the-java-virtual-machine-jvm-with-armed-bear-common-lisp-abcl)

---

## A little exercise in Common Lisp

I found this little exercise in Common Lisp on the _sbcl_ REPL (Read-Eval-Print Loop). Start in your Linux shell with _$ rlwrap sbcl_ to get some comfortable expression history with keyboard controls:

```
(let ((x 3))
(print x)
(setf x 9)
(print x))
```

I gives this (expected) output:

```
3
9
9
```

Now let's test this expression:

```
(let ((x 3))
(print x)
(setf x 9)
(print x)
(print "hello"))
```

I gives this output:

```
3
9
"hello"
"hello"
```

As seen in PDF file "CLISP Tutorial-Basic", page 17 from 26: https://silp.iiita.ac.in/courses/archive/ppl/

And no, _let*_ instead of _let_ doesn't change the order of output (in my system).

**So, why is this (in my opionion)?**

While it's said that _let_ doesn't guarantee you a certain order of execution (because: _let performs the bindings in parallel and let* does them sequentially_ from: https://lisp-docs.github.io/cl-language-reference/chap-5/f-d-dictionary/let_let_special-operator) it's pretty clear (to me), that the old "left-to-right evaluation" of Lisp prevails more often than one might think.

See for example standard ISO/IEC 13816 (https://www.iso.org/standard/44338.html) and it's numerous statements like this for example:

> All of the arguments are evaluated, from left to right, ...

The left-to-right rule of Lisp is important to quickly understand this expression from the next example:

```
(dotimes (x 3 "you") (print "hello"))
```

with output:

```
"hello"
"hello"
"hello"
"you" ; this is the return value
```

What I want demonstrate with these examples is this:

> [!NOTE]
> Functional Programming is (obviously) not taking over the world.

which refers to: _If Haskell is so great, why hasn't it taken over the world? ..._ from 2017: https://pchiusano.github.io/2017-01-20/why-not-haskell.html

<br/>

## Installing Steel Bank Common Lisp

Since installing lastest SBCL version from sources needs a working Common Lisp installation (though not necessarily SBCL), I did this to install it from scratch:

- first, I installed an older version of SBCL with Ubuntu's package manager: _$ sudo apt install sbcl_
- then I downloaded and extracted latest SBCL sources as described here: https://sbcl.org/platform-table.html; I got tarball file: _sbcl-2.5.11-source.tar.bz2_
- now, change into the extracted directory, that is: _.../sbcl-2.5.11-source/sbcl-2.5.11_
- there, I ran: _$ sh ./make.sh_
- I followed the given tips at the end of this compilation process, like:
- _$ cd ./tests && sh ./run-tests.sh_  # this test will take a really long time: ..._(28 tests skipped for this combination of platform and features); test failed, expected 104 return code, got 1_...
- _$ cd .._
- _$ sudo apt-get install texinfo_  # needed to install the documentation
- _$ sudo apt-get install texlive-base_  # needed to install the documentation
- _$ cd ./doc/manual && make_  # install the documentation
- _$ cd ../.._
- then, I started the installation process: _$ sudo sh install.sh_ => _...SBCL has been installed: binary /usr/local/bin/sbcl; core and contribs in /usr/local/lib/sbcl/..._
- finally, I did this simple test: _$ sbcl --version_ => _SBCL 2.2.9.debian_, which means that I have to restart my Bash shell
- now, try again: _$ sbcl --version_ => _SBCL 2.5.11_

<br/>

## Execution speed of a Lisp program as a standalone executable in Linux: Steel Bank Common Lisp

After my [Scheme](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme#scheme) experiments, I revisited my Common Lisp and Clojure programs to see if I can make them faster from what I've learned from four Scheme dialects.

I came to the conclusion that I haven't learned anything for Lisp or Clojure in this regard (these languages are all too different in the end), but it motivates me to say a little bit more about Common Lisp.

I tried these Common Lisp environments:

- **Clozure Common Lisp** (Clozure CL or CCL): https://ccl.clozure.com/
- **SBCL**: https://www.sbcl.org/
- **Embeddable Common Lisp (ECL)**: https://ecl.common-lisp.dev/static/manual/What-is-ECL.html

I will shortly explain how to make standalone, executable Lisp based programs in Linux (on the Bash shell), because for historical reasons this isn't so easy as with other programming languages.

### Making standalone executables

#### SBCL

```
$ sbcl --load <program name>.lisp --eval "(sb-ext:save-lisp-and-die \"<program name>\" :executable t :toplevel #'main)"
```

I can't say how to apply potential compiler swiches for (speed) optimization. However, SBCL builds the fastest executables of this group according to my experience - by far!

(Globally) putting expressions like this for example:

```
(declaim (optimize (debug 0)
  (safety 0) (speed 3) (space 0)))
```

..into the source code file, speed parameter value 3 for the highest priority, doesn't change the execution speed of my little benchmark program. This observation is true for all three environments, including ECL, where such an expression can be applied in the ECL REPL ((Read-Eval-Print Loop) before compiling source code file(s); see below at [ECL](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Common%20Lisp#ecl).

#### CCL

Here I took this app builder: https://www.xach.com/lisp/buildapp/ and ran:

```
$ buildapp --load <program name>.lisp --entry main --output <program name>
```

#### ECL

I learned building executables from this script: https://github.com/csvitlik/build-with-ecl

```
$ rlwrap ecl
ECL (Embeddable Common-Lisp) 24.5.10 (git:UNKNOWN)
...
> (compile-file "<program name>.lisp" :system-p t)
> (c:build-program "<program name>" :lisp-files '("<program name>.o"))
> (quit)
$ ./<program name> # run your program
...
```

### Environment specific source code for Hello, World!

Since these are three different Common Lisp environments, there are differences in the related source code -- which doesn't mean that things couldn't be done differently than shown here:

#### SBCL

```
(defun main ()
  (princ "Hello, world from SBCL!")
  (terpri))
```

#### CCL

```
(defun main (argv)
  (declare (ignore argv))
  (princ "Hello, world from CCL!")
  (terpri))
```

#### ECL

```
(defun main ()
  (princ "Hello, world from ECL!")
  (terpri))
(main)
(ext:quit 0)
```

### Microbenchmark program

With my microbenchmark program I got these mean execution times, again time stopped  with _$ sudo perf stat -r 20 ./< program name >_:

- SBCL: 64 milliseconds  (still with version _SBCL 2.2.9.debian_)
- CCL: 222 milliseconds
- ECL: 590 milliseconds

Sizes of the related executables (if not applying tricks with SBCL and CCL) also differ greatly:

- SBCL: 46.1 MB -- there's a reason why a Lisp implementation is hosting a [World](#the-lisp-machine-as-a-world); still with version _SBCL 2.2.9.debian_
- CCL: 27.2 MB
- ECL: 79.1 kB -- this little size helps to explain "Embeddable..."

Here's an article from 2020 with much more information on how to build a project in Lisp: _**Common Lisp in Practice**_: https://atomized.org/blog/2020/07/06/common-lisp-in-practice/

This leaves one question open: where does the name _**Steel Bank**_ come from?

This name has its root in its precursor _**Carnegie Mellon** University Common Lisp_ (CMUCL): https://sbcl.org/history.html with Carnegie Mellon University's big original sponsors Andrew Carnegie with steel and the Mellon family with banking.

<br/>

## Common Lisp on the Java Virtual Machine (JVM) with Armed Bear Common Lisp (ABCL)

After the showstopper at [Kawa Scheme](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Scheme%20dialects%20on%20the%20Java%20Virtual%20Machine%20(JVM)#kawa-scheme) on the JVM, I thought to give ABCL a try.

But a look into its official User Manual (https://abcl.org/releases/1.9.2/abcl-1.9.2.pdf) shows a common pattern:

- it's apparently not so easy to make a portable "fat JAR" or "uberJAR" (JAR = Java Archive) file for the JVM from Common Lisp source code files.

However, I've found these two sources:

- https://stackoverflow.com/questions/61381499/how-do-i-create-a-jar-using-armed-bear-common-lisp (1)
- https://kodejava.org/how-do-i-evaluate-or-execute-a-script-file/ (2)

..and I can also reuse my little knowledge with the Maven build tool (https://maven.apache.org/guides/getting-started/maven-in-five-minutes.html) when I was playing with the GraalVM to make fast standalone apps based on uberJAR files: [Graal Virtual Machine](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/04%20-%20GraalVM#graal-virtual-machine-graalvm) (3)

<br/>

### "Hello, World!" in Common Lisp on the JVM

I take the ECL approach from above chapter [Execution speed of a Lisp program as a standalone executable in Linux...](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Common%20Lisp#execution-speed-of-a-lisp-program-as-a-standalone-executable-in-linux-steel-bank-common-lisp) to have a _main_ function too (potentially for better future expansions), though it wouldn't be needed here. However, I leave away the final _exit_ command because it's not needed here:

```
$ cat hello_world_abcl.lisp
(defun main ()
(princ "Hello, world from Armed Bear Common Lisp (ABCL)!")
(terpri))
(main)
$
```

Then I create some directories in Linux: _$ mkdir -p ./hello_world/src/main/java/hello_world_abcl_

Now I copy source code file _hello_world_abcl.lisp_ into the project working directory _./hello_world/hello_world_abcl.lisp_

<br/>

#### The pom.xml file

This by far the hardest part, but doable after some tinkering!

A Project Object Model or POM file is an XML file that contains information about the project and configuration details used by Maven to build the project. 

I mixed both approaches, (1) and (3), into a [pom.xml](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Common%20Lisp/pom_hello_world_abcl.xml) file (which cannot be renamed!):

```
<project xmlns="http://maven.apache.org/POM/4.0.0"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
   http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>
    <groupId>hello_world_abcl</groupId>
    <artifactId>hello_world_abcl</artifactId>
    <version>1.0.0</version>
    <packaging>jar</packaging>
    <name>hello_world_abcl</name>
    <url>http://maven.apache.org</url>
    <dependencies>
        <dependency>
            <groupId>org.abcl</groupId>
            <artifactId>abcl</artifactId>
            <version>1.9.2</version>
        </dependency>
    </dependencies>
    <build>
        <finalName>hello_world_abcl</finalName>
        <plugins>
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-compiler-plugin</artifactId>
                <version>3.14.0</version>
                <configuration>
                    <source>1.8</source>
                    <target>1.8</target>
                </configuration>
            </plugin>
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-assembly-plugin</artifactId>
                <version>3.7.1</version>
                <configuration>
                    <descriptorRefs>
                        <descriptorRef>jar-with-dependencies</descriptorRef>
                    </descriptorRefs>
                    <archive>
                        <manifest>
                            <addClasspath>true</addClasspath>
                            <mainClass>hello_world_abcl.Main</mainClass>
                        </manifest>
                    </archive>
                </configuration>
                <executions>
                    <execution>
                        <id>make-assembly</id>
                        <phase>package</phase>
                        <goals>
                            <goal>single</goal>
                        </goals>
                    </execution>
                </executions>
            </plugin>
        </plugins>
    </build>
</project>  
```

Then I copy _pom.xml_ also into the project working directory: _./hello_world/pom.xml_

If not done yet, build or install ABCL (I built without problems after installing Ant: _$ sudo apt-get install ant_) according to the given instructions, and also install Maven if not done yet: _$ sudo apt-get install maven_

<br/>

#### The Java hosting file

Now (2) comes into play, here with a Java source code file named [Main.java](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Common%20Lisp/Main_hello_world_abcl.java)

```
package hello_world_abcl;

import javax.script.ScriptEngineManager;
import javax.script.ScriptEngine;
import javax.script.ScriptException;
import java.io.File;
import java.io.Reader;
import java.io.FileReader;
import java.io.FileNotFoundException;

import org.armedbear.lisp.scripting.AbclScriptEngine;
import org.armedbear.lisp.scripting.AbclScriptEngineFactory;

public class Main {
    public static void main(String[] args) {
        AbclScriptEngine scriptEngine = (AbclScriptEngine) new AbclScriptEngineFactory()
                    .getScriptEngine();       
        try {
            // https://kodejava.org/how-do-i-evaluate-or-execute-a-script-file/
            File script = new File("hello_world_abcl.lisp");
            Reader reader = new FileReader(script);
            scriptEngine.eval(reader);
        } catch (FileNotFoundException | ScriptException e) {
            e.printStackTrace();
        }
    }
}
```
<br/>

..which is then copied into the Java source code directory: _./hello_world/src/main/java/hello_world_abcl/Main.java_

Now I change into the project working directory: _$ cd hello_world_

..and build the whole thing, which may take some time at the first time with a couple of downloads before building the uberJAR file: _$ mvn package_

```
...
[INFO] --- maven-assembly-plugin:3.7.1:single (make-assembly) @ hello_world_abcl ---
[INFO] Building jar: .../Armed_Bear_Common_Lisp/hello_world/target/hello_world_abcl-jar-with-dependencies.jar
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time: 2.048 s
[INFO] Finished at: 2025-07-03T11:47:28+02:00
[INFO] ------------------------------------------------------------------------
$
```

With this building success comes the big moment: running the "Hello, World!"-uberJAR on the JVM:

```
$ java -jar ./target/hello_world_abcl-jar-with-dependencies.jar
Failed to introspect virtual threading methods: ...
...
Hello, world from Armed Bear Common Lisp (ABCL)!
$
```

Well, it got me an error, but it worked!

(this error may come from the different JDK versions, because Armed Bear Common Lisp 1.9.2 is using _Java 11.0.26 Eclipse Adoptium, OpenJDK 64-Bit Server VM_ and my Java default environment is: _$ java -version_ --> _openjdk version "21.0.7" 2025-04-15 ..._).

However, I copied _hello_world_abcl-jar-with-dependencies.jar_ ("Write once, run anywhere": https://en.wikipedia.org/wiki/Write_once%2C_run_anywhere) to my Windows 11 machine and ran it like:

```
> java -jar ./hello_world_abcl-jar-with-dependencies.jar
Hello, world from Armed Bear Common Lisp (ABCL)!
>
```

<br/>

#### What about my microbenchmark program in Common Lisp on the JVM?

It works too, and also with the "ECL approach" for _main ()_ in the Common Lisp source code file.

This means that both string builder functions, that is _make-string-output-stream_ and _get-output-stream-string_, see from [source code](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Common%20Lisp/random_streams_for_perf_stats2.lisp), must have been implemented correctly in Java.

Furthermore, Common Lisp's [handler-case macro](https://lisp-docs.github.io/cl-language-reference/chap-9/j-c-dictionary/handler-case_macro) for exception handling when writing to a file is apparently also working correctly:

```
; write bit stream to disk:
  (handler-case
    (with-open-file (stream *file_bits_x* :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format stream *bits_x*)
      (format t "Bit stream has been written to disk under name:  ~a~%" *file_bits_x*))
    (file-error (e)
      (format t "could not write to file: ~a -- ~a~%" *file_bits_x* e)))
```

For a test case, I just temporarily change the permissions of file *random_bitstring.bin* from "Read and Write" to "Read-Only" and then run my program again. Now the program defined error message should be shown like this: _could not write to file: ..._, executed in Linux again:

```
$ java -jar ./target/random_streams_for_perf_stats-jar-with-dependencies.jar
Failed to introspect virtual threading methods: ...
...

generating a random bit stream...
could not write to file: random_bitstring.bin -- Unable to open #P".../Armed_Bear_Common_Lisp/random_bitstring.bin".
Byte stream has been written to disk under name: random_bitstring.byte
$ 
```

... but the program keeps running and is doing its remaining tasks.

#### Execution speed

My program takes about 4 seconds to run in this environment, this is much slower than Clojure's 600 milliseconds ([Master diagram with most program environments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/02%20-%20execution%20times#master-diagram-with-most-program-environments)), but better than my first version in Clojure without Java's _StringBuilder_ class with over 7 seconds: [Initial struggles with execution speed](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Clojure#initial-struggles-with-execution-speed)

But again, this is a solution with "natural" functional approach to exception handling and not Clojure's "natural" fallback to an imperative _try-catch_ construct.

#### What about the GraalVM? (doesn't work)

> Using the GraalVM (https://www.graalvm.org/; VM = Virtual Machine) for a Scala, Kotlin and Clojure program is a real hit:...

from: [Ahead Of Time (AOT) program compilation with the GraalVM](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/04%20-%20GraalVM#ahead-of-time-aot-program-compilation-with-the-graalvm)

Emboldened by so much success, I tried my luck with the GraalVM again and compiled the uberJAR file into a binary, standalone executable:

```
$ ... /.sdkman/candidates/java/24-graal/lib/svm/bin/native-image -jar hello_world_abcl-jar-with-dependencies.jar
...
```

This command has indeed built something, but the generated program is not working:

```
$ ./hello_world_abcl-jar-with-dependencies
Failed to introspect virtual threading methods: java.lang.NoSuchMethodException: java.lang.ThreadBuilders$VirtualThreadFactory.newThread(java.lang.Runnable)
java.lang.ClassNotFoundException: org.armedbear.lisp.Primitives
at org.graalvm.nativeimage.builder/com.oracle.svm.core.hub.ClassForNameSupport.forName(ClassForNameSupport.java:215)
at org.graalvm.nativeimage.builder/com.oracle.svm.core.hub.ClassForNameSupport.forName(ClassForNameSupport.java:183)
...
$
```

Well, I guess that nobody claimed so far that uberJAR files based on Armed Bear Common Lisp can be used for the GraalVM.

The major difference to Scala, Kotlin and Clojure on the GraalVM is the fact that with those languages I could just use my default OpenJDK environment to build an uberJAR file which is then being AOT compiled.

<br/>

#### The Lisp Machine as a "World"

> ..a user could run for several days or even a few weeks before having to save out the running “world” to disk and restart it.

from: "The Evolution of Lisp" by Guy L. Steele Jr. and Richard P. Gabriel, 1992: https://doc.lagout.org/programmation/Lisp/Lisp%20Mess/Gabriel%20%26%20Steele%20-%20The%20Evolution%20of%20Lisp.pdf

<br/>

##_end
