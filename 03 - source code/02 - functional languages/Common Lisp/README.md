# Common Lisp

2025-09-16: work in progress

TBD: add a link from "02 - functional languages" to chapter "A little exercise in Common Lisp"

https://lisp-lang.org/

SBCL = **Steel Bank Common Lisp**:

- https://www.sbcl.org/
- https://www.sbcl.org/history.html

Lisp implementations: https://lisp-lang.org/wiki/article/implementations

<br/>

Table of contents:

- [A little exercise in Common Lisp](#a-little-exercise-in-common-lisp)
- [Execution speed of a Lisp program as a standalone executable in Linux: Steel Bank Common Lisp](#execution-speed-of-a-lisp-program-as-a-standalone-executable-in-linux-steel-bank-common-lisp)
- [Common Lisp on the Java Virtual Machine (JVM) with Armed Bear Common Lisp (ABCL)](#common-lisp-on-the-java-virtual-machine-jvm-with-armed-bear-common-lisp-abcl)

---

## A little exercise in Common Lisp

I found this little exercise in Common Lisp (on the _sbcl_ REPL (Read-Eval-Print Loop); start in your Linux shell with _$ rlwrap sbcl_ to get some expression history with keyboard controls):

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

> Functional Programming is (obviously) not taking over the world.

(which refers to: _If Haskell is so great, why hasn't it taken over the world? ..._ from 2017: https://pchiusano.github.io/2017-01-20/why-not-haskell.html)

<br/>

## Execution speed of a Lisp program as a standalone executable in Linux: Steel Bank Common Lisp

(TBD)

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

I take the ECL approach from the chapter [Execution speed of a Lisp program as a standalone executable in Linux...](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Common%20Lisp#execution-speed-of-a-lisp-program-as-a-standalone-executable-in-linux-steel-bank-common-lisp) above to have a _main_ function too (potentially for better future expansions), though it wouldn't be needed here. However, I leave away the final _exit_ command because it's not needed here:

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

#### What about my microbenchmark program in Common Lisp?

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

My program takes about 4 seconds to run in this environment, this is much slower than Clojure's 780 milliseconds ([The 1 second execution time limit](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main?tab=readme-ov-file#the-1-second-execution-time-limit)), but better than my first version in Clojure without Java's _StringBuilder_ class with more than 7 seconds: [Initial struggles with execution speed](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Clojure#initial-struggles-with-execution-speed)

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

##_end
