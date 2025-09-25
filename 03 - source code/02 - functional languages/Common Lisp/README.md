# Common Lisp

2025-09-16: work in progress

TBD: add a link from "02 - functional languages" to chapter "A little exercise in Common Lisp"

https://lisp-lang.org/

https://www.sbcl.org/

SBCL = Steel Bank Common Lisp: https://www.sbcl.org/history.html

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

With the background of my showstopper at [Kawa Scheme](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Scheme%20dialects%20on%20the%20Java%20Virtual%20Machine%20(JVM)#kawa-scheme) on the JVM, I thought to give ABCL a try.

But a look into its official User Manual (https://abcl.org/releases/1.9.2/abcl-1.9.2.pdf) shows a common pattern:

- it's apparently not so easy to make a portable "fat JAR" or "uberJAR" (JAR = Java Archive) file for the JVM from Common Lisp source code files.

However, I've found these two sources:
- https://stackoverflow.com/questions/61381499/how-do-i-create-a-jar-using-armed-bear-common-lisp (*)
- https://kodejava.org/how-do-i-evaluate-or-execute-a-script-file/ (**)
- and I can also reuse my little knowledge with the Maven build tool (https://maven.apache.org/guides/getting-started/maven-in-five-minutes.html) when I was playing with the GraalVM to make fast standalone apps based on uberJAR files: [Graal Virtual Machine](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/04%20-%20GraalVM#graal-virtual-machine-graalvm) (***)

### "Hello, World!" in Common Lisp on the JVM

I take the ECL approach from above the chapter above (TBD) to have a _main_ function too (for better future expansions potentially), though it wouldn't be needed here, but I leave
away the final _exit_ command because it's not needed here:

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

### The pom.xml file

This by far the hardest part, but doable after some tinkering!

A Project Object Model or POM file is an XML file that contains information about the project and configuration details used by Maven to build the project. 

I mixed both approaches, (*) and (***), into a _pom.xml_ file (which cannot be renamed!):

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


<br/>

##_end
