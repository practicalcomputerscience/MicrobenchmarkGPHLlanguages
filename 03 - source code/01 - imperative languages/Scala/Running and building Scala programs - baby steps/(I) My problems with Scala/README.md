# My problems with Scala

<br/>

### 1/ Does a character or string have symbolic or literal meaning?

```
import java.io._  // for FileWriter etc.
...
    val path1: os.Path = os.pwd / file_name  // '/' is part of path!
```

### 2/ val and var

Do the keywords _val_ and _var_ refer to the immutability and mutability, respectively, of the related objects or the values inside these objects?

### 3/ 3rd party library imports when using the Scala code runner

Scala code runner like: _$ scala < Scala program name >_

I was only successful with importing 3rd party libraries so far when using the sbt (simple build tool).

### 4/ Loop indices

A loop with a syntax like this _...r <- 0 to RUNS..._ is inclusive on both sides; so _RUNS_ acts like a valid index value!

When you come from the Python world etc., you are used to have the loop only running like: 0,1,2,... , RUNS - 1 (inclusively)

### 5/ How to read from the console when executing JavaScript on node.js?

See at: [How to read from the console when executing JavaScript on node.js?](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Scala/Running%20and%20building%20Scala%20programs%20-%20baby%20steps/(H)%20From%20a%20Scala%20program%20to%20JavaScript%20for%20node.js#how-to-read-from-the-console-when-executing-javascript-on-nodejs)

<br/>

##_end
