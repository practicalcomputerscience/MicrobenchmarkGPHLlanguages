# Tips for working with the sbt (simple build tool)

## More detailed compilation warnings

Sometimes you want more detailed warnings during compilation, something which can be really helpful when working with intractable third-party libraries.

Edit _built.sbt_ file and add:

_scalacOptions ++= Seq("-deprecation", "-feature")_

Seen from here: https://alvinalexander.com/scala/scala-sbt-re-run-with-deprecation-feature-message/

<br/>

## Just keep it standard and compile to bytecode for the JVM:

See from here: [Is "Scala native" worth the effort?](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Scala/Running%20and%20building%20Scala%20programs%20-%20baby%20steps/(F)%20Working%20with%20scala-cli%20to%20create%20a%20standalone%20program%20in%20Windows%2011#is-scala-native-worth-the-effort)

<br/>

##_end
