# Tips for working with the sbt (simple build tool)

## More detailed compilation warnings

Sometimes you want more detailed warnings during compilation, something which can be really helpful when working with intractable third-party libraries.

Edit _built.sbt_ file and add:

_scalacOptions ++= Seq("-deprecation", "-feature")_

Seen from here: https://alvinalexander.com/scala/scala-sbt-re-run-with-deprecation-feature-message/

<br/>

## Just keep it standard and compile to bytecode for the JVM:

See from here: [Is "Scala native" worth the effort?](TBD) <============== 2025-10-04

<br/>

##_end
