# Swift

https://www.swift.org/

<br/>

Swift may explain why you cannot make money with a (new) programming language apparently, only at its boundaries, that is when connecting to the outside world.

Or put differently: why is Apple's Swift (finally) [open source](https://github.com/swiftlang), but not [SwiftUI](https://swiftpackageregistry.com/OpenSwiftUIProject/OpenSwiftUI)?

<br/>

Command _$ swift build -c release_ is not compiling to a standalone executable for Linux. The compiled program depends on one **shared library** (_libswiftCore_) at least, which is provided if Swift is installed on the target machine.

##_end
