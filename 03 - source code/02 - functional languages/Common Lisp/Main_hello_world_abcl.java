// Main.java
// 
// 2025-07-03
// 
// test on Ubuntu 24 LTS: 
// 
// build on Ubuntu 24 LTS: get pom.xml ready
//                         get hello_world_abcl.lisp ready
//                         $ mkdir -p ./hello_world/src/main/java/hello_world_abcl
//                         copy hello_world_abcl.lisp into working dir = dir with pom.xml file
//                         copy this file (Main.java) into dir: ./hello_world/src/main/java/hello_world_abcl
//                         $ cd hello_world
//                         $ mvn package  # run in working dir
// 
// run on Ubuntu 24 LTS:   $ java -jar ./target/hello_world_abcl-jar-with-dependencies.jar
//                         
// run on Windows 11:      > java -jar ./hello_world_abcl-jar-with-dependencies.jar


package hello_world_abcl;

import javax.script.ScriptEngineManager;
import javax.script.ScriptEngine;
import javax.script.ScriptException;
import java.io.File;
import java.io.Reader;
import java.io.FileReader;
import java.io.FileNotFoundException;


// https://stackoverflow.com/questions/61381499/how-do-i-create-a-jar-using-armed-bear-common-lisp
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

// end of Main.java
