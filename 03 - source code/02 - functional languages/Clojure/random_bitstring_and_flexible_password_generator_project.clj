(defproject random_bitstring_and_flexible_password_generator "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  ; :main ^:skip-aot random-bitstring-and-flexible-password-generator.core
  :main random-bitstring-and-flexible-password-generator.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
)
