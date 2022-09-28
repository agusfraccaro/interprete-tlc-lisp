(defproject tp-final "0.1.0-SNAPSHOT"
  :description "TP FINAL LENGUAJES FORMALES - FACULTAD DE INGENIERÍA UBA
                           INTÉRPRETE TLC-LISP
                    ALUMNA: AGUSTINA FRACCARO - Padrón 103199"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]]
  :main ^:skip-aot tp-final.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
