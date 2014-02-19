(defproject sneerteam.chat-terminal "0.1.0-SNAPSHOT"
  :description "A simple chat client."
  :url "http://github.com/sneerteam/chat-terminal"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [amalloy/ring-buffer "1.0"]
                 [clojure-lanterna "0.9.4"] ; http://sjl.bitbucket.org/clojure-lanterna/reference/#consoles
                 [com.googlecode.lanterna/lanterna "2.1.7"]]
  :main ^:skip-aot sneerteam.chat-terminal.main
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
