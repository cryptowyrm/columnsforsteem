(set-env!
  :source-paths #{"src"}
  :dependencies '[[adzerk/boot-cljs "2.1.4" :scope "test"]
                  [adzerk/boot-reload "0.5.2" :scope "test"]
                  [org.clojure/test.check "0.9.0" :scope "test"]
                  [crisptrutski/boot-cljs-test "0.3.5-SNAPSHOT" :scope "test"]
                  [nightlight "2.0.4" :scope "test"]
                  [proto-repl "0.3.1" :scope "test"]
                  [binaryage/devtools "0.9.8" :scope "test"]
                  [binaryage/dirac "RELEASE" :scope "test"]
                  [powerlaces/boot-cljs-devtools "0.2.0" :scope "test"]
                  ; project deps
                  [org.clojure/clojure "1.9.0-beta4"]
                  [org.clojure/clojurescript "1.9.946"]
                  [org.clojure/core.async "0.3.443"]
                  [org.clojure/data.json "0.2.6"]
                  [org.clojure/tools.cli "0.3.5"]
                  [javax.xml.bind/jaxb-api "2.3.0"]
                  [http-kit "2.2.0"]
                  [ring "1.6.2"]
                  [reagent "0.8.0-alpha2"]
                  [cljs-react-material-ui "0.2.48"]
                  [com.cemerick/url "0.1.1"]
                  [rum "0.10.8"]
                  [play-cljs "0.10.2"]
                  [compojure "1.6.0"]
                  [com.rpl/specter "1.0.4"]
                  [com.taoensso/sente "1.11.0"]
                  [org.clojure/java.jdbc "0.7.3"]
                  [com.h2database/h2 "1.4.196"]
                  [honeysql "0.9.1"]])

(task-options!
  pom {:project 'columnsforsteem
       :version "1.0.0-SNAPSHOT"
       :description "FIXME: write description"}
  aot {:namespace '#{columnsforsteem.server}}
  jar {:main 'columnsforsteem.server}
  sift {:include #{#"\.jar$"}})

(require
  '[clojure.spec.test.alpha :refer [instrument]]
  '[nightlight.core :as nightlight]
  '[adzerk.boot-cljs :refer [cljs]]
  '[adzerk.boot-reload :refer [reload]]
  '[crisptrutski.boot-cljs-test :refer [test-cljs]]
  '[powerlaces.boot-cljs-devtools :refer [cljs-devtools dirac]]
  'columnsforsteem.server)

(deftask testing []
  (merge-env! :source-paths #{"test"})
  identity)

(deftask test-once []
  (set-env! :resource-paths #{"resources" "test-resources"})
  (comp
    (testing)
    (test-cljs :js-env :phantom)))

(deftask auto-test []
  (set-env! :resource-paths #{"resources" "test-resources"})
  (comp
    (testing)
    (watch)
    (test-cljs :js-env :phantom)))

(deftask dev []
  (set-env! :resource-paths #{"resources" "dev-resources"})
  (comp
    (with-pass-thru _
      (instrument)
      (let [server (columnsforsteem.server/-main)
            port (-> server meta :local-port)
            url (str "http://localhost:" port "/index.html")]
        (println "Started app on" url)
        (nightlight/start {:port 4000 :url url})))
    (watch)
    (reload :asset-path "columnsforsteem")
    (cljs-devtools)
    (dirac)
    (cljs
      :source-map true
      :optimizations :none
      :compiler-options {:asset-path "main.out"})
    (target)))

(deftask build []
  (set-env! :resource-paths #{"resources" "prod-resources"})
  (comp
    (cljs :optimizations :advanced)
    (aot)
    (pom)
    (uber)
    (jar)
    (sift)
    (target)))

(deftask build-simple []
  (set-env! :resource-paths #{"resources" "prod-resources"})
  (comp
    (cljs :optimizations :advanced)
    (target)))
