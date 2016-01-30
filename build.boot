(set-env!
  :source-paths #{"src" "test"}
  :resource-paths #{"src" "test"}
  :dependencies '[[adzerk/boot-cljs "1.7.228-1" :scope "test"]
                  [adzerk/boot-cljs-repl "0.3.0" :scope "test"]
                  [adzerk/boot-reload "0.4.4" :scope "test"]
                  [pandeiro/boot-http "0.7.0" :scope "test"]
                  [provisdom/boot-tasks "0.4.0" :scope "test"]])

(require
  '[adzerk.boot-cljs :refer :all]
  '[adzerk.boot-reload :refer :all]
  '[provisdom.boot-tasks :refer :all]
  '[pandeiro.boot-http :refer :all])

(set-project-deps!)

(default-task-options!)

(task-options!
  reload {:on-jsload 'allgress.cereus.core/on-jsload})

(deftask web-dev
         "Developer workflow for web-component UX."
         []
         (comp
           (asset-paths :asset-paths #{"bower_components" "html"})
           (serve :dir "target/")
           (watch)
           (speak)
           (reload)
           #_(cljx)
           #_(cljs-repl)
           (cljs)
           ))

