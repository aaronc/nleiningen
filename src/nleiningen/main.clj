(ns nleiningen.main
  (:import
   [System.IO Directory])
  (:use
   [nleiningen core nuspec]))

(defn main [& args]
  (try
    (binding [*project-root* (or *project-root* (Directory/GetCurrentDirectory))
              *project* (atom nil)
              *compile-path* nil]
      (when (load-project)
        (let [args (or args ["repl"])
              nargs (count args)]
          (cond
           (= 0 args)
           (println "NLeiningen Preview. Tasks: repl, compile")
           :default
           (case (first args)
             "repl" (start-repl (rest args))
             "compile" (compile-project)
             "nuspec" (nuspec (rest args))
             (args-error args)))))
      0)
    (catch Exception ex
               (println "Error: " ex)
               -1)))

