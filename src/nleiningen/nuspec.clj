(ns nleiningen.nuspec
  (:require
   [nleiningen.core :as core])
  (:import
   [NuGet PackageBuilder]
   [System.IO FileStream FileMode Path]))

(defn nuspec [args]
  (core/bootstrap-project)
  (let [pg (PackageBuilder.)
        proj @core/*project*
        id (:name proj)]
    (set! (.Id pg) id)
    (println proj)
    (.Save pg (FileStream. (Path/Combine core/*project-root* (str id ".nuspec")) FileMode/OpenOrCreate))))
