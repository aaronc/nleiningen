(ns nleiningen.signing
  (:import [System.Reflection StrongNameKeyPair]
           [System.IO File FileMode]
           [Mono.Cecil AssemblyDefinition WriterParameters]))

(defn create-strong-name-key-pair [strong-name-key-path]
  (with-open [fs (File/Open (Environment/ExpandEnvironmentVariables strong-name-key-path) FileMode/Open)]
    (StrongNameKeyPair. fs)))

(defn strong-name-sign
  ([assembly-path strong-name-key-path]
     (strong-name-sign assembly-path strong-name-key-path assembly-path))
  ([assembly-path strong-name-key-path assembly-out-path]
     (let [assembly-path (Environment/ExpandEnvironmentVariables assembly-path)
           assembly-out-path (Environment/ExpandEnvironmentVariables assembly-out-path)
           asm (AssemblyDefinition/ReadAssembly assembly-path)
            wp (WriterParameters.)]
       (set! (.StrongNameKeyPair wp)
             (create-strong-name-key-pair strong-name-key-path))
       (.Write asm assembly-out-path wp))))
