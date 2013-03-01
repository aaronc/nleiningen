(ns nleiningen.signing
  (:import [System.Reflection StrongNameKeyPair]
           [System.IO File FileMode]
           [Mono.Cecil AssemblyDefinition WriterParameters]))

(defn strong-name-sign
  ([assembly-path strong-name-key-path]
     (strong-name-sign assembly-path strong-name-key-path assembly-path))
  ([assembly-path strong-name-key-path assembly-out-path]
      (let [asm (AssemblyDefinition/ReadAssembly assembly-path)
            wp (WriterParameters.)]
        (with-open [fs (File/Open strong-name-key-path FileMode/Open)]
          (let [snk (StrongNameKeyPair. fs)]
            (set! (.StrongNameKeyPair wp) snk)
            (.Write asm assembly-out-path wp))))))
