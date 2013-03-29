(ns nleiningen.core
  (:use
   [clojure.clr.emit :only [op]])
  (:require
   [clojure.string :as str]
   [clojure.clr.emit :as emit]
   [clojure.clr.io :as io]
   [nleiningen.signing :as signing])
  (:import
   [System.IO Path Directory SearchOption DirectoryInfo StreamReader File TextReader
    FileMode FileShare FileAccess MemoryStream]
   [System.Reflection Assembly BindingFlags AssemblyName
    AssemblyFileVersionAttribute AssemblyInformationalVersionAttribute
    AssemblyTitleAttribute AssemblyDescriptionAttribute
    ResourceAttributes]
   [System.Reflection.Emit OpCodes PEFileKinds CustomAttributeBuilder]
   [NuGet PackageRepositoryFactory SemanticVersion PackageManager
    PackageOperationEventArgs]
   [clojure.lang.CljCompiler.Ast GenContext]
   [|System.EventHandler`1[NuGet.PackageOperationEventArgs]|]))

(def repo-path "packages")

(def ^:dynamic *project* (atom nil))

(defn on-package-installed [s e]
  (let [pkg (.Package e)]
    (println "Package" pkg "installed")
    (swap! *project* update-in [:packages] conj pkg)))

(defn create-nuget-repo [url]
  (let [repo (.CreateRepository (PackageRepositoryFactory/Default) url)
        pack-man (PackageManager. repo repo-path)]
    (.add_PackageInstalled pack-man
                           (gen-delegate
                            |System.EventHandler`1[NuGet.PackageOperationEventArgs]|
                            [s e] (on-package-installed s e)))
    {:repo repo
     :pack-man pack-man}))

(def nuget-repo (create-nuget-repo "http://go.microsoft.com/fwlink/?LinkID=206669"))

(def clj-clr-myget-repo (create-nuget-repo "http://www.myget.org/F/clojure-clr/"))

(defn repo-install-package [{:keys [pack-man repo]} id ver & {:keys [throw?]}]
  [id ver]
  (let [ver (when ver (SemanticVersion. ver))]
    (try
      (if ver
        (.InstallPackage pack-man id ver)
        (.InstallPackage pack-man id))
      (let [pkgs (filter (fn package-filter [pkg]
                          (and (= (.Id pkg) id)
                               (>= (.CompareTo (.Version pkg) ver) 0)))
                         (.. pack-man LocalRepository GetPackages))
            pkg (last (sort (fn pkg-sort [a b]
                              (.CompareTo (.Version a) (.Version b))) pkgs))]
        (swap! *project* update-in [:packages] conj pkg)
        (println "Found package" id (str (.Version pkg)) "in repository:" (.Source repo)))
      true
      (catch Exception ex
        (println (str "Unable to find package " id (when ver ver) " in repository: " (.Source repo)))
        (when throw? (throw ex))
        false))))

(defn install-package
  ([name]
     (install-package name nil))
  ([name ver]
     (or
      (repo-install-package clj-clr-myget-repo name ver)
      (repo-install-package nuget-repo name ver :throw? true))))

(def target-framework (System.Runtime.Versioning.FrameworkName. ".NETFramework,Version=v4.0"))

(defn install-dependencies [deps]
  (doseq [dep deps]
    (let [name (str (first dep))
          dep (cons name (rest dep))])
    (apply install-package dep)))

(defn- target-framework-filter [asmref]
  (if-let [asmref-framework (.TargetFramework asmref)]
    (and (= (.Version target-framework) (.Version asmref-framework))
         (#{"" "Client"} (.Profile asmref-framework)))
    true))

(defn get-project-packages []
  (seq (:packages @*project*)))

(defn get-dependency-assembly-references []
  (let [asmrefs (flatten
                 (map #(seq (.AssemblyReferences %))
                      (get-project-packages)))]
    (filter target-framework-filter asmrefs)))

(defn add-to-bin-path [path]
  (let [bin-path (.. (AppDomain/CurrentDomain) SetupInformation PrivateBinPath)]
    (set! (.. (AppDomain/CurrentDomain) SetupInformation PrivateBinPath)
          (str ";" path))))

(defn get-package-info []
  (for [package (get-project-packages)]
    {:id (.Id package)}))

(defn read-bytes [stream]
  (let [mem (System.IO.MemoryStream.)]
    (.CopyTo stream mem)
    (.ToArray mem)))

(defn- clj-init-type-name [asm-or-name]
  (let [asm-name (cond (instance? Assembly asm-or-name)
                       (.. asm-or-name GetName Name)
                       (instance? AssemblyName asm-or-name)
                       (.Name asm-or-name)
                       :default
                       asm-or-name)]
    (str (clojure.lang.Compiler/munge asm-name) ".ClojureInit")))
;;(def ^:private ^:const clj-init-type-name "__$Clj$Init$__")

(defn invoke-asm-load-hooks [asm]
  (when-let [init-type (.GetType asm (clj-init-type-name asm))]
    (let [init-method (.GetMethod init-type "Init")]
      (.Invoke init-method nil nil))))

(def ^:dynamic *project-root* nil)

(defn load-dependencies []
  (doseq [asmref (get-dependency-assembly-references)]
    (let [src-path (.SourcePath asmref)
          asm-name (AssemblyName/GetAssemblyName src-path)
          asm (or (try (Assembly/Load (.Name asm-name))
                       (catch Exception ex))
                  (assembly-load-file src-path))]
      (when asm
        (invoke-asm-load-hooks asm)
        (swap! *project* assoc-in [:computed-dependencies (Path/GetFileName src-path)] {:path src-path :asm asm :asm-name asm-name})
        (println "Loaded" asm)))))

(defn configure-load-path [src-paths]
  (str/join ";" src-paths))

(defn process-project-map [proj]
  (comment (let [{:keys
                  [dependencies
                   source-paths
                   local-dependencies]
                  :or
                  [source-paths ["src"]]
                  :as proj} proj]
             proj))
  (let [proj (merge
              {:source-paths ["src"]
               :target-path "bin"
               :target :dll
               :computed-dependencies {}
               :packages #{}}
              proj)]
    proj))

(defmacro defproject [name version & attrs]
  `(merge (hash-map ~@attrs)
          {:name ~(str name)
           :version ~version}))

(defn resolve-path [path]
  (Path/GetFullPath
   (if-not (Path/IsPathRooted path)
     (Path/Combine
      *project-root* path)
     path)))

(defn read-project-file [proj-file]
  (when (File/Exists proj-file)
    (let [src (File/ReadAllText proj-file)
          proj (process-project-map (eval (read-string src)))]
      (set! *compile-path* (Path/Combine *project-root* (:target-path proj)))
      (reset! *project* proj)
      true)))

(defn load-project []
  (or
   (read-project-file (Path/Combine *project-root* "clr-project.clj"))
   (read-project-file (Path/Combine *project-root* "project.clj"))
   (println "project.clj or clr-project.clj not found")))

(defn load-dependency [path]
  (let [filename (Path/GetFileName path)
        asm (assembly-load-file path)]
    (println "Loading" path)
    (swap! *project* assoc-in [:computed-dependencies filename] {:path path :asm asm :asm-name (.GetName asm)})))

(declare compile-project)

(defn get-target-ext [target]
  (if (or (= target :console)
          (= target :windows))
    ".exe" ".dll"))

(defn get-compiled-target-name []
    (let [{:keys [name target] :as proj} @*project*
          ext (get-target-ext target)]
  (str (Path/Combine *compile-path* (str name ext)))))

(defn add-to-load-path [paths]
  (let [load-path (Environment/GetEnvironmentVariable "CLOJURE_LOAD_PATH")
        paths (map resolve-path paths)
        load-path (str load-path ";" (str/join ";" paths))]
      (Environment/SetEnvironmentVariable "CLOJURE_LOAD_PATH" load-path)))

(defn load-sub-project [proj-path]
  (let [this-compile-path *compile-path*
        dep-path
        (binding [*project-root* proj-path
                  *project* (atom nil)
                  *compile-path* *compile-path*]
          (load-project)
          (add-to-load-path (:source-paths @*project*))
          (set! *compile-path* this-compile-path)
          (let [dep-path (get-compiled-target-name)]
            (when-not (File/Exists dep-path)
              (compile-project))
            dep-path))]
    (load-dependency dep-path)))

(defn bootstrap-project []
  (let [{:keys [gac-dependencies local-dependencies source-paths dependencies package-sources] :as proj} @*project*]
    (add-to-load-path source-paths)
    (install-dependencies dependencies)
    (load-dependencies)
    (doseq [dep gac-dependencies]
      (try
        (assembly-load dep)
        (catch Exception ex
          (println "Error loading" dep ":" ex))))
    (doseq [dep local-dependencies]
      (try
        (let [path (resolve-path dep)]
          (println "Trying to resolve" dep)
          (cond
           (Directory/Exists path) (load-sub-project path)
           (File/Exists path) (load-dependency path)
           :default (println "Don't know how to load local dependency" path)))
        (catch Exception ex
          (println "Error loading" dep ":" ex))))))

;; Compile Clojure Files
(defn get-clj-files
  ([src-path] (get-clj-files src-path nil))
  ([src-path rel]
     (let [path (if rel
                  (Path/Combine *project-root* src-path rel)
                  (Path/Combine *project-root* src-path))
           dir (DirectoryInfo. path)
           fsinfos (.GetFileSystemInfos dir)]
       (flatten
        (remove
         #(or (nil? %) (empty? %))
         (for [info fsinfos]
           (let [rel-path (if rel
                            (Path/Combine rel (.Name info))
                            (.Name info))
                 rel-path (.Replace rel-path "\\" "/")]
             (cond
              (instance? DirectoryInfo info) (get-clj-files src-path rel-path)
              (= ".clj" (.Extension info)) {:src-path src-path :rel-path rel-path :path (.FullName info) :name (.Name info)}
              :default nil))))))))

(defn find-app-domain-assembly [^AssemblyName asm-name]
  (let [app-domain-asms (seq (.GetAssemblies AppDomain/CurrentDomain))]
    (loop [asm (first app-domain-asms)
           more (rest app-domain-asms)]
      (when asm
        (if (= (.FullName asm-name) (.. asm GetName FullName))
          asm
          (recur (first more) (rest more)))))))

(defn paths-equal [path1 path2]
  (String/Equals
   (Path/GetFullPath path1)
   (Path/GetFullPath path2)
   StringComparison/InvariantCultureIgnoreCase))

(defn copy-assembly-references [asm-name copied]
  (try
    (when-not (contains? @copied asm-name)
      (when-let [asm (find-app-domain-assembly asm-name)]
        (when-not (.GlobalAssemblyCache asm)
          (let [path (.Location asm)
                name (.Name asm-name)]
            (when-not (String/IsNullOrEmpty path)
              (let [asm-file (Path/GetFileName path)
                    new-path (Path/Combine *compile-path* asm-file)]
                (when-not (paths-equal path new-path)
                  (File/Copy path new-path true)
                  (swap! copied conj asm-name)
                  (doseq [refed-asm (.GetReferencedAssemblies asm)]
                    (copy-assembly-references refed-asm copied))
                  asm-file)))))))
    (catch Exception ex
      (println "Error while trying to copy" asm-name "." ex))))

(let [is-compiling-field (.GetField clojure.lang.Compiler "CompilerActiveVar" (enum-or BindingFlags/Static BindingFlags/NonPublic BindingFlags/Public))]
  (def is-compiling-var (.GetValue is-compiling-field nil)))

;; From bultitude.core
(defn- read-ns-form
  "Given a reader on a Clojure source file, read until an ns form is found."
  [rdr]
  (let [form (try
               (binding [*read-eval* false]
                 (read rdr false ::done))
                  (catch Exception e ::done))]
    (if (try
          (and (list? form) (= 'ns (first form)))
          (catch Exception _))
      (try
        (str form) ;; force the read to read the whole form, throwing on error
        (second form)
        (catch Exception _))
      (when-not (= ::done form)
        (recur rdr)))))

;; From bultitude.core
(defn- ns-form-for-file [file]
  (with-open [r (clojure.lang.PushbackTextReader. (io/text-reader file))]
    (read-ns-form r)))

(defn- date-time-stamp [now]
  (format "%04d%02d%02d%02d%02d" (.Year now) (.Month now) (.Day now) (.Hour now) (.Minute now)))

(defn- date-time-version [now]
  (format "%04d.%02d.%02d.%02d%02d" (.Year now) (.Month now) (.Day now) (.Hour now) (.Minute now)))

(defn- set-custom-attr [target attr-type ctr-param-types ctr-params]
  (let [ctr (.GetConstructor attr-type (into-array Type ctr-param-types))
        attr-builder (CustomAttributeBuilder. ctr (into-array Object ctr-params))]
    (.SetCustomAttribute target attr-builder)
    attr-builder))

(defn add-as-embedded-resource
  [mod-builder embedded-filename bytes]
  (let [mem-stream (MemoryStream. bytes)]
    (.DefineManifestResource mod-builder
                             embedded-filename
                             mem-stream
                             ResourceAttributes/Public)))

(defn compile-project [& {:keys [clojure-dll]}]
  (bootstrap-project)
  (println "Compile path:" *compile-path*)
  (let [{:keys [name description source-paths computed-dependencies gac-dependencies main target version key-file
                disable-default-aot dll-exclusions] :as proj} @*project*
        clj-asm (or clojure-dll (assembly-load "Clojure"))
        gen-context-type (.GetType clj-asm "clojure.lang.CljCompiler.Ast.GenContext")
        files (flatten (for [path source-paths] (get-clj-files path)))
        ext (get-target-ext target)
        asm-name (AssemblyName. name)
        now (DateTime/Now)
        version (.Replace version "*" (date-time-stamp now))
        sem-ver (SemanticVersion. version)
        asm-ver (.Version sem-ver)
        ;; create-with-ext-asm-method
        ;; (.GetMethod gen-context-type "CreateWithExternalAssembly"
        ;;             (emit/type-array String String Boolean))
        ;; gen-ctxt nil
        ;; gen-ctxt (.Invoke create-with-ext-asm-method
        ;;                   nil (emit/obj-array name ext true))
        ;; compiler-type (.GetType clj-asm "clojure.lang.Compiler")
        ;; compile-method (.GetMethod compiler-type "Compile"
        ;;                            (emit/type-array gen-context-type TextReader
        ;;                                             String String
        ;;                                             String))
        ]
    (set! (.Version asm-name) asm-ver)
    (when key-file
      (set! (.KeyPair asm-name) (signing/create-strong-name-key-pair key-file)))
    (let [gen-ctxt (GenContext/CreateWithExternalAssembly (.Name asm-name) asm-name ext true)
          asm-builder (.AssemblyBuilder gen-ctxt)
          mod-builder (.ModuleBuilder gen-ctxt)]
      (when (not (Directory/Exists *compile-path*))
        (println "Creating" *compile-path*)
        (Directory/CreateDirectory *compile-path*))
      (set-custom-attr asm-builder
                       AssemblyFileVersionAttribute
                       [String]
                       [(date-time-version now)])
      (set-custom-attr asm-builder
                       AssemblyInformationalVersionAttribute
                       [String]
                       [version])
      (set-custom-attr asm-builder
                       AssemblyTitleAttribute
                       [String]
                       [name])
      (when description
        (set-custom-attr asm-builder
                         AssemblyDescriptionAttribute
                         [String]
                         [description]))
      (try
        (clojure.lang.Var/pushThreadBindings
         {emit/cur-gen-context-var gen-ctxt
          is-compiling-var true})
        (doseq [{:keys [rel-path path name]} files]
          (assert (.EndsWith rel-path ".clj"))
          ;;  (clojure.lang.RT/load  (.Substring rel-path 0 (- (.Length rel-path) 4)))
          (when-let [ns-name (ns-form-for-file path)]
            (when-not (some #(re-matches % rel-path) dll-exclusions)
              (try
                (if disable-default-aot
                  (do
                    (add-as-embedded-resource
                     mod-builder
                     (str (.Replace (str ns-name) "-" "_") ".clj")
                     (File/ReadAllBytes path))
                    (println "Embedded" ns-name))
                  (do
                    (compile (symbol ns-name))
                    (println "AOT compiled" ns-name)))
                (catch Exception ex
                  (println "Error compiling" path)
                  (throw ex)))))
          (comment
            (with-open [rdr (StreamReader. path)]
              ;;(clojure.lang.Compiler/Compile gen-ctxt rdr nil name rel-path)
              ;;(.Invoke compile-method nil (emit/obj-array gen-ctxt rdr nil
                                        ;name rel-path))
              )))
        (finally
          (clojure.lang.Var/popThreadBindings)))
      (let [clj-init-type (emit/clr-type* gen-ctxt (clj-init-type-name name))
            clj-init (emit/clr-method* clj-init-type "Init" [:Public :Static] nil nil)
            ilg (.GetILGenerator clj-init)
            asm-load-method (.GetMethod Assembly "Load" (emit/type-array String))]
        (binding [emit/*ilgen* ilg]
          (doseq [asm-name gac-dependencies]
            (op :Ldstr asm-name)
            (op :Call asm-load-method)
            (op :Pop))
          (doseq [[name {:keys [path asm asm-name]}] computed-dependencies]
            (let [init (when asm (.GetType asm (clj-init-type-name asm)))]
              (if init
                (let [init-method (.GetMethod init "Init")]
                  (op :Call init-method))
                (do
                  (op :Ldstr (.Name asm-name))
                  (op :Call asm-load-method)
                  (op :Pop)))))
          (op :Ret))
        (let [init-type (.CreateType clj-init-type)]
          (when main
            (let [init-method (.GetMethod init-type "Init")
                  program-type (emit/clr-type* gen-ctxt "Program")
                  main-method (emit/clr-method*
                               program-type
                               "Main" [:Public :Static] nil
                               [|System.String[]|])
                  ilg (.GetILGenerator main-method)
                  main-sym (symbol main)
                  main-ns (.Namespace main-sym)
                  main-name (.Name main-sym)
                  write-line-method (.GetMethod Console "WriteLine" (emit/make-typed-array Type [String]))
                  to-string-method (.GetMethod Object "ToString")
                  rt-type (.GetType clj-asm "clojure.lang.RT")
                  var-method (.GetMethod rt-type "var"
                                         (emit/make-typed-array Type [String String]))
                  seq-method (.GetMethod rt-type "seq")
                  var-type (.GetType clj-asm "clojure.lang.Var")
                  invoke-method (.GetMethod var-type "invoke"
                                            (emit/make-typed-array Type [Object]))
                  applyTo-method (.GetMethod var-type "applyTo")
                  symbol-type (.GetType clj-asm "clojure.lang.Symbol")
                  intern-method (.GetMethod symbol-type "intern"
                                            (emit/make-typed-array Type [String]))]
              (binding [emit/*ilgen* ilg]
                ;; Invoke __$Clj$Init$__.Init
                (op :Call init-method)

                ;; Call RT.var("clojure.core", "require")
                (op :Ldstr "clojure.core")
                (op :Ldstr "require")
                (op :Call var-method) ; 

                ;; Call Symbol.intern(main-ns)
                (op :Ldstr main-ns)
                (op :Call intern-method) ; 

                ;; Call invoke method of clojure.core/require var
                ;; on main-ns symbol
                (op :Callvirt invoke-method)
                (op :Pop)

                ;; Call RT.var(main-ns, main-name)
                (op :Ldstr main-ns)
                (op :Ldstr main-name)
                (op :Call var-method) 
                
                ;; Call RT.seq on first argument
                (op :Ldarg_0)
                (op :Call seq-method) ; 

                ;; Call applyTo method of main function on args
                (op :Callvirt applyTo-method)
                (op :Pop)

                ;; Return
                (op :Ret)
                (.CreateType program-type)
                (.SetEntryPoint (.. gen-ctxt AssemblyGen AssemblyBuilder)
                                main-method
                                (case target
                                  :console PEFileKinds/ConsoleApplication
                                  :windows PEFileKinds/WindowApplication
                                  PEFileKinds/Dll))))
            (println "Saving" (str name ext)))
          (.Invoke (.GetMethod GenContext "SaveAssembly" (enum-or BindingFlags/Instance BindingFlags/NonPublic)) gen-ctxt nil)
          (let [copied-refs (atom #{})
                asm-builder (.. gen-ctxt AssemblyGen AssemblyBuilder)]
            (doseq [[name {:keys [path asm asm-name]}] computed-dependencies]
              (when-not (contains? @copied-refs asm-name)
                (let [new-path (Path/Combine *compile-path* name)]
                  (when-not (paths-equal path new-path)
                    (File/Copy path new-path true)
                    (swap! copied-refs conj asm-name)
                    (doseq [refasm (.GetReferencedAssemblies asm)]
                      (copy-assembly-references asm-name copied-refs))
                    name))))
            (doseq [asm-name (.GetReferencedAssemblies asm-builder)]
              (copy-assembly-references asm-name copied-refs))
            (when-not (empty? @copied-refs)
              (println "Copied" (str/join ", " (map #(.Name %) @copied-refs)) "to output directory"))))))
     ; Output file name
    ))

(defn create-clj-appdomain [path]
  (let [app-domain-setup
        (AppDomainSetup.)]
    (set! (.ApplicationBase app-domain-setup) path)
    (let [app-domain (AppDomain/CreateDomain
                      "NLein-subdomain"
                      nil app-domain-setup)
          proxy (.CreateInstanceFromAndUnwrap app-domain
                                              (Path/Combine path "Clojure.dll")
                                              "clojure.lang.Hosting.ClojureProxy")]
      {:app-domain app-domain :proxy proxy})))

(defn rep [{:keys [proxy]} obj]
  (.ReadEvalPrint proxy obj))

(defn print-help []
  (println "nleiningen is a tool for working with ClojureCLR projects.")
  (println)
  (println "Available tasks are:")
  (println "compile\tCompile Clojure source files into a CLR assembly")
  (println "repl\tStart a repl"))

(defn args-error [args]
  (if (empty? args)
    (print-help)
    (println "Don't know how to execute" args)))

(defn start-repl [args]
  (require 'clojure.main)
  (bootstrap-project)
  (apply clojure.main/main args))
