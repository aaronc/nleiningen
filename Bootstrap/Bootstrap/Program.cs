using System;
using System.Reflection;
using clojure.lang;

namespace Bootstrap
{
    public static class Program
    {
        private const string CLOJURE_LOAD_PATH = "CLOJURE_LOAD_PATH";
        private static readonly Symbol CLOJURE_MAIN = Symbol.intern("clojure.main");
        private static readonly Var REQUIRE = RT.var("clojure.core", "require");
        private static readonly Var LEGACY_REPL = RT.var("clojure.main", "legacy-repl");
        private static readonly Var LEGACY_SCRIPT = RT.var("clojure.main", "legacy-script");
        private static readonly Var MAIN = RT.var("clojure.main", "main");

        static void Main(string[] args)
        {
            Assembly.Load("NuGet.Core");
            Assembly.Load("Mono.Cecil");
            var cljLoadPath = Environment.GetEnvironmentVariable(CLOJURE_LOAD_PATH);
            cljLoadPath += @"ClojureClrEx\src;src";
            Environment.SetEnvironmentVariable(CLOJURE_LOAD_PATH, cljLoadPath);
            REQUIRE.invoke(CLOJURE_MAIN);
            MAIN.applyTo(RT.seq(args));
        }
    }
}
