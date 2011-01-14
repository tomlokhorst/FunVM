Compiler Architecture
=====================

Some thoughts on an extendible, generic compiler architecture.

Pipeline (stop, or start, at any point):
  - Lexing/parsing
  - Type checking
  - Static analyses
  - AST transformations
  - Code generation
  - Linking (with link-time transformations)

Options (commandline arguments) handling:
  - Generic: `--verbose, --parse, --typecheck`
  - Specific for module:
    `--transformations "DefinitionSiteArityRaising=on InlineWrappers=off"`
  - Modules can specify default options (that can be overridden by commandline):
    (ArityRaising enabled InlineWrappers by default)

File loading (and intermediate representations like .hi or .obj):
  - Modules can request files (of different types)
     - Type checking of Prelude requests Data.List (parse .hs or load .hi)
     - Optimizing transformation of Prelude requests analysis results
       for Data.List (.prof file)
     - Linking of Prelude requests Data.List (codegen .hs or load .obj)
  - Modules can produce one or more files
     - Type checking produces Prelude.hi
     - Static analysis produces Prelude.prof
     - AST tranformtion updates Prelude.hi and Prelude.prof
     - Code generation produces Prelude.obj
     - Linking produces Program.exe
  
