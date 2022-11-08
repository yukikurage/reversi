{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "arrays"
  , "console"
  , "control"
  , "datetime"
  , "debug"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "node-path"
  , "now"
  , "numbers"
  , "partial"
  , "prelude"
  , "random"
  , "simple-json"
  , "st"
  , "strings"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, backend = "purs-backend-es build"
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
