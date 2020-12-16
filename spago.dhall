{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arraybuffer"
  , "base58"
  , "numbers"
  , "bigints"
  , "console"
  , "effect"
  , "integers"
  , "numbers"
  , "lists"
  , "node-buffer"
  , "psci-support"
  , "rationals"
  , "spec"
  , "b64"
  , "arraybuffer-types"
  , "purescript-erlps-core"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
