{ name = "my-project"
, dependencies =
  [ "arraybuffer"
  , "arraybuffer-types"
  , "b64"
  , "base58"
  , "bigints"
  , "console"
  , "effect"
  , "integers"
  , "lists"
  , "node-buffer"
  , "numbers"
  , "ordered-set"
  , "psci-support"
  , "purescript-erlps-core"
  , "rationals"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
