{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "game-of-life"
, dependencies = [ "aff", "console", "effect", "matrices", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
