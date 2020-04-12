{ name = "game-of-life"
, dependencies = [ "aff", "console", "effect", "matrices", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
