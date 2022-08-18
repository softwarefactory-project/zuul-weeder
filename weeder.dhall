{ roots =
  [ -- This might be useful in the future
    "ZuulWeeder.Graph.findReachable"
  , "ZuulWeeder.Prelude..*JSON"
  , -- Used for doc rendering
    "ZuulWeeder.UI.Dot..*"
  , "ZuulWeeder.Prelude.writeFileText"
  , -- The entrypoints
    "ZuulWeeder.main"
  , "ZuulWeeder.runDemo"
  , "Main.main"
  , "^Paths_.*"
  ]
, type-class-roots = True
}
