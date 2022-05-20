# Developper Guide

## Design decisions

Zuul Weeder is written using the [Haskell](https://haskell.org) Language.
Haskell provides a powerfull type system to safely manipulate and transform data.

The user interface is written using the [HTMX](https://htmx.org/) library.
HTMX provides access to modern browser features without using Javascript.

The project can be deployed with [nix](https://nixos.org) reproducible build.


## Main dependencies

These elements are essentials for the project.

### GHC2021

Automatically derive instances and use flexible contexs.

[Learn more about GHC2021](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0372-ghc-extensions.rst#motivation).


### OverloadedRecordDot and DuplicateRecordFields: record.field syntax

Better record ergonomics:

```haskell
data Job
  = Job {
    name : JobName,
    id : JobId
  }

data Node
 = Node {
   name : NodeName,
   path : FilePathT
 }

isSamePath :: Node -> Node -> Bool
isSamePath n1 n2 = n1.path == n2.path
```

Though HLS doesn't support auto completion [yet](https://github.com/haskell/haskell-language-server/issues/2732).


### [algebraic-graphs][algebraic-graphs]: Algebraic graph construction and transformation

The configuration is loaded into an algebraic graph that is used to query
the reachable dependencies between elements.
See the *ZuulWeeder.Graph.Analysis* data type.

[Learn more about algebraic-graphs](https://nobrakal.github.io/alga-tutorial/)


### [lucid][lucid]: Clear to write, read and edit DSL for HTML

The web interface is writen using a DSL.
See the *ZuulWeeder.UI* module.

[Learn more about lucid](https://chrisdone.com/posts/lucid/)


### [servant][servant]: A family of combinators for defining webservices APIs

The HTTP API is defined as a Type. See the *ZuulWeeder.UI.API* type.

[Learn more about servant](http://docs.servant.dev/en/stable/tutorial/index.html)


### [witch][witch]: Convert values from one type into another

Rust inspired ergonomic to convert types:

```haskell
textPack :: String -> Text
textPack = Witch.from

encodeText :: Text -> ByteString
encodeText = Witch.from
```


## Extra libraries

These libraries are helpful, but they could be removed if they really hurts maintenance.

### [lens][lens]: Lenses, Folds and Traversals

The lenses are defined using generic-lens and the OverloadedLabels extension.

[Learn more about lens](https://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html)


### [string-qq][string-qq] for multiline strings

Using the QuasiQuotes language extensions.


### Quality of life extensions

- BlockArgument: omit `$` before `do` block.
- LambdaCase: shorter pattern match, e.g. `f = \cases` instead of `f x = case x of`.
- TypeApplications: type variable specialization: `into @Int64`.
- NegativeLiterals: better syntax, `-1` (instead of `(-1)`).
- NamedFieldPuns: enable `X { result }` instead of `X { result = result }`.
- ImportQualifiedPost: enable imports alignments: `import Data.Text qualified`.


[algebraic-graphs]: https://hackage.haskell.org/package/algebraic-graphs-0.5
[lucid]: https://hackage.haskell.org/package/lucid
[servant]: https://hackage.haskell.org/package/servant
[witch]: https://hackage.haskell.org/package/witch
[lens]: https://hackage.haskell.org/package/lens
[string-qq]: https://hackage.haskell.org/package/string-qq
[qq-literals]: https://hackage.haskell.org/package/qq-literals
[streaming]: https://hackage.haskell.org/package/streaming


## Code Contribution

Checkout the haddock code documentation: [zuul-weeder](https://docs.softwarefactory-project.io/zuul-weeder/)

Run `nix develop` to setup the project dependencies (ghc, language server, linters, ...)

Auto reload the web ui with a demo config:

```
ghcid -W --test "ZuulWeeder.runDemo"
```

> After adding css class, run `nix run .#tailwind` to update the tailwind.css file. Then hard refresh the web page.

Validate your changes:

```
./bin/run-tests
```
