# Haskompiler
This is a (work-in-progress) compiler for the [Civic](https://staff.fnwi.uva.nl/c.u.grelck/teaching/docs/civic_2020.pdf) model programming language.

I'm writing it as an exercise to deepen my Haskell knowledge, and to show that higher-level languages such as Haskell can be just as or more suited to a course on compiler construction than C, the language that was used in the compiler construction course I followed (that Civic was created for).

Currently, the project is still in early stages. The lexer and parser are mostly finished (some constructs are not yet implemented, but could be added without too much effort), but the traversals are still very sparse as I'm still designing and evaluating the traversal framework (as of now, just parts of the semantic analysis component work).

Documentation is also still _very_ sparse &mdash; I'm working on improving it.

## Components
The compiler is currently structured into three main components: an [Alex](https://www.haskell.org/alex/)-based lexer (`lexer/Tokens.x`), a [Megaparsec](https://github.com/mrkkrp/megaparsec/)-based parser (`src/Parser/`), and a set of AST traversals (`src/Traversal.hs` for common traversal code, `src/Traversals/` for the individual traversals).

## Building and running
`Setup.hs` will automatically generate the lexer, but it requires `alex` to be on your executable path. Aside from that dependency, everything should be resolved automatically when building via Stack.

To build the compiler, simply run `stack build` anywhere in the project tree.