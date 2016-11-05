meia-lua
----------------
A superset of Lua, made mostly for sake of experimentation.

Why?
--------
Ask not why, but why not!

I wanted to create a superset of Lua that contained some things I wanted to experiment with, like match expressions, destructuring, while still being 100% capable of ingesting normal Lua code, to make it as easy as possible to use in projects where I would normally use Lua, like working with LÖVE for example.

Another factor was the desire to write a nanopass compiler in **Racket**, inspired by [Andy Keep's talk on writing a Nanopass Compiler)](https://www.youtube.com/watch?v=Os7FE3J-U5Q), so.. here we are.

Whats different from Lua?
--------------------------------
Currently, nothing! :D

Version ``v0.1`` is intended to be a milestone where Lua ingested and put through the pipeline comes out as the same Lua on the other end, after which actual experimentation will resume.

What does it depend on?
--------------------------------
 * [Racket](http://racket-lang.org/)
   * [brag](http://docs.racket-lang.org/brag/)
   * [nanopass](https://docs.racket-lang.org/nanopass/)

What does it look like?
--------------------------------
Well, Lua; for now at least.

License
----------------
??? To be cleared up what I can actually license this under!
