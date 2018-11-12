# hix

The [MIX architecture][mix] is a hypothetical computer used in Donald
Knuth's _The Art of Computer Programming_. This is an attempt to
implement an emulator for MIX in a highly typesafe style, using
type-level integers and size-tagged vectors to indicate whether a
given MIX processor is running in binary or decimal mode.

[mix]: https://en.wikipedia.org/wiki/MIX

Things that are done:

* Bytes, words, registers, the MIX CPU

Things that are not done:

* Input/output devices
* Reading instructions from disk
* Instruction decoding/execution
* Pretty much everything else

# License

Released under the [Solopsistic Public License][spl].

[spl]: https://github.com/matildah/SPL
