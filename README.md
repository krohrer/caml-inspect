OCaml Inspect - caml-inspect.
======================================================================

> This library is free software; you can redistribute it and/or
> modify it under the terms of the GNU Lesser General Public
> License as published by the Free Software Foundation; either
> version 2.1 of the License, or (at your option) any later version,,
> with the special exception on linking described in file LICENSE.

> This library is distributed in the hope that it will be useful,
> but WITHOUT ANY WARRANTY; without even the implied warranty of
> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
> Lesser General Public License for more details.
 
> You should have received a copy of the GNU Lesser General Public
> License along with this library; if not, write to the Free Software
> Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

About
-----

Inspect is a small library to inspect arbitrary OCaml values and their
associated object graph by either dumping them as S-expressions (with
sharing and references), or by writing output in the DOT-language
which can then be further processed by [Graphviz][]. In a way, Inspect
provides a window into the OCaml runtime system, a view behind the
curtains.

[Caml-Inspect][] was originally written by Kaspar M. Rohrer (<kaspar.rohrer@gmail.com>).

Installation
------------

Unzip or untar in any directory, then run

    make

to generate the library and documentation.
To install the library using findlib, simply type

    make install

And to uninstall it

    make uninstall

For development, you may instead run

    sudo ln -s `pwd` `ocamlfind printconf path`/inspect

Usage
-----

If you have findlib installed, using the library is as simple as
typing

    #use "topfind";;
    #require "inspect";;

into your OCaml prompt. I suggest you open the Inspect module as well.

    open Inspect;;

For starters, both the Dot and the Sexpr library provide a test_data
function to generate some interesting data to dump.

    Sexpr.dump (Sexpr.test_data ());;
    Dot.dump (Dot.test_data ());;

It is naturally also possible to let the dump functions inspect
themselves:

    Sexpr.dump Sexpr.dump;;
    Dot.dump Dot.dump;;

If you are on a Mac, the Inspect.Dot.dump_osx function should be of
interest. It writes the DOT output to a temporary file, uses Graphviz
to generate the graph, and displays the results using the open
command.

    Dot.dump_osx Dot.dump_osx;;

It goes without saying that you should have [Graphviz][] installed for
this last part to work.

Representation of OCaml Values
------------------------------

OCaml values all share a common low-level representation. In contrast
to dynamically typed languages, it is usually not possible to infer
the type of a value from the low-level representation, because several
distinct OCaml types can share the same representation. This is not
really an issue, because OCaml is statically typed after all. So
except for some trickery with `Obj.magic`, it is simply not possible
to apply a function to a value of the wrong type.

### The Value Type

The basic building block that is used by the runtime-system (which is
written in the C programming language) to represent any value in the
OCaml universe is the value type. Values are always word-sized. A word
is either 32 or 64 bits wide, depending on the architecture (see
`Sys.word_size`).

A value can either be a pointer to a block of values in the OCaml
heap, a pointer to an object outside of the heap, or an unboxed
integer. Naturally, blocks in the heap are garbage-collected.

To distinguish between unboxed integers and pointers, the system uses
the least-significant bit of the value as a flag. If the LSB is set,
the value is unboxed. If the LSB is cleared, the value is a pointer to
some other region of memory. This encoding also explains why the int
type in OCaml is only 31 bits wide (63 bits wide on 64 bit platforms).

Because blocks in the heap are garbage-collected, they have strict
structure constraints. Information like the tag of a block and its
size (in words) is encoded in the header of each block.

There are two categories of blocks with respect to the garbage collector:

- **Structured blocks** may only contain well-formed values, as they are
  recursively traversed by the garbage collector.
- **Raw blocks** are not scanned by the garbage collector, and can thus
  contain arbitrary values.

Structured blocks have tag values lower than `Obj.no_scan_tag`, while
raw blocks have tags equal or greater than `Obj.no_scan_tag`.

### Heap Blocks

The chapter on *Interfacing C with Objective Caml* in the [OCaml
manual][OCamlManual] describes the following types of blocks. The type
of a block is its tag, which is stored in the block header. (see
`Obj.tag`).

- `0` to `Obj.no_scan_tag-1`, A structured block (an array of Caml
objects). Each field is a value.
- `Obj.closure_tag`: A closure representing a functional value. The
first word is a pointer to a piece of code, the remaining words are
values containing the environment.
- `Obj.string_tag`: A character string.
- `Obj.double_tag`: A double-precision floating-point number.
- `Obj.double_array_tag`: An array or record of double-precision
floating-point numbers.
- `Obj.abstract_tag`: A block representing an abstract datatype.
- `Obj.custom_tag`: A block representing an abstract datatype with
user-defined finalization, comparison, hashing, serialization and
deserialization functions atttached.

There are a few more structured block types which are not directly
described in the [manual][OCamlManual].

- `Obj.object_tag`: A structured block representing an object. The first
  field is a value that describes the class of the object. The second
  field is a unique object id (see `Oo.id`). The rest of the block
  represents the variables of the object.
- `Obj.lazy_tag`, `Obj.forward_tag`: These two block types are used by the
runtime-system to implement lazy-evaulation.
- `Obj.infix_tag`: A special block contained within a closure block. 

### Summary

This section is only a summary of the most important things.  The
chapter on *Interfacing C with Objective Caml* in the [OCaml
manual][OCamlManual] gives a much better explanation over the
translation of OCaml types to their actual representation.

+ **Atomic types**
  + `int`: Unboxed integer values.
  + `char`: Unboxed integer values (ASCII code).
  + `float`: Blocks with tag `Obj.double_tag`.
  + `string`: Blocks with tag `Obj.string_tag`.
  + `int32`/`int64`/`nativeint`: Blocks with `Obj.custom_tag`.
+ **Tuples and records**: Blocks with tag 0.
+ **Arrays**: Blocks with tag 0.
+ **Arrays and records of floats**: Blocks with tag
`Obj.double_array_tag`.
+ **Concrete types**
    + *Constant constructors*: Represented by unboxed integers, first
    declared constant constructor is 0, second declared constant
    constructor is 1, and so on.
    + *Non-constant constructors*: Blocks with a tag lower than
    `Obj.no_scan_tag` that encodes the constructor, numbered in order
    of declaration, starting at 0.
+ **Objects**: Blocks with tag `Obj.object_tag`. The first field
refers to the class of the object and its associated method
suite. The second field contains a unique object ID. The remaining
fields are the instance variables of the object.
+ **Variants**: Variants are similar to constructed terms. There are a
few differences however.
    + Variant constructors are identified by their hash value.
    + Non-constant variant constructors are not flattened. They are
    always blocks of size 2, where the first field is the hash. The
    second field can either contain a single value, or a pointer to
    another structured block (just like a tuple).

If in doubt, dump it out.

References
----------

* [OCaml 3.11 Manual][OCamlManual] - The Objective Caml system (release 3.11), Documentation and user's manual
* [Graphviz][] - Graph Visualization Software
* [Caml-Inspect][] - Source code repository for OCaml Inspect

[OCamlManual]: http://caml.inria.fr/pub/docs/manual-ocaml/ "OCaml Manual"
[Graphviz]: http://www.graphviz.org/ "Graphviz - Graph Visualization Software"
[Caml-Inspect]: http://github.com/krohrer/caml-inspect "Source code repository"