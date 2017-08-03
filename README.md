# NAND\* Programming languages

Ocaml implementation of the NAND, NAND++ and NAND<< programming languages from
[http://www.introtcs.org](http://www.introtcs.org/public/index.html).
See in particular the following [appendix](http://www.introtcs.org/public/lec_A_NAND_prog_lang.html) for the languages' specifications.

## To use:

* Compile with `ocamlbuild main.native`
* To run a the prog `prog.nand` on some `<binary input>` such as `011` you type  `./main.native prog.nand <binary input>`
* The Python code referenced in the lecture notes appears in the `python` directory.

## Dependencies

In order to compile this code, you will need OCaml and ocamlbuild installed on your computer. Installation instructions can be found on the following website: https://ocaml.org/docs/install.html
