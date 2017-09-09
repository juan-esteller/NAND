# NAND\* Programming languages

Ocaml implementation of the NAND, NAND++ and NAND<< programming languages from
[http://www.introtcs.org](http://www.introtcs.org/public/index.html).
See in particular the following [appendix](http://www.introtcs.org/public/lec_A_NAND_prog_lang.html) for the languages' specifications.

See [this website](http://chess-master-doubt-60650.netlify.com/) for a web interface of this implementation.

## To compile on OS X and Linux:

* Clone the repository-- we recommend you pull frequently, so that you can have the latest version.
* Once in the root directory of the repository, enter the NAND source with the command `cd nand-pl`
* Then run the script `build.bash` with the command `./build.bash`
* If the compilation is successful, it will produce the executable `main.native`
* For your convenience, we recommend [adding an alias](http://www.hostingadvice.com/how-to/set-command-aliases-linuxubuntudebian/) to your `.bashrc` or `.bash_profile` by adding `alias nand=<absolute path to main.native>`. That way, you don't have to perform all of your development in the same directory in the build. The rest of the README assumes you have this alias; if you don't, then `./main.native` will substitute for `nand`

## To run your .nand programs: 
* All NAND\* programs must have a filename that ends with `.nand` 
* To run a program, type `nand [flags] <path/to/prog.nand> <binary input>` 
* The program will run, printing its trace and eventual output to stdout 
* The recognized flags are as follows:
	* `-pp`, `-ll`: run the NAND++ and NAND<< interpreters, respectively. NAND is the default.
	* `-addSS`: enables syntactic sugar for the program.  
	* `-s`: silences the execution trace. 
	* `-dry`: outputs the source of the program that would have been executed to stdout. 
    
	
## Dependencies

In order to compile this code, you will need OCaml, ocamlbuild, ocamllex, and ocamlyacc installed on your computer. Installation instructions can be found on the following website: https://ocaml.org/docs/install.html. The build.bash script for OS X and Linux checks for the presence of these program, and will abort the build if they are not present.  
