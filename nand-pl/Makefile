MAIN=main.native

INCLUDE_FLAGS=-I parsing -I src

$(MAIN): clean 
    ocamlbuild INCLUDE_FLAGS $(MAIN) 

clean: rm -rf main.native
