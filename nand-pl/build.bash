#!/bin/bash

# function to check that all dependencies are installed
function check_installs { 
  DEPENDS=$@
 
  echo "Checking that dependencies of NAND are installed"  
  for DEPEND in $DEPENDS 
  do 
    if ! type $DEPEND > /dev/null ; then
      echo "It looks like you don't have "$DEPEND" installed-- aborting build." ;
      exit 1 ;
    fi 
  done 
  echo "All dependencies are installed" 
}

# function that builds nand in current directory 
function build_nand { 
  check_installs ocaml ocamlbuild ocamlyacc ocamllex

  # actual build 

  # location of source code 
  INCLUDE_FLAGS="-I src -I parsing" 

  echo "Starting build" 
  if   ocamlbuild main.native $INCLUDE_FLAGS ; then 
    echo "Build succeeded" 
  else 
    echo "Build failed-- compilation exited with code $?" 
  fi 
}

# function that installs nand by creating alias in .bashrc or .bash_profile, depending on OS 
function install_nand { 
  CUR_DIR=`pwd`
  NAND=$CUR_DIR/main.native 

  if [ ! -f $NAND ]; then 
    echo "NAND has not yet been built or cannot be found by script; aborting installation" 
    exit 1 
  fi 
   
  NAND_ALIAS="alias nand=$NAND" 
  OS=`uname`
 
  case $OS in 
    "Linux") 
      ALIAS_FILE=~/.bash_aliases
    ;; 
    "Darwin") 
      ALIAS_FILE=~/bash_profile
    ;; 
    *) 
      echo "Unrecognized operating system-- aborting installation" 
    ;;
  esac  


}
 
if [ $# -ne 1 ]; then 
  echo "Run ./build.bash with one of two arguments:" 
  echo "make-- builds NAND source code" 
  echo "install-- creates alias for nand executable" 
else 
  case $1 in
    make) 
      build_nand
    ;; 
    install) 
      install_nand 
    ;; 
    *) 
      echo "Unrecognized argument" 
    ;;
  esac 
fi    
