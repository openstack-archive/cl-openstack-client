#!/bin/sh
export HOME=$PWD/.test-env
mkdir $HOME
cd $HOME
wget -q http://beta.quicklisp.org/quicklisp.lisp -O quicklisp.lisp
sbcl --load ../update-deps.lisp
sbcl --load ../run-tests.lisp
