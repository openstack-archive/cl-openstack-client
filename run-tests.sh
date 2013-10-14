#!/bin/sh
export HOME=$PWD/.test-env
mkdir $HOME
cd $HOME
wget -q http://beta.quicklisp.org/quicklisp.lisp -O quicklisp.lisp
ln -s $PWD ~/quicklisp/cl-openstack-client
sbcl --script ../update-deps.lisp
sbcl --script ../run-tests.lisp
