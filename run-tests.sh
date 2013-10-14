#!/bin/sh -x -e
export HOME=$PWD/.test-env
mkdir $HOME
cd $HOME
wget -q http://beta.quicklisp.org/quicklisp.lisp -O quicklisp.lisp
sbcl --script ../update-deps.lisp
ls $HOME
echo $HOME
ln -s $PWD/.. ~/quicklisp/local-projects/cl-openstack-client
sbcl --script ../run-tests.lisp
