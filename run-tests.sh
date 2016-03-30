#!/bin/bash
set -e
set -x
export HOME=$PWD/.test-env
mkdir $HOME
cd $HOME
wget -q https://common-lisp.net/project/asdf/asdf.lisp -O asdf.lisp
wget -q http://beta.quicklisp.org/quicklisp.lisp -O quicklisp.lisp
sbcl --script ../update-deps.lisp
ln -s $PWD/.. ~/quicklisp/local-projects/cl-openstack-client
sbcl --script ../run-tests.lisp
