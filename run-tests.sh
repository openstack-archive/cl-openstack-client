#!/bin/bash
set -e
set -x
export HOME=$PWD/.test-env
[ -d $HOME ] || mkdir $HOME
cd $HOME
export CIM_HOME=$HOME
if [ ! -d $HOME ]; then
    curl -L https://raw.github.com/KeenS/CIM/master/scripts/cim_installer | /bin/sh
fi
source "$CIM_HOME/init.sh"
cim install sbcl || true
cim use sbcl
sbcl --script ../update-deps.lisp
if [ ! -L ~/quicklisp/local-projects/cl-openstack-client ]; then
    ln -s $PWD/.. ~/quicklisp/local-projects/cl-openstack-client
fi
sbcl --script ../run-tests.lisp
