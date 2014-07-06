#!/bin/bash

source .bashrc
export CIM_HOME

IMPL=${2:-sbcl-system}

cim install $IMPL
cim use $IMPL

HOME=$(readlink -f ./) bash $1
