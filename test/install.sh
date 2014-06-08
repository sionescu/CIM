#!/bin/bash


mkdir -p cimtest
dir=$(readlink -f cimtest)
head=$(git symbolic-ref HEAD --short)

CIM_HOME=$dir CIM_INSTALL_BRANCH=$head /bin/sh ../scripts/cim_installer



