#!/bin/bash

sed -i.old -e 's/CIM_HOME.*$//g' ~/.bashrc
rm -rf cimtest
rm -rf shunit2*
rm -f loader
