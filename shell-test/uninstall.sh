#!/bin/bash

sed -i.old -e 's/CIM_HOME.*$//g' ~/.bashrc
rm -rf cimtest
