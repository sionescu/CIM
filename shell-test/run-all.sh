#!/bin/bash

./setup.sh
source ~/.bashrc
export CIM_HOME
./test.sh
./testInstallQuicklisp.sh
