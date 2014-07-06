#!/bin/bash

source .bashrc
export CIM_HOME
HOME=$(readlink -f ./) bash $1
