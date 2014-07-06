#!/bin/bash

testDownloadQuicklisp(){

    . "$CIM_HOME/scripts/cim_utils"
    rm -rf tmp
    mkdir tmp
    cim_wget "http://beta.quicklisp.org/quicklisp.lisp" "tmp/quicklisp.lisp"
    assertTrue '[ -e tmp/quicklisp.lisp ]'
}

InstallQuicklisp(){
    CIM_HOME=cimtest
    . "$CIM_HOME/scripts/cim_utils"
    . "$CIM_HOME/scripts/cim_lib_install"

    rm -f "$CIM_HOME/lib/quicklisp.lisp"
    rm -rf "$CIM_HOME/quicklisp"
    __install_ql_if_need $1 $2
    assertTrue 'quicklisp.lisp download not successful' "[ -e $CIM_HOME/lib/quicklisp.lisp ]"
    assertTrue 'quicklisp direcrtory not successfully created' "[ -d $CIM_HOME/quicklisp ]"
    assertTrue 'setup.lisp not successfully created' "[ -e $CIM_HOME/quicklisp/setup.lisp ]"
}

testInstallQuicklisp(){
    InstallQuicklisp sbcl 1.2.0
    # InstallQuicklisp ccl 1.9
    # currently fails
}

# loading shunit2.
. loader
