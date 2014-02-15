#!/bin/csh
if ( ${?CIM_HOME} ) then
    setenv CIM_HOME "$CIM_HOME"
else
    setenv CIM_HOME "$HOME/.cim"
endif
setenv PATH "$CIM_HOME/bin:$PATH"
setenv CIM_ID "$$"
setenv MANPATH "$CIM_HOME/share/man:$MANPATH"
sh "$CIM_HOME/init"
