#!/bin/csh
if ( ${?CIM_HOME} ) then
    setenv CIM_HOME $CIM_HOME
else
    setenv CIM_HOME `(cd \`dirname "$0"\` && pwd)`
endif
setenv PATH "$CIM_HOME/bin:$PATH"
setenv CIM_ID "$$"
setenv MANPATH "$CIM_HOME/share/man:$MANPATH"
sh "$CIM_HOME/init"
