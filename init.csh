#!/bin/csh
setenv CIM_HOME "`(cd \`dirname $0\`; pwd)`"
setenv PATH "$CIM_HOME/bin:$PATH"
setenv CIM_ID "$$"
setenv MANPATH "$CIM_HOME/share/man:$MANPATH"
sh "$CIM_HOME/init"
