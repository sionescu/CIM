

CIM mainly consists of lisp parts and shell-script parts. Therefore
to see CIM work correctly you have to run tests on both parts.

To test the lisp part, put CIM in somewhere asdf recognizes and evaluate:

    (require :cim-test)

To test the shell-script part and its integration with lisp, run:

    cd shell-test; make all_impls

The test is exhaustive; it runs several tests on all implementations!

Enjoy!
