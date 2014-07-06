
.PHONY: allclean clean ../lib test all_test_results

################################################################
#### install

HOME=$(shell readlink -f ./)
head=$(shell git symbolic-ref HEAD --short)

shunit2=shunit2-2.1.6

all: cimtest/lib cimtest/scripts cimtest/test $(shell ls cimtest/init*)

cimtest:
	mkdir -p cimtest
	HOME=$(HOME) CIM_HOME=$(HOME)/cimtest CIM_INSTALL_BRANCH=$(head) /bin/sh ../scripts/cim_installer git ../

../lib:
	rm -f ../lib/script.lisp
	cd ../lib ; ./build.sh

cimtest/%: ../% cimtest
	rm -rf $@
	cp -rf $< cimtest/

$(shunit2).tgz:
	wget https://shunit2.googlecode.com/files/$(shunit2).tgz -O $(shunit2).tgz

$(shunit2): $(shunit2).tgz
	tar xf $(shunit2).tgz
	touch $(shunit2)

shunit2: $(shunit2)
	ln -fs $(shunit2) shunit2

loader: shunit2
	ln -fs shunit2/src/shunit2 loader

allclean:
	rm -rf .bashrc cimtest shunit2* loader *.out 

################################################################
#### test

#  $(IMPL) might be ""

ifndef IMPL
	IMPL=sbcl-system
endif

$(info $(IMPL))

test: all loader all_test_results

all_test_results: testCL.out testInstallQuicklisp.out

test%.out: test%.sh run-test.sh loader all
	./run-test.sh $< $(IMPL) | tee $@

clean:
	rm -rf .cache *.out

################################################################
#### alter the testing implementation

sbcl:
	IMPL=sbcl $(MAKE) test
ccl:
	IMPL=ccl $(MAKE) test
clisp:
	IMPL=clisp $(MAKE) test
abcl:
	IMPL=abcl $(MAKE) test
ecl:
	IMPL=ecl $(MAKE) test

all_impls: sbcl ccl clisp abcl ecl
