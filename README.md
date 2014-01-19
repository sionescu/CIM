# CIM -- Common Lisp Implementation Manager

* https://github.com/KeenS/CIM

## DESCRIPTION

__CAUTION__
CIM is under eager development. Most of features does not work yet.
Please wait for release version or contribute to accelerate development.

CIM aims to be a tool which allows you to easily install, manage,
run REPL with, and execute with multiple Common Lisp implementation and systems.

I hope CIM could be to Lisp what RVM is to ruby.

## USAGE

Most of commands are designed refering to rvm. Syset is the counterpart of gemset.

### Installing Lisp Impl(s)

```
$ cim install sbcl
$ cim install clisp abcl-1.2.1
```

### Selecting A Lisp impl

```
$ cim use ccl
$ cim use ccl --default
```

### Running REPL

```
$ cl --repl
CL-USER>
```

if `rlwrap` is installed, `cl` use it.

### Executing Lisp file

```sh
$ cat hello.lisp
#!/usr/bin/env cl
(format t "Hello, CIM")
$ cl hello.lisp
Hello, CIM
$ chmod +x hello.lisp
$ ./hello.lisp
Hello, CIM
$ cl -h     # most of options are import of ruby's.
Usage: cl [switchs] [--] [programfile] [argumensts]
 -C DIR			set *default-pathname-defaults* DIR, before executing your script
 -d, --debug		set debugging flags (push :debug into *features*)
 -e, --eval SEXP	one line of script. Several -e's allowed. Omit [programfile]
 -f, --load file	load the file
 -h, --help		print this help
 -i[extxntion]		edit *argv* files in place and make backup with the extension .EXT
 -l library		quickload the library
 -L library 		quickload and use-package the library
 -r, --repl		run repl
 -q, --no-init		do not load ~/.lisprc
     --no-rl		do not use rlwrap
     --no-right		do not display right prompt. This is effective only --repl is specified
     --no-color         do not use color. This is effective only --repl is specified
 -v, --version		print the version
if neither programfile, -e(--eval) nor -r(--repl) are specified, cl reads scripts from the standard input and then eval them.
```
	
### Executing sexp

```sh
$ cl -e '(format t "Hello from command line")'
Hello from command line
```

### Managing lisp systems

```sh
$ ql install alexandria # alias of quickload
$ ql search xml         # alias of system-apropos
$ ql update             # alias of update-all-dists
$ ql update integral    # update system `integral`
$ ql update-client
$ ql list remote

<many systems>

$ ql list local
alexandria
$ ql deps --path ./quicklisp myapp.asd
$ ql list local --path ./quicklisp

<myapp dependencies>

```

## License
See LICENSE file.

## Author
κeen(@blackenedgold)
