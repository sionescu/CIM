# CIM -- Common Lisp Implementation Manager

* https://github.com/KeenS/CIM

## DESCRIPTION

__CAUTION__
CIM is under eager development. It does not work yet.
Please wait for release version or contribute to accelerate development.

CIM aims to be a tool which allows you to easily install, manage,
run REPL with, and execute with multiple Common Lisp implementation and systems.

I hope CIM could be to Lisp what RVM is to ruby.

## USAGE

Most of commands are designed refering to rvm. Syset is the counterpart of gemset.

### Installing Lisp Impl(s)

```sh
$ cim install sbcl
$ cim install clisp abcl-1.2.1
```

### Selecting A Lisp impl

```sh
$ cim use ccl
$ cim use ccl --default
```

### Running REPL

```
$ cl
CL-USER>
```

I hope to enable readline in REPL.

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
Usage: cl [OPTIONS]  [--] *argv*
            # ($ ..) macros may be separated from CIM. See also [https://gist.github.com/KeenS/7059301]
  -a          autosplit mode with -n or -p (splits ($ _) into ($ F))
  -C  =DIR    set *default-pathname-defaults* DIR, before executing your
                 script
  -d          set debugging flags (push :debug into *features*)
  -e  =SCRIPT one line of script. Several -e's allowed. Omit
                 [programfile]
  -F  =PATTERN  split pattern for autosplit (-a)
  -i  =EXT    edit *argv* files in place and make backup with extension
                 .EXT
  -I  =DIRS   push DIRS asdf:*central-registry* directories separated by
                 ":"
  -l          enable line ending processing
  -n          assume '(loop while (setf ($ _) (readline)) do ...)'
                 around your script
  -p          assume loop like -n but print line also like sed
  -S          look for the script using PATH environment variable
  -h, --help  Prints this summary
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
$ ql update
$ ql upgrade
$ ql update-client
$ ql list remote

<many systems>

$ ql list local
alexandria

$ ql syset use foo
Using syset "foo"
$ ql list local
none

$ ql deps myapp.asd --path ./quicklisp
```

## License
See LICENSE file.

## Author
κeen(@blackenedgold)
