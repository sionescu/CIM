# CIM -- Common Lisp Implementation Manager

* https://github.com/KeenS/CIM

## DESCRIPTION

CIM aims to be a tool which allows you to easily install, manage,
run REPL with, and execute with multiple Common Lisp implementation and systems.

I hope CIM could be to Lisp what RVM is to ruby.

## INSTALL

CIM has installer. Run command below and you get cim installed to ~/.cim.

```
$ curl https://raw.github.com/KeenS/CIM/master/scripts/cim_installer | /bin/sh
```
To change install path, set `CIM_HOME`.

```
$ curl https://raw.github.com/KeenS/CIM/master/scripts/cim_installer | CIM_HOME=/path/to/cim /bin/sh
```

## USAGE

Most of commands are designed refering to rvm.

`cim help <command>` or `cim list <command>` will also help you.

### Installing lisp implementation(s)
Use `cim install`.

```
$ cim install sbcl
$ cim install clisp abcl-1.2.1
```
As you can see, you can install the latest version or specified version if given.
### Selecting a lisp implementation
Use `cim use`.

```
$ cim use ccl
```
After this command, `cl` uses the latest ccl as backend.

Or to use it as default,
```
$ cim use ccl --default
```
Of cause, you can use system installed lisp.
```
$ cim use clisp-system
```

This also affects bare lisp command.
```
$ sbcl --version
SBCL 1.1.14
$ cim use sbcl-1.1.10
$ sbcl --version
SBCL 1.1.10
```

### Executing a lisp file
Use `cl`.

```
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

-C DIR          set *default-pathname-defaults* DIR.
-d, --debug     set debugging flags (push :debug into *features*)
-e, --eval SEXP one line of script. Several -e's are allowed. Omit [programfile]
-f, --load FILE load the FILE
-i EXT          edit *argv* files in place and make backup with the extension .EXT
-l LIBRARY      quickload the LIBRARY
-L LIBRARY      quickload and use-package the LIBRARY
-r, --repl      run repl
-q, --no-init   do not load $CIM_HOME/init.lisp
--no-rl         do not use rlwrap. This is effective only when --repl is specified
--no-right      do not display right prompt. This is effective only when --repl is specified
--no-color      do not use color. This is effective only when --repl is specified
-h, --help      print this help
-v, --version   print the version

If neither programfile, -e (--eval) nor -r (--repl) are specified, cl reads scripts from the standard input and then eval them.
```

### Running REPL
Use `cl` with `--repl` or `-r` in short.

```
$ cl --repl
CL-USER> (format t "Hi~%")
Hi
;=> NIL
```

if `rlwrap` is installed, `cl` use it.

### Executing one liner
Use `cl` with `-e`.

```
$ cl -e '(format t "Hello from command line~%")'
Hello from command line
```
`-e` can be specified many times. Those expressions are concatenated.
```
$ cl -e '(format t "Hello, ' -e $USER -e '")'
Hello, kim
```

### Executing with multiple implemetations
Use `cim for <impl>... do <args>`.
The `<args>` take the same format as `cl`.

```
$ cim for clisp sbcl do --no-init -e '(labels ((fib (n) (if (> 1 n) 1 (+ (fib (- n 1)) (fib (- n 2)))))) (time (fib 39)))'
>>>clisp --no-init -e (labels ((fib (n) (if (> 1 n) 1 (+ (fib (- n 1)) (fib (- n 2)))))) (time (fib 39)))
Real time: 542.5009 sec.
Run time: 541.5862 sec.
Space: 0 Bytes
<<<
>>>sbcl --no-init -e (labels ((fib (n) (if (> 1 n) 1 (+ (fib (- n 1)) (fib (- n 2)))))) (time (fib 39)))
Evaluation took:
  8.957 seconds of real time
  8.953758 seconds of total run time (8.949651 user, 0.004107 system)
  99.97% CPU
  21,447,269,828 processor cycles
  0 bytes consed
  
<<<
```


### Managing lisp systems
Use `ql`.

```
$ ql install alexandria # alias of quickload
$ ql search xml         # alias of system-apropos
$ ql update             # alias of update-all-dists
$ ql update client      # update quicklisp itself.
$ ql list remote

<many systems>

$ ql list local
alexandria
$ ql deps --path ./quicklisp myapp.asd
# myapp dependencies are installed to ./quicklisp.
```

### Upgrading cim itself
Use `cim get`.
Currently, `cim get` fetch `master` branch.
```
$ cim get
```

## License
BSD

## Author
Sunrim KIM
Eitarow FUKAMACHI
Syohei YOSHIDA
