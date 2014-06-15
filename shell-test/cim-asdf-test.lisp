#! /usr/bin/env cl --

(require :asdf)
(push *default-pathname-defaults*
      ql:*local-project-directories*)
(ql:quickload :cim-test)
