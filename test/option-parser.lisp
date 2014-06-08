
(in-package :cim.test)
(def-suite :cim)
(in-suite :cim)

(test short-opt-p
  (is (short-opt-p "-h"))
  (is (not (short-opt-p "--help")))
  (is (short-opt-p "-a"))
  (is (not (short-opt-p "-avl")))
  (is (not (short-opt-p "-"))))


(test long-opt-p
  (is (long-opt-p "--help"))
  (is (not (long-opt-p "-a")))
  (is (not (long-opt-p "-avl")))
  (is (not (long-opt-p "--"))))







