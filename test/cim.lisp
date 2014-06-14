(in-package :cim.test)
(in-suite :cim)

; trailing slash and slash in the beginning

; xxx..xx/ + /yyy/yy... 
; xxx..xx/ + ttt/tt...  
; xxx..xx + /yyy/yy...  
; xxx..xx + ttt/tt...   

(test x_home
  (is (scan "//" "/some/path//some/remaining/path"))
  (is (not (scan "//" (cim_home "/aaa"))))
  (is (not (scan "//" (cim_home "aaa"))))
  (is (not (scan "//" (ql_home "/aaa"))))
  (is (not (scan "//" (ql_home "aaa")))))
