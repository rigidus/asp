;; [[file:doc.org::*Точка входа][enter_point]]
;;;; Copyright © 2014-2015 Glukhov Mikhail. All rights reserved.
;;;; Licensed under the GNU AGPLv3
;;;; start.lisp

(in-package #:asp)

(defun main ()
  ;; start
  (restas:start '#:asp :port 3999)
  (restas:debug-mode-on)
  ;; (restas:debugg-mode-off)
  (setf hunchentoot:*catch-errors-p* t)
  (make-event :name "restart"
              :tag "restart"
              :msg (format nil "Сервер перезапущен")
              :author-id 0
              :ts-create (get-universal-time)))

(main)

;; Тесты

;; Тестируем asp
(defun asp-test ()
  (make-checkpoint :name "1" :state ":POWEROFF")
  
  
  (dbg "passed: asp-test~%"))
(asp-test)
;; enter_point ends here
