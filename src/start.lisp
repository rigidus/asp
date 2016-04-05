;; [[file:doc.org::*Точка входа][enter_point]]
;;;; Copyright © 2014-2015 Glukhov Mikhail. All rights reserved.
;;;; Licensed under the GNU AGPLv3
;;;; start.lisp

(in-package #:asp)

(defclass asp-render () ())

(defun start-server ()
  ;; start
  (restas:start '#:asp :port 3999)
  (restas:debug-mode-on)
  ;; (restas:debugg-mode-off)
  (setf hunchentoot:*catch-errors-p* t)
  ;; (make-event :name "restart"
  ;;             :tag "restart"
  ;;             :msg (format nil "Сервер перезапущен")
  ;;             :author-id 0
  ;;             :ts-create (get-universal-time))
  )

(start-server)

;; Тесты

;; Тестируем asp
(defun asp-test ()
  (in-package #:asp)
  
  (let ((checkpoint (car (find-checkpoint :name "1"))))
    (when (null checkpoint)
      (setf checkpoint
            (make-checkpoint :name "1" :state ":SELFTEST")))
    (takt checkpoint :selftest)
    )
  
  (dbg "passed: asp-test~%"))
(asp-test)
;; enter_point ends here
