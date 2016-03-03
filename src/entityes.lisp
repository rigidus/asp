;; [[file:doc.org::*Сущности][entity_and_automates]]
;;;; Copyright © 2014-2015 Glukhov Mikhail. All rights reserved.
;;;; Licensed under the GNU AGPLv3
(in-package #:asp)

(define-automat checkpoint "Автомат стойки"
  ((id serial)
   (name varchar))
  (:fin :goon :init :dialog :finding :standby :lock :selftest)
  ((:selftest :lock :selftest-to-lock)
   (:selftest :standby :selftest-to-standby)
   (:standby :lock :standby-to-lock)
   (:standby :finding :standby-to-finding)
   (:finding :lock :finding-to-lock)
   (:finding :dialog :finding-to-dialog)
   (:dialog :lock :dialog-to-lock)
   (:dialog :init :dialog-to-init)
   (:init :lock :init-to-lock)
   (:init :goon :init-to-goon)
   (:goon :lock :goon-to-lock)
   (:goon :fin :goon-to-fin)))


(defun power-on ()
  "selftest -> poweron")
;; entity_and_automates ends here
