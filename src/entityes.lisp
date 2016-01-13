;; [[file:doc.org::*Сущности][entity_and_automates]]
;;;; Copyright © 2014-2015 Glukhov Mikhail. All rights reserved.
;;;; Licensed under the GNU AGPLv3

(define-automat checkpoint "Автомат стойки"
  ((id serial)
   (name varchar))
  (:fin :goon :init :dialog :finding :standby :poweron :poweroff)
  ((:poweroff :poweron :power-on)
   (:poweron :standby :poweron-to-standby)
   (:standby :finding :standby-to-finding)
   (:finding :dialog :finding-to-dialog)
   (:dialog :init :dialog-to-init)
   (:init :goon :init-to-goon)
   (:goon :fin :goon-to-fin)))


(defun power-on ()
  "poweroff -> poweron")
;; entity_and_automates ends here
