;; [[file:doc.org::*Каркас проекта][defsystem]]
;;;; Copyright © 2014-2015 Glukhov Mikhail. All rights reserved.
     ;;;; Licensed under the GNU AGPLv3
     ;;;; asp.asd

(asdf:defsystem #:asp
  :serial t
  :pathname "src"
  :depends-on (#:closer-mop
               #:postmodern
               #:cl-mysql
               #:anaphora
               #:cl-ppcre
               #:restas
               #:restas-directory-publisher
               #:closure-template
               #:cl-json
               #:cl-base64
               #:drakma
               #:split-sequence
               #:cl-html5-parser
               #:cl-who
               #:parenscript
               #:cl-fad
               #:optima
               #:fare-quasiquote-extras
               #:fare-quasiquote-optima
               )
  :description "asp"
  :author "Glukhov Mikhail"
  :version "0.0.3"
  :license "GNU AGPLv3"
  :components ((:file "package")    ;; файл пакетов
               (:static-file "templates.htm")
               (:file "prepare")    ;; подготовка к старту
               (:file "util")       ;; файл с утилитами
               (:file "globals")    ;; файл с глобальными определеями
               (:file "bricks")     ;; компоненты для создания интерфейсов
               ;; Модуль сущностей, автоматов и их тестов
               (:module "entity"
                        :serial t
                        :pathname "mod"
                        :components ((:file "entity")))
               (:file "entityes")   ;; Сущности и автоматы
               (:file "start")      ;; стартовый файл
               ;; Модуль авторизации (зависит от определения сущностей в стартовом файле)
               (:module "auth"
                        :serial t
                        :pathname "mod/auth"
                        :components ((:static-file "auth-tpl.htm")
                                     (:file "auth")))
               ;; Модуль очередей
               (:module "que"
                        :serial t
                        :pathname "mod/que"
                        :components ((:file "que")))
               ;; Модуль сообщений
               
               ;; (:file "events")     ;; события системы
               (:file "iface")      ;; файл веб-интерфейса
               ;; Модуль демонстрации
               (:module "demo"
                        :serial t
                        :pathname "mod/demo"
                        :components ((:file "demo")))
               ))
;; defsystem ends here
