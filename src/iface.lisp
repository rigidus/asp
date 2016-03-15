;; [[file:louis.org][iface]]
;;;; 

;;;; iface.lisp

(in-package #:asp)

;; Компилируем шаблоны
(closure-template:compile-template
 :common-lisp-backend (pathname (concatenate 'string *base-path* "templates.htm")))

;; Страницы
(in-package #:asp)

(defmethod show ((param (eql nil)) &rest actions &key &allow-other-keys)
  (ps-html
   ((:div :class "article-list-container article-list-container--list")
    ((:ul :class "article-list article-list--list")
     ((:p) "Нет элементов для отображения")))))
(in-package #:asp)

(defmethod show ((param list) &rest actions &key &allow-other-keys)
  (setf (car param)
        (apply #'show (list* (car param) actions)))
  (ps-html
   ((:div :class "article-list-container article-list-container--list")
    ((:ul :class "article-list article-list--list")
     (reduce #'(lambda (acc elt)
                 (concatenate 'string
                              acc
                              (apply #'show (list* elt actions))))
             param)))))
;; iface ends here
