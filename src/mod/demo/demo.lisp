(in-package #:asp)

(in-package :asp)

(defun get-current-time-str ()
  (format nil "~A"
          (multiple-value-bind (second minute hour date month year day daylight-p zone)
              (decode-universal-time (get-universal-time))
            (format nil "~2,'0d:~2,'0d:~2,'0d ~2,'0d.~2,'0d.~2,'0d"
                    hour minute second date month year))))

(defparameter *begin-ticket-id* 1)
(defparameter *current-time* (get-current-time-str))
(defparameter *checkpoint-id* 1)
(defparameter *sector-id* 1)
(defparameter *timer-1* 3000)
(defparameter *timer-2* 3000)
(defparameter *timer-3* 3000)
(defparameter *timer-4* 3000)
(defparameter *secret* "secret-key")

(defparameter *low-level-endpoint* "http://localhost:3999/low_level_endpoint")
(defparameter *tx-counter* 1)

(defun send-to-low-level (msg)
  (multiple-value-bind (body-or-stream status-code headers uri stream must-close reason-phrase)
      (drakma:http-request
       *low-level-endpoint*
       :method :post
       :content (format nil "~{~A~^&~}"
                        `(("json" . ,(drakma:url-encode (cl-json:encode-json-to-string msg) :utf-8))))
       :content-type "application/x-www-form-urlencoded; charset=UTF-8"
       :force-binary t)
    (prog1
        (flexi-streams:octets-to-string body-or-stream :external-format :utf-8)
      (incf *tx-counter*))))

(defmacro define-demo-page ((name page-url title note btn) &body body)
  `(define-page ,name ,page-url
    (let ((breadcrumb (breadcrumb ,title))
          (user       (if (null *current-user*) "" (name (get-user *current-user*)))))
      (standard-page (:breadcrumb breadcrumb :user user :menu (menu) :overlay (reg-overlay))
        (content-box () (heading (,title) ,note))
        (content-box (:class "size-3-5 switch-content-container")
          (form ("demoform" ,title :action ,page-url :class "form-section-container")
            %CTRL%))
        (ps-html ((:span :class "clear")))))
    (:CTRL (ps-html
            ((:input :type "hidden" :name "act" :value "CTRL"))
            ((:div :class "form-send-container")
             (submit ,btn)))
           (let ((breadcrumb (breadcrumb ,title))
                 (user       (if (null *current-user*) "" (name (get-user *current-user*)))))
             (standard-page (:breadcrumb breadcrumb :user user :menu (menu) :overlay (reg-overlay))
               (content-box () (heading ("Успешно")))
               (content-box () (system-msg ("success")
                                 ,@body))
               (ps-html ((:span :class "clear"))))))))

(in-package :asp)

(flet ((form-section (default-email btn)
         (content-box (:class "size-3-5 switch-content-container")
           (form ("demoform" "Начальные установки" :action "/demo" :class "form-section-container")
             ((:div :class "form-section")
              (fieldset ""
                (input ("begin-ticket-id"  "начальный ID билета" :required t :type "" :maxlength "50" :value *begin-ticket-id*))
                (input ("current-time"     "текущее время" :required t :type "" :maxlength "50" :value  (get-current-time-str)))
                (input ("checkpoint-id"    "номер стойки" :required t :type "" :maxlength "50" :value *checkpoint-id*))
                (input ("sector-id"        "номер сектора" :required t :type "" :maxlength "50" :value *sector-id*))
                (input ("timer-1"          "таймаут забора билета (таймер 1)" :required t :type "" :maxlength "50" :value *timer-1*))
                (input ("timer-2"          "длина импульса открытия шлагбаума в миллисекундах (таймер 2)" :required t :type "" :maxlength "50" :value *timer-2*))
                (input ("timer-3"          "длина импульса закрытия шлагбаума в миллисекундах (таймер 3)" :required t :type "" :maxlength "50" :value *timer-3*))
                (input ("timer-4"          "время, отведенное на проезд (таймер 4)" :required t :type "" :maxlength "50" :value *timer-4*))
                (input ("secret"           "ключ шифрования штрих-кода" :required t :type "password" :maxlength "50" :autocomplete "off" :value *secret*))))
             btn))))
  (define-page demo "/demo"
    (let ((breadcrumb (breadcrumb "Демо_1"))
          (user       (if (null *current-user*) "" (name (get-user *current-user*)))))
      (standard-page (:breadcrumb breadcrumb :user user :menu (menu) :overlay (reg-overlay))
        (content-box ()
          (heading ("Демо 1") "На этой странице можно изменять начальные установки системы"))
        (form-section (aif (post-parameter "email") it "") %CTRL%)
        (ps-html ((:span :class "clear")))))
    (:CTRL (ps-html
             ((:input :type "hidden" :name "act" :value "CTRL"))
             ((:div :class "form-send-container")
              (submit "Сохранить" )))
           (progn
             (setf *begin-ticket-id* (parse-integer (getf p :begin-ticket-id)))
             (setf *current-time* (getf p :current-time))
             (setf *checkpoint-id* (parse-integer (getf p :checkpoint-id)))
             (setf *sector-id* (parse-integer (getf p :sector-id)))
             (setf *timer-1* (parse-integer (getf p :timer-1)))
             (setf *timer-2* (parse-integer (getf p :timer-2)))
             (setf *timer-3* (parse-integer (getf p :timer-3)))
             (setf *timer-4* (parse-integer (getf p :timer-4)))
             (setf *secret* (getf p :secret))
             (let ((breadcrumb (breadcrumb "Демо_1"))
                   (user       (if (null *current-user*) "" (name (get-user *current-user*)))))
               (standard-page (:breadcrumb breadcrumb :user user :menu (menu) :overlay (reg-overlay))
                 (content-box ()
                   (heading ("Успешно")))
                 (content-box ()
                   (system-msg ("success")
                     (ps-html ((:p) "Данные сохранены")
                              ((:a :href "/event_checkpoint_button") "Следующий шаг - нажатие кнопки на стойке"))))
                 (ps-html ((:span :class "clear")))))))))

(in-package :asp)

(let ((msg `((:TXID . ,*tx-counter*)
             (:DEVICE . "display")
             (:COMMAND . "show")
             (:PARAMETERS (:SCREEN . 0)))))
  (define-demo-page (demo-page-start "/demo_page_start" "Включение стойки"
                                     "На этой странице можно эмулировать включение стойки"
                                     "Включить стойку")
    (progn
      ;; Отправка сообщения чтобы показать текст на дисплее
      (let ((response (send-to-low-level msg)))
        (format nil "~{~A~}"
                (list
                 (ps-html ((:p) "Сообщение:"))
                 "<pre>"
                 (cl-json:encode-json-to-string msg)
                 "</pre>"
                 (ps-html ((:p) "Ответ:"))
                 "<pre>"
                 (bprint response)
                 "</pre>"
                 ))))))

(in-package :asp)

(defun ticket-assembly (ticket-id time-str checkpoint-id sector-id)
  `("========================="
    "ДОБРО ПОЖАЛОВАТЬ!"
    "========================="
    ,(format nil "~8,'0d" *begin-ticket-id*)
    ,(format nil "Въезд: ~A" (get-current-time-str))
    "Тип т/с: легковой"
    "Гос. знак: А0000АА 78RUS"
    ,(format nil "Стойка ~A, сектор ~A" *checkpoint-id* *sector-id*)
    "========================="
    "проследуйте на место"
    "МЕСТО 0201"
    "в секторе 2"
    "========================="
    "Выезд 1 работает:"
    "с 09:00 до 22:00"
    "Выезд 2 работает:"
    "круглосуточно"
    "В тарифной зоне 1:"
    "20 минут - бесплатно"
    "1 час - 100 руб."
    "более 3 часов - 50 руб."
    "В тарифной зоне 2:"
    "1 час - 100 руб."
    "после 22:00 - 200 руб."
    "========================="
    "Телефон для справок:"
    "8(812)000-00-00"))

(defun barcode-assembly (param)
  "1A B4 C5 DE 35")

;; {
;;   "eventid":XX,
;;   "device":"user-button",
;;   "command":"press"
;; }

(let* ((ticket      (ticket-assembly
                     *begin-ticket-id*
                     (get-current-time-str)
                     *checkpoint-id*
                     *sector-id*))
       (barcode     (barcode-assembly ""))
       (msg-ticket  `((:TXID . ,*tx-counter*)
                      (:DEVICE . "display")
                      (:COMMAND . "print")
                      (:PARAMETERS ((:TICKET . ,ticket) (:BARCODE . ,barcode)))))
       (msg-display `((:TXID . ,*tx-counter*)
                      (:DEVICE . "display")
                      (:COMMAND . "show")
                      (:PARAMETERS (:SCREEN . 1)))))
  (define-demo-page (demo-page-init "/demo_page_init" "Нажатие кнопки на стойке"
                                    "На этой странице можно эмулировать нажатие кнопки на стойке"
                                    "Нажать кнопку")
    (progn
      ;; Отправка сообщения чтобы напечатать билет со штрихкодом
      (let ((response-ticket  (send-to-low-level msg-ticket))
            (response-display (send-to-low-level msg-display)))
        ;; TODO : Непонятно кто должен установить таймер, который вызовет событие окончания печати
        (format nil "~{~A~}"
                (list
                 (ps-html ((:p) "Печать билета со штрихкодом"))
                 "<pre>"
                 (format nil "~{~A<br />~}" ticket)
                 "</pre>"
                 (ps-html ((:p) "Печать штрихкода"))
                 "<pre>"
                 (bprint response-ticket)
                 "</pre>"
                 (ps-html ((:p) "Вывод на дисплей"))
                 "<pre>"
                 (bprint response-ticket)
                 "</pre>"
                 ))))))

(in-package :asp)

;; {
;;   "eventid":XX,
;;   "device":"printer",
;;   "command":"print"
;;   "parameters":{ "result":"OK" }
;; }

(let* ((msg `((:TXID . ,*tx-counter*)
              (:DEVICE . "display")
              (:COMMAND . "show")
              (:PARAMETERS (:SCREEN . 2)))))
  (define-demo-page (demo-page-printout "/demo_page_printout" "Конец печати, пользователь забирает билет"
                                        "На этой странице можно эмулировать забирание билета пользователем"
                                        "Забрать билет")
    (progn
      ;; Отправка сообщения чтобы показать текст "Заберите билет"
      (let ((response (send-to-low-level msg)))
        (format nil "~{~A~}"
                (list
                 (ps-html ((:p) "Сообщение"))
                 "<pre>"
                 (cl-json:encode-json-to-string msg)
                 "</pre>"
                 (ps-html ((:p) "Response"))
                 "<pre>"
                 (bprint response)
                 "</pre>"
                 ))))))

(in-package :asp)

(let* ((msg-shlag   `((:TXID . ,*tx-counter*)
                      (:DEVICE . "shlagbaum_in")
                      (:COMMAND . "up")
                      (:PARAMETERS (:TIMEOUT . 3000))))
       (msg-display `((:TXID . ,*tx-counter*)
                      (:DEVICE . "display")
                      (:COMMAND . "show")
                      (:PARAMETERS (:SCREEN . 3)))))
  (define-demo-page  (demo-page-open "/demo_page_open" "Открытие шлагбаума"
                                     "На этой странице можно открыть шлагбаум" "Открыть шлагбаум")
    (progn
      (let ((response-shlag   (send-to-low-level msg-shlag))
            (response-display (send-to-low-level msg-display)))
        (format nil "~{~A~}"
                (list
                 (ps-html ((:p) "Сообщение шлагбауму"))
                 "<pre>"
                 (cl-json:encode-json-to-string msg-shlag)
                 "</pre>"
                 (ps-html ((:p) "Response"))
                 "<pre>"
                 (bprint response-shlag)
                 "</pre>"
                 (ps-html ((:p) "Сообщение дисплею"))
                 "<pre>"
                 (cl-json:encode-json-to-string msg-display)
                 "</pre>"
                 (ps-html ((:p) "Response"))
                 "<pre>"
                 (bprint response-display)
                 "</pre>"
                 ))))))

(in-package :asp)

(let* ((msg-display `((:TXID . ,*tx-counter*)
                      (:DEVICE . "display")
                      (:COMMAND . "show")
                      (:PARAMETERS (:SCREEN . 4)))))
  (define-demo-page  (demo-page-goon "/demo_page_goon" "Проезд"
                                     "На этой странице можно эмулировать проезд" "Проезжайте")
    (progn
      (let ((response-display (send-to-low-level msg-display)))
        (format nil "~{~A~}"
                (list
                 (ps-html ((:p) "Сообщение дисплею"))
                 "<pre>"
                 (cl-json:encode-json-to-string msg-display)
                 "</pre>"
                 (ps-html ((:p) "Response"))
                 "<pre>"
                 (bprint response-display)
                 "</pre>"
                 ))))))

(in-package :asp)

(let* ((msg-shlag   `((:TXID . ,*tx-counter*)
                      (:DEVICE . "shlagbaum_in")
                      (:COMMAND . "down")
                      (:PARAMETERS (:TIMEOUT . 3000))))
       (msg-display `((:TXID . ,*tx-counter*)
                      (:DEVICE . "display")
                      (:COMMAND . "show")
                      (:PARAMETERS (:SCREEN . 5)))))
  (define-demo-page  (demo-page-close "/demo_page_close" "Закрытие шлагбаума"
                                     "На этой странице можно закрыть шлагбаум" "Закрыть шлагбаум")
    (progn
      (let ((response-shlag   (send-to-low-level msg-shlag))
            (response-display (send-to-low-level msg-display)))
        (format nil "~{~A~}"
                (list
                 (ps-html ((:p) "Сообщение шлагбауму"))
                 "<pre>"
                 (cl-json:encode-json-to-string msg-shlag)
                 "</pre>"
                 (ps-html ((:p) "Response"))
                 "<pre>"
                 (bprint response-shlag)
                 "</pre>"
                 (ps-html ((:p) "Сообщение дисплею"))
                 "<pre>"
                 (cl-json:encode-json-to-string msg-display)
                 "</pre>"
                 (ps-html ((:p) "Response"))
                 "<pre>"
                 (bprint response-display)
                 "</pre>"
                 ))))))


;; Тестируем авторизацию
(defun demo-test ()
  (in-package #:asp)
  
  ;; TODO
  (dbg "passed: demo-test~%"))
(demo-test)
