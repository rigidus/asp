(in-package #:asp)

(in-package :asp)

(use-package :bit-smasher)

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

;; generate: date +%s | sha256sum | head -c 16 ; echo
(defparameter *secret* "8afc8e800cb6998A")

;; номер стойки через которую был совершен въезд - 7 бит (0 - 127)
(defparameter *checkpoint-id* 1)
;;номер сектора к которому относится въездная стойка - 6 бит  (0 - 63)
(defparameter *sector-id*     1)
;; номер места из вирутального диапазона мест, приписанных к сектору - 16 бит  (0 - 65535)
(defparameter *place-id*      1)
;; дата и время вьезда 32 бита (до 19.01.2038) - берется сразу в момент выдачи
;; ...
;; массогабаритные характеристики ТС посетителя (например: 0 = легковой, 1 = средний, 2 = грузовик)
(defparameter *ts-type*       1)

(defparameter *low-level-endpoint* "http://localhost:8000/")
(defparameter *hi-level-endpoint* "http://localhost:3999/gate")

(defparameter *tx-counter* 1)

(defun send-to-low-level (msg)
  (multiple-value-bind (body-or-stream status-code headers uri stream must-close reason-phrase)
      (drakma:http-request
       *low-level-endpoint*
       :method :post
       :content ;; (format nil "~{~A~^&~}"
       ;;        `(("json" . ,(drakma:url-encode (cl-json:encode-json-to-string msg) :utf-8))))
       (cl-json:encode-json-to-string msg)
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

;; unix-time

(defvar *unix-epoch-difference* (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

(defun get-unix-time ()
  (universal-to-unix-time (get-universal-time)))


;; converters

(defun bit-vector->integer (bit-vector)
  "Create a positive integer from a bit-vector."
  (reduce #'(lambda (first-bit second-bit)
              (+ (* first-bit 2) second-bit))
          bit-vector))

(defun integer->bit-vector (integer)
  "Create a bit-vector from a positive integer."
  (labels ((integer->bit-list (int &optional accum)
             (cond ((> int 0)
                    (multiple-value-bind (i r) (truncate int 2)
                      (integer->bit-list i (push r accum))))
                   ((null accum) (push 0 accum))
                   (t accum))))
    (coerce (integer->bit-list integer) 'bit-vector)))

;; shifters

(defun shl (width bits x)
  (logand (ash x bits)
          (1- (ash 1 width))))

(defun shr (width bits x)
  (logand (ash x (- bits))
          (1- (ash 1 width))))

;; macro helpers

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
                ,@body)))))

;; bits implant|explant

(defmacro bits-implant (base start size var)
  (with-gensyms (iter-base iter-implant iter-base-up len)
    (once-only (start size)
      `(let ((,len           (length ,var))
             (,iter-base-up  (+ ,start ,size 1)))
         (loop
            :for ,iter-base     :from ,start         :below ,iter-base-up
            :for ,iter-implant  :from (- ,len ,size) :below ,len
            :do  (setf (aref ,base ,iter-base)
                       (aref ,var ,iter-implant)))))))

(defmacro bits-explant (base start size var)
  (with-gensyms (iter-base iter-implant iter-implant-up)
    (once-only (start size)
      `(let ((,iter-implant-up (+ ,size ,start)))
         (loop
            :for ,iter-base     :from 0 :below ,size
            :for ,iter-implant  :from ,start :below ,iter-implant-up
            :do  (setf (aref ,base ,iter-base)
                       (aref ,var ,iter-implant)))))))

;; barcode assembler|disassembler

(defun barcode-assembly (checkpoint sector place unix-time ts-type)
  "Return bit-array of barcode"
  (let ((base       (make-array 64 :element-type 'bit))
        (checkpoint (bits<- checkpoint))
        (sector     (bits<- sector))
        (place      (let* ((tmp (bits<- place)) (len (length tmp)))
                      (if (not (equal 16 len))
                          (let ((tmp2 (make-array 16 :element-type 'bit)))
                            (bits-implant tmp2 0 len tmp)
                            tmp2)
                          tmp)))
        (time       (bits<- unix-time))
        (ts-type    (bits<- ts-type)))
    (bits-implant base 0 7 checkpoint)
    (bits-implant base 7 6 sector)
    (bits-implant base 13 16 place)  ;; bug
    (bits-implant base 29 32 time)
    (bits-implant base 61 3 ts-type)
    base))

;; (barcode-assembly *checkpoint-id* *sector-id* *place-id* (get-unix-time) *ts-type*)

(defun barcode-disassembly (barcode)
  (let ((checkpoint (make-array 7  :element-type 'bit))
        (sector     (make-array 6  :element-type 'bit))
        (place      (make-array 16 :element-type 'bit))
        (time       (make-array 32 :element-type 'bit))
        (ts-type    (make-array 3  :element-type 'bit)))
    (bits-explant checkpoint 0 7 barcode)
    (bits-explant sector 7 6 barcode)
    (bits-explant place 13 16 barcode)
    (bits-explant time 29 32 barcode)
    (bits-explant ts-type 61 3 barcode)
    (values checkpoint sector place time ts-type)))

;; (barcode-disassembly
;;  (barcode-assembly *checkpoint-id* *sector-id* *place-id* (get-unix-time) *ts-type*))


;; simmetric

(defun simmetric (param key)
  (bit-xor param key))



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
                              ((:a :href "/demo_page_init") "Следующий шаг - нажатие кнопки на стойке"))))
                 (ps-html ((:span :class "clear")))))))))

(in-package :asp)

(restas:define-route gate ("/gate" #|:method :post|#)
  (with-wrapper
    (let* ((p (alist-to-plist (hunchentoot:post-parameters*))))
      (error 'ajax :output "This is hi-level endpoint. You must send a POST")
      )))

(restas:define-route gate-post ("/gate" :method :post)
  (with-wrapper
    (let* ((p (alist-to-plist (hunchentoot:post-parameters*))))
      (error 'ajax :output "POST accepted")
      )))

(in-package :asp)

(let ((msg `((:TXID . ,*tx-counter*)
             (:DEVICE . "display")
             (:COMMAND . "show")
             (:PARAMETERS (:SCREEN . "0")))))
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

;; {
;;   "eventid":XX,
;;   "device":"user-button",
;;   "command":"press"
;; }

(define-demo-page (demo-page-init "/demo_page_init" "Нажатие кнопки на стойке"
                                  "На этой странице можно эмулировать нажатие кнопки на стойке"
                                  "Нажать кнопку")
    (let* ((ticket      (ticket-assembly
                         *begin-ticket-id*
                         (get-current-time-str)
                         *checkpoint-id*
                         *sector-id*))
           (barcode     (hex<- (simmetric
                         (barcode-assembly *checkpoint-id* *sector-id* *place-id* (get-unix-time) *ts-type*)
                         (bits<- *secret*))))
           (msg-ticket  `((:TXID . ,*tx-counter*)
                          (:DEVICE . "printer")
                          (:COMMAND . "print")
                          (:PARAMETERS . ((:TICKET . ,ticket) (:BARCODE . ,barcode)))))
           (msg-display `((:TXID . ,*tx-counter*)
                          (:DEVICE . "display")
                          (:COMMAND . "show")
                          (:PARAMETERS (:SCREEN . 1)))))
      ;; Отправка сообщения чтобы напечатать билет со штрихкодом
      (let ((response-ticket  (send-to-low-level msg-ticket))
            (response-display (send-to-low-level msg-display)))
        (incf *begin-ticket-id*)
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
                 )))))

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
