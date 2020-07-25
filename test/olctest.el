;; -*-coding: utf-8; lexical-binding: t;-*-
;;
;; Copyright (C) 2020 David Byers
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

(require 'cl-lib)

;; Due to rounding and floating point representation we can't seem
;; to get closer than 1e-10 to the reference test cases, but sinze
;; it only affects decoding, that is an insignificant error level.

(defvar olctest-decode-tolerance 0.0000000001)
(defvar --olctest-results)
(defvar --olctest-current-case)


(defmacro olctest-testcase (name &rest body)
  "Set up an open location code test case."
  (declare (indent 1)
           (debug (form &rest form)))
  `(let ((--olctest-results nil)
         (--olctest-current-case ,name))
     (message "olctest running %s" ,name)
     ,@body
     (olctest-report-results (reverse --olctest-results))))


(cl-defun olctest-record-failure (&key exp act msg)
  "Record a test failure."
  (setq --olctest-results
        (cons `((name . ,--olctest-current-case)
                (msg . ,msg)
                (exp . ,exp)
                (act . ,act))
              --olctest-results)))

(defmacro olctest-expect-failure (name &rest body)
  "Expect a failure."
  (declare (indent 1) (debug (form &rest form)))
  `(unless (let ((--olctest-results nil))
             ,@body
             --olctest-results)
     (olctest-record-failure :exp 'failure :act 'success :msg ,name)))

(defun olctest-report-results (results)
  "Report results from tests."
  (if (null results)
      t
    (dolist (result results)
      (princ (format "%s:%s: expected %s got %s\n"
                     (alist-get 'name result)
                     (or (alist-get 'msg result) "-")
                     (alist-get 'exp result)
                     (alist-get 'act result))))))

(cl-defun olctest-string= (&key exp act msg)
  (unless (string= exp act)
    (olctest-record-failure :exp exp :act act :msg msg)))

(cl-defun olctest-float= (&key exp act msg)
  (unless (< (abs (- act exp)) olctest-decode-tolerance)
    (olctest-record-failure :exp exp :act act :msg msg)))

(cl-defun olctest-equal (&key exp act msg)
  (unless (equal exp act)
    (olctest-record-failure :exp exp :act act :msg msg)))

(cl-defmacro olctest-assert-error ((&key exp msg) &rest body)
  (declare (indent 1))
  `(when (condition-case --olctest-caught-error
             (progn ,@body t)
           ,@(mapcar (lambda (spec)
                       (cond ((symbolp spec) `(,spec nil))
                             ((listp spec)
                              `(,(car spec)
                                (olctest-equal :exp ',spec :act --olctest-caught-error) nil))
                             (t (error "invalid olctest error specification"))))
                     exp)
           (error (olctest-record-failure :exp ',exp :act --olctest-caught-error :msg ,msg) nil))
     (olctest-record-failure :exp ',exp :act 'noerror :msg ,msg)))


(defun olctest-read-csv (filename)
  "Read a CSV file with test data."
  (let ((buffer (generate-new-buffer "*olctest*"))
        (lineno 1))
    (unwind-protect
        (save-window-excursion
          (set-buffer buffer)
          (insert-file-contents filename)
          (goto-char (point-min))
          (save-excursion (while (re-search-forward "full code" nil t)
                            (replace-match "fullcode" nil t)))
          (unless (re-search-forward "^# Format.*:$" nil t)
            (error "format line not found in test data"))
          (forward-line 1)
          (setq lineno (1+ lineno))
          (beginning-of-line)
          (looking-at "^# *\\(\\S-*\\)")
          (let ((columns (split-string (match-string 1) "," nil))
                (cases nil))
            (while (= 0 (forward-line 1))
              (setq lineno (1+ lineno))
              (beginning-of-line)
              (cond ((looking-at "^#"))
                    ((looking-at "^\\s-*$"))
                    ((looking-at "^\\(\\S-+\\)$")
                     (setq cases
                           (cons `((lineno . , lineno)
                                   ,@(cl-mapcar (lambda (key val)
                                                  (cons (intern key)
                                                        (cond ((string-match "^[-0-9.]+$" val)
                                                               (string-to-number val))
                                                              ((string= val "true") t)
                                                              ((string= val "false") nil)
                                                              (t val))))
                                                columns
                                                (split-string (match-string 1) "," nil)))
                                 cases)))
                    (t (error (format "unable to parse test data: %s"
                                      (buffer-substring
                                       (point)
                                       (progn (end-of-line) (point))))))))
            cases))
      (kill-buffer buffer))))


(defmacro olctest-run-csv (spec &rest body)
  "Run open location code tests.

\(fn (VAR LIST) BODY...)"
  (declare (indent 1) (debug ((form symbolp) body)))
  (let ((data (gensym "---olctest")))
    `(let* ((,data (olctest-read-csv ,(elt spec 0))))
       (dolist (,(elt spec 1) ,data)
         ,@body))))


(defmacro olctest-run-list (spec &rest body)
  "Run open location code tests.

\(fn (VAR LIST) BODY...)"
  (declare (indent 1) (debug ((form symbolp) body)))
  (let ((data (gensym "$olctest")))
    `(let* ((,data ,(elt spec 0))
            ($olctest-results nil))
       (dolist (,(elt spec 1) ,data)
         ,@body)
       (olctest-report-results $olctest-results))))


(defun olctest-encode ()
  "Test encoding."
  (olctest-testcase "reference:encoding"
    (olctest-run-csv ("encoding.csv" case)
      (let ((code (olc-encode (alist-get 'latitude case)
                              (alist-get 'longitude case)
                              :len (alist-get 'length case))))
        (olctest-string= :act code
                         :exp (alist-get 'expected case)
                         :msg (alist-get 'msg case))))))


(defun olctest-decode ()
  "Test decoding."
  (olctest-testcase "reference:decoding"
    (olctest-run-csv ("decoding.csv" case)
      (let* ((code (alist-get 'code case))
             (parse (condition-case nil (olc-parse-code code) (error nil)))
             (area (and parse (olc-decode parse)))
             (exp-latlo (alist-get 'latLo case))
             (exp-lathi (alist-get 'latHi case))
             (exp-lonlo (alist-get 'lngLo case))
             (exp-lonhi (alist-get 'lngHi case))
             (exp-len (alist-get 'length case))
             (lineno (alist-get 'lineno case))
             (pact-len (and parse (olc-code-precision parse)))
             (sact-len (olc-code-precision code))
             (msg (format "%d:%s:%%s" lineno code)))
        (if (null area)
            (olctest-record-failure :exp 'success :act 'parse-error :msg code)
          (olctest-equal :act (olc-code-precision code) :exp exp-len :msg (format msg "len(string)"))
          (olctest-equal :act (olc-code-precision parse) :exp exp-len :msg (format msg "len(parsed)"))
          (olctest-float= :act (olc-area-latlo area) :exp exp-latlo :msg (format msg "latlo"))
          (olctest-float= :act (olc-area-lathi area) :exp exp-lathi :msg (format msg "lathi"))
          (olctest-float= :act (olc-area-lonlo area) :exp exp-lonlo :msg (format msg "lonlo"))
          (olctest-float= :act (olc-area-lonhi area) :exp exp-lonhi :msg (format msg "lonhi"))
          )))))

(defun olctest-shortcodes ()
  "Test recovering."
  (olctest-testcase "reference:shortcodes"
    (olctest-run-csv ("shortCodeTests.csv" case)
      (let ((fullcode (alist-get 'fullcode case))
            (lat (alist-get 'lat case))
            (lon (alist-get 'lng case))
            (shortcode (alist-get 'shortcode case))
            (test-type (alist-get 'test_type case))
            (lineno (alist-get 'lineno case)))

        ;; Test recover
        (when (or (string= test-type "B") (string= test-type "R"))
          (olctest-string= :act (olc-recover shortcode lat lon)
                           :exp fullcode
                           :msg lineno))

        ;; Test shorten
        (when (or (string= test-type "B") (string= test-type "S"))
          (olctest-string= :act (olc-shorten fullcode lat lon)
                           :exp shortcode
                           :msg lineno))))))


(defun olctest-validity ()
  "Test validity."
  (olctest-testcase "reference:validity"
    (olctest-run-csv ("validityTests.csv" case)
      (let* ((code (alist-get 'code case))
             (parse (condition-case nil (olc-parse-code code) (error nil)))
             (exp (list (alist-get 'isValid case)
                        (alist-get 'isShort case)
                        (alist-get 'isFull case)))
             (sact (list (and parse (not (not (olc-is-valid code))))
                         (and parse (not (not (olc-is-short code))))
                         (and parse (not (not (olc-is-full code))))))
             (pact (list (and parse (not (not (olc-is-valid parse))))
                         (and parse (not (not (olc-is-short parse))))
                         (and parse (not (not (olc-is-full parse)))))))
        (olctest-equal :exp exp :act pact :msg (format "%s:parsed" code))
        (olctest-equal :exp exp :act sact :msg (format "%s:string" code))))))


(defvar olctest-local-shorten-tests
  '(((code . "9C3W9QCJ+2VX") (lat . 51.3701125) (lon . -1.217765625) (len . 8) (exp . "+2VX"))
    ((code . "9C3W9QCJ+2VX") (lat . 51.3701125) (lon . -1.217765625) (len . 6) (exp . "CJ+2VX"))
    ((code . "9C3W9QCJ+2VX") (lat . 51.3701125) (lon . -1.217765625) (len . 4) (exp . "9QCJ+2VX"))
    ((code . "9C3W9QCJ+2VX") (lat . 51.3701125) (lon . -1.217765625) (len . 2) (exp . "3W9QCJ+2VX"))))

(defun olctest-localtests ()
  (olctest-testcase "local:misc"
    (olctest-run-list (olctest-local-shorten-tests case)
      (let* ((fullcode (alist-get 'code case))
             (lat (alist-get 'lat case))
             (lon (alist-get 'lon case))
             (len (alist-get 'len case))
             (shortcode (alist-get 'exp case))
             (actual (olc-shorten fullcode lat lon :limit len)))
        (olctest-string= :exp shortcode :act actual :msg len)))

    (olctest-equal :act (olc-shorten-compound "546FWWM2+F6")
                   :exp "WWM2+F6 Adamstown, Pitcairn")
    ))


(defun olctest-issue-3 ()
  (olctest-testcase "local:issue-3"
    (olctest-equal :exp nil
                   :act (olc-is-short "22334455+")
                   :msg "S1")

    (olctest-equal :exp t
                   :act (olc-is-short "334455+66")
                   :msg "S2")

    (olctest-equal :exp nil
                   :act (olc-is-short "+12345678")
                   :msg "S3")))

(defun olctest-issue-2 ()
  (olctest-testcase "local:issue-2"
    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-parse-code")
      (olc-parse-code nil))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-is-valid")
      (olc-is-valid nil))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-is-short")
      (olc-is-short nil))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-is-full")
      (olc-is-full nil))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-code-precision")
      (olc-code-precision nil))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-encode:lat")
      (olc-encode nil 1))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-encode:lon")
      (olc-encode 1 nil))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-encode:key")
      (olc-encode 1 1 :len nil))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-decode")
      (olc-decode nil))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-shorten:code")
      (olc-shorten nil 1 1))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-shorten:lat")
      (olc-shorten "" nil 1))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-shorten:lon")
      (olc-shorten "" 1 nil))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-shorten:limit:nil")
      (olc-shorten "" 1 nil))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-shorten:limit:lo")
      (olc-shorten "" 1 1 :limit 0))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-shorten:limit:hi")
      (olc-shorten "" 1 1 :limit 19))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-shorten:limit:odd")
      (olc-shorten "" 1 1 :limit 3))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-shorten-compound:code:nil")
      (olc-shorten-compound nil))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-shorten-compound:limit:nil")
      (olc-shorten-compound "22222222+" :limit nil))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-shorten-compound:limit:lo")
      (olc-shorten-compound "22222222+" :limit 0))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-shorten-compound:limit:hi")
      (olc-shorten-compound "22222222+" :limit 19))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-shorten-compound:limit:odd")
      (olc-shorten-compound "22222222+" :limit 3))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-shorten-compound:zoom:nil")
      (olc-shorten-compound "22222222+" :zoom nil))

    (olctest-assert-error (:exp (args-out-of-range) :msg "olc-shorten-compound:zoom:lo")
      (olc-shorten-compound "22222222+" :zoom 0))

    (olctest-assert-error (:exp (args-out-of-range) :msg "olc-shorten-compound:zoom:hi")
      (olc-shorten-compound "22222222+" :zoom 19))

    (olctest-assert-error (:exp (args-out-of-range) :msg "olc-shorten-compound:zoom:llo")
      (olc-shorten-compound "22222222+" :zoom '(0 8)))

    (olctest-assert-error (:exp (args-out-of-range) :msg "olc-shorten-compound:zoom:rhi")
      (olc-shorten-compound "22222222+" :zoom '(1 19)))

    (olctest-assert-error (:exp (args-out-of-range) :msg "olc-shorten-compound:zoom:inv")
      (olc-shorten-compound "22222222+" :zoom '(5 4)))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-recover:code")
      (olc-recover nil 1 1))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-recover:lat")
      (olc-recover "" nil 1))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-recover:lon")
      (olc-recover "" 1 nil))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-recover:format")
      (olc-recover "22222222+" 1 1 :format 'invalid))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-recover-compound:code")
      (olc-recover-compound nil :ref "Stockholm"))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-recover-compound:ref:double")
      (olc-recover-compound "2222+ Stockholm" :ref "Stockholm"))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-recover-compound:ref:nil")
      (olc-recover-compound "2222+" :ref nil))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "olc-recover-compound:format")
      (olc-recover-compound "2222+" :ref "Stockholm" :format 'invalid))

    ))



(defun olctest-issue-1 ()
  (olctest-testcase "local:issue-1"
    (olctest-assert-error (:exp (wrong-type-argument) :msg "F1")
      (olc-recover-compound nil))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "F2")
      (olc-recover-compound "+9C Sweden" :ref "Norway"))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "F3")
      (olc-recover-compound "+9C" :ref nil))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "F4")
      (olc-recover-compound "+9C Sweden" :format 'undefined))

    (olctest-string= :exp "9FFV9VH8+9C"
                     :act (olc-recover-compound "9FFV9VH8+9C")
                     :msg "O1")

    (olctest-string= :exp "9FFV9VH8+9C"
                     :act (olc-recover-compound "9FFV9VH8+9C" :ref "Antarctica")
                     :msg "O1")

    (olctest-equal :exp '(-89.99875 . -179.99875)
                   :act (olc-recover "22222222+" 0 0 :format 'latlon)
                   :msg "O4")

    (olctest-equal :exp '(-89.99875 . -179.99875)
                   :act (olc-recover-compound "22222222+" :format 'latlon)
                   :msg "O4")

    (olctest-string= :exp "9FFPMGGC+9C"
                     :act (olc-recover-compound "+9C Sweden")
                     :msg "O2")

    (olctest-string= :exp "9FFPMGGC+9C"
                     :act (olc-recover-compound "+9C" :ref "Sweden")
                     :msg "O3")

    ))


(defun olctest-errors ()
  (olctest-testcase "local:errors"
    (olctest-assert-error (:exp ((olc-parse-error-unexpected-end "22" 2)) :msg "P1")
      (olc-parse-code "22"))

    (olctest-assert-error (:exp ((olc-parse-error-invalid-character "O2+" 0 "O")) :msg "P2")
      (olc-parse-code "O2+"))

    (olctest-assert-error (:exp ((olc-parse-error-invalid-character "2O+" 1 "O")) :msg "P3")
      (olc-parse-code "2O+"))

    (olctest-assert-error (:exp ((olc-parse-error-invalid-character "20+" 1 "0")) :msg "P4")
      (olc-parse-code "20+"))

    (olctest-assert-error (:exp ((olc-parse-error-unexpected-end "FFFFFFFF" 8)) :msg "P5")
      (olc-parse-code "FFFFFFFF"))

    (olctest-assert-error (:exp ((olc-parse-error-missing-plus "FFFFFFFFF" 8)) :msg "P6")
      (olc-parse-code "FFFFFFFFF"))

    (olctest-assert-error (:exp ((olc-parse-error-padded-shortcode "FF0000+" 7)) :msg "P7")
      (olc-parse-code "FF0000+"))

    (olctest-assert-error (:exp ((olc-parse-error-invalid-padding "FF00000+" 8)) :msg "P8")
      (olc-parse-code "FF00000+"))

    (olctest-assert-error (:exp ((olc-parse-error-digit-after-padding "FF000000+FF" 9 "F")) :msg "P9")
      (olc-parse-code "FF000000+FF"))

    (olctest-assert-error (:exp ((olc-parse-error-unexpected-end "FFFFFFFF+F" 10)) :msg "P10")
      (olc-parse-code "FFFFFFFF+F"))

    (olctest-assert-error (:exp ((olc-parse-error-invalid-character "FFFFFFFF+F0" 10 "0")) :msg "P11")
      (olc-parse-code "FFFFFFFF+F0"))

    (olctest-assert-error (:exp ((olc-parse-error-invalid-character "FFFFFFFF+FF0" 11 "0")) :msg "P12")
      (olc-parse-code "FFFFFFFF+FF0"))

    (olctest-assert-error (:exp ((olc-parse-error-empty-code "+" 0)) :msg "P13")
      (olc-parse-code "+"))

    (olctest-assert-error (:exp ((olc-decode-error-shortcode "22+")) :msg "D1")
      (olc-decode "22+"))

    (olctest-assert-error (:exp ((olc-shorten-error-padded "22222200+")) :msg "S1")
      (olc-shorten "22222200+" 0 0))

    (olctest-assert-error (:exp ((olc-shorten-error-shortcode "22+")) :msg "S2")
      (olc-shorten-compound "22+"))

    (olctest-assert-error (:exp ((olc-shorten-error-padded "FFFFFF00+")) :msg "S3")
      (olc-shorten-compound "FFFFFF00+"))

    (olctest-assert-error (:exp ((olc-recover-error-reference-not-found
                                  "22+" "Nowhere Special, Pitcairn")) :msg "R1")
      (olc-recover-compound "22+ Nowhere Special, Pitcairn"))

    (let ((olc-nominatim-url "https://invalid.domain/nominatim"))
      (olctest-assert-error (:exp ((olc-recover-error-reference-search-failed
                                    "22+" "Sweden")) :msg "R2")
        (olc-recover-compound "22+ Sweden")))

    ))

(defun olctest-issue-5 ()
  (olctest-testcase "issue-5"
      (olctest-string= :exp "https://nominatim.openstreetmap.org/search"
                       :act (olc-nominatim-endpoint "search")
                       :msg "1")

    (let ((olc-nominatim-url "https://nominatim.invalid"))
      (olctest-string= :exp "https://nominatim.invalid/search"
                       :act (olc-nominatim-endpoint "search")
                       :msg "2"))

    (let ((olc-nominatim-url "https://nominatim.invalid/"))
      (olctest-string= :exp "https://nominatim.invalid/reverse"
                       :act (olc-nominatim-endpoint "reverse")
                       :msg "3"))))

(defun olctest-issue-6 ()
  (olctest-testcase "issue-6"
    (olctest-equal :exp t :act (olc-is-valid "2222+"))
    (olctest-equal :exp t :act (olc-is-valid "2222+" :compound t))
    (olctest-equal :exp nil :act (olc-is-valid "2222+ Sweden"))
    (olctest-equal :exp t :act (olc-is-valid "2222+ Sweden" :compound t))

    (olctest-equal :exp t :act (olc-is-short "2222+"))
    (olctest-equal :exp t :act (olc-is-short "2222+" :compound t))
    (olctest-equal :exp nil :act (olc-is-short "2222+ Sweden"))
    (olctest-equal :exp t :act (olc-is-short "2222+ Sweden" :compound t))
    (olctest-equal :exp nil :act (olc-is-short "22222222+"))
    (olctest-equal :exp nil :act (olc-is-short "22222222+" :compound t))
    (olctest-equal :exp nil :act (olc-is-short "22222222+ Sweden"))
    (olctest-equal :exp nil :act (olc-is-short "22222222+ Sweden" :compound t))

    (olctest-equal :exp nil :act (olc-is-full "2222+"))
    (olctest-equal :exp nil :act (olc-is-full "2222+" :compound t))
    (olctest-equal :exp nil :act (olc-is-full "2222+ Sweden"))
    (olctest-equal :exp nil :act (olc-is-full "2222+ Sweden" :compound t))
    (olctest-equal :exp t :act (olc-is-full "22222222+"))
    (olctest-equal :exp t :act (olc-is-full "22222222+" :compound t))
    (olctest-equal :exp nil :act (olc-is-full "22222222+ Sweden"))
    (olctest-equal :exp t :act (olc-is-full "22222222+ Sweden" :compound t))
    ))


(defvar olctest-selected-tests)

(defmacro run-test (arg)
  `(if (or (null (ignore-errors olctest-selected-tests))
           (memq ',arg olctest-selected-tests))
       (funcall (intern (concat "olctest-" (symbol-name ',arg))))
     t))

(defun olctest-run-all ()
  "Run all tests."
  (let ((olctest-selected-tests
         (mapcar 'intern command-line-args-left)))
    (and (run-test decode)
         (run-test encode)
         (run-test shortcodes)
         (run-test validity)
         (run-test localtests)
         (run-test errors)
         (run-test issue-6)
         (run-test issue-5)
         (run-test issue-3)
         (run-test issue-2)
         (run-test issue-1)
         )
       ))

(defun olctest-batch-test ()
  (kill-emacs
   (if (condition-case err
           (olctest-run-all)
         (error (message (format "error: %s %s" (car err) (cdr err))) nil))
       0 1)))

