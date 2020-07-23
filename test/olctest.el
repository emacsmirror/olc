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
     (olctest-report-results --olctest-results)))


(cl-defun olctest-record-failure (&key exp act msg)
  "Record a test failure."
  (setq --olctest-results
        (cons `((name . ,--olctest-current-case)
                (msg . ,msg)
                (exp . ,exp)
                (act . ,act))
              --olctest-results)))

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

(cl-defun olctest-equal (&key exp act msg)
  (unless (equal exp act)
    (olctest-record-failure :exp exp :act act :msg msg)))

(cl-defmacro olctest-assert-error ((&key exp msg) &rest body)
  (declare (indent 1))
  `(when (condition-case --olctest-caught-error
             (progn ,@body t)
           (,exp nil)
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
      (let ((area (olc-decode (alist-get 'code case)))
            (exp-latlo (alist-get 'latLo case))
            (exp-lathi (alist-get 'latHi case))
            (exp-lonlo (alist-get 'lngLo case))
            (exp-lonhi (alist-get 'lngHi case))
            (exp-len (alist-get 'length case)))
        (unless (and (= exp-len (olc-code-precision (alist-get 'code case)))
                     (< (abs (- (olc-area-latlo area) exp-latlo)) olctest-decode-tolerance)
                     (< (abs (- (olc-area-lathi area) exp-lathi)) olctest-decode-tolerance)
                     (< (abs (- (olc-area-lonlo area) exp-lonlo)) olctest-decode-tolerance)
                     (< (abs (- (olc-area-lonhi area) exp-lonhi)) olctest-decode-tolerance))
          (olctest-record-failure
           :exp (format "%d,%f,%f,%f,%f" exp-len exp-latlo exp-lonlo exp-lathi exp-lonhi)
           :act (format "%d,%f,%f,%f,%f"
                        (olc-code-precision (alist-get 'code case))
                        (olc-area-latlo area)
                        (olc-area-lonlo area)
                        (olc-area-lathi area)
                        (olc-area-lonhi area))
           :msg (alist-get 'lineno case)))))))


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
             (exp (list (alist-get 'isValid case)
                        (alist-get 'isShort case)
                        (alist-get 'isFull case)))
             (act (list (not (not (olc-is-valid code)))
                        (not (not (olc-is-short code)))
                        (not (not (olc-is-full code))))))
        (olctest-equal :exp exp :act act :msg code)))))


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
        (olctest-string= :exp shortcode :act actual :msg len)))))


(defun olctest-issue-1 ()
  (olctest-testcase "local:issue-1"
    (olctest-string= :exp "9FFV9VH8+9C"
                     :act (olc-recover-compound "9FFV9VH8+9C" :ref "Antarctica")
                     :msg "O1")

    (olctest-string= :exp "9FFPMGGC+9C"
                     :act (olc-recover-compound "+9C Sweden")
                     :msg "O2")

    (olctest-string= :exp "9FFPMGGC+9C"
                     :act (olc-recover-compound "+9C" :ref "Sweden")
                     :msg "O3")

    (olctest-assert-error (:exp (wrong-type-argument) :msg "F1")
      (olc-recover-compound nil))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "F2")
      (olc-recover-compound "+9C Sweden" :ref "Norway"))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "F3")
      (olc-recover-compound "+9C" :ref nil))

    (olctest-assert-error (:exp (wrong-type-argument) :msg "F4")
      (olc-recover-compound "+9C Sweden" :format 'undefined))))


(defun olctest-run-all ()
  "Run all tests."
  (and (olctest-decode)
       (olctest-encode)
       (olctest-shortcodes)
       (olctest-validity)
       (olctest-localtests)
       (olctest-issue-1)
       ))

(defun olctest-batch-test ()
  (kill-emacs
   (if (condition-case err
           (olctest-run-all)
         (error (message (format "error: %s %s" (car err) (cdr err))) nil))
       0 1)))
