;;;; -*-coding: utf-8;-*-
;;;;
;;;; Copyright (C) 2020 David Byers
;;;;
;;;; This program is free software: you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License as
;;;; published by the Free Software Foundation, either version 3 of
;;;; the License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;;; General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program. If not, see
;;;; <https://www.gnu.org/licenses/>.

(require 'cl-lib)

;; Decode still uses float arithmetic, so results can be slightly off
;; from the test cases. This is deemed acceptable.

(defvar olctest-decode-tolerance 0.0000000001)

(defun olctest-read-csv (filename)
  "Read a CSV file with test data."
  (let ((buffer (generate-new-buffer "*olctest*")))
    (unwind-protect
        (save-window-excursion
          (set-buffer buffer)
          (insert-file filename)
          (goto-char (point-min))
          (save-excursion (replace-string "full code" "fullcode"))
          (unless (re-search-forward "^# Format.*:$" nil t)
            (error "format line not found in test data"))
          (forward-line 1)
          (beginning-of-line)
          (looking-at "^# *\\(\\S-*\\)")
          (let ((columns (split-string (match-string 1) "," nil))
                (cases nil))
            (while (= 0 (forward-line 1))
              (beginning-of-line)
              (cond ((looking-at "^#"))
                    ((looking-at "^\\s-*$"))
                    ((looking-at "^\\(\\S-+\\)$")
                     (setq cases
                           (cons
                            (cl-mapcar (lambda (key val)
                                         (cons (intern key)
                                               (cond ((string-match "^[-0-9.]+$" val)
                                                      (string-to-number val))
                                                     ((string= val "true") t)
                                                     ((string= val "false") nil)
                                                     (t val))))
                                       columns
                                       (split-string (match-string 1) "," nil))
                            cases)))
                    (t (error (format "unable to parse test data: %s"
                                      (buffer-substring
                                       (point)
                                       (progn (end-of-line) (point))))))))
            cases))
      (kill-buffer buffer))))


(defmacro olctest-run-tests (spec &rest body)
  "Run open location code tests.

\(fn (VAR LIST) BODY...)"
  (declare (indent 1) (debug ((form symbolp) body)))
  (let ((data (gensym "$olctest")))
    `(let ((,data (olctest-read-csv ,(elt spec 0)))
           ($olctest-results nil))
       (setq foo ,data)
       (dolist (,(elt spec 1) ,data)
         ,@body)
       (olctest-report-results $olctest-results))))

(defun olctest-record-failure (case expected actual)
  "Record a test failure."
  (setq $olctest-results (cons `((case . ,case)
                                 (expected . ,expected)
                                 (actual . ,actual))
                               $olctest-results)))

(defun olctest-report-results (results)
  "Report results from tests."
  (if (null results)
      t
    (dolist (result results)
      (princ (format "%s: expected %s got %s\n"
                     (mapconcat (lambda (val) (format "%s" (cdr val))) (alist-get 'case result) ",")
                     (alist-get 'expected result)
                     (alist-get 'actual result))))))


(defun olctest-encode ()
  "Test encoding."
  (olctest-run-tests ("encoding.csv" case)
    (let ((code (olc-encode (alist-get 'latitude case)
                            (alist-get 'longitude case)
                            (alist-get 'length case))))
      (unless (string= code (alist-get 'expected case))
        (olctest-record-failure case (alist-get 'expected case) code)))))


(defun olctest-decode ()
  "Test decoding."
  (olctest-run-tests ("decoding.csv" case)
    (let ((area (olc-decode (alist-get 'code case)))
          (exp-latlo (alist-get 'latLo case))
          (exp-lathi (alist-get 'latHi case))
          (exp-lonlo (alist-get 'lngLo case))
          (exp-lonhi (alist-get 'lngHi case))
          (exp-len (alist-get 'length case)))
      (unless (and (= exp-len (olc-code-length (alist-get 'code case)))
                   (< (abs (- (olc-area-latlo area) exp-latlo)) olctest-decode-tolerance)
                   (< (abs (- (olc-area-lathi area) exp-lathi)) olctest-decode-tolerance)
                   (< (abs (- (olc-area-lonlo area) exp-lonlo)) olctest-decode-tolerance)
                   (< (abs (- (olc-area-lonhi area) exp-lonhi)) olctest-decode-tolerance))
        (olctest-record-failure case
                                (format "%d,%f,%f,%f,%f" exp-len exp-latlo exp-lonlo exp-lathi exp-lonhi)
                                (format "%d,%f,%f,%f,%f"
                                        (olc-code-length (alist-get 'code case))
                                        (olc-area-latlo area)
                                        (olc-area-lonlo area)
                                        (olc-area-lathi area)
                                        (olc-area-lonhi area)))))))


(defun olctest-shortcodes ()
  "Test recovering."
  (olctest-run-tests ("shortCodeTests.csv" case)
    (let ((fullcode (alist-get 'fullcode case))
          (lat (alist-get 'lat case))
          (lon (alist-get 'lng case))
          (shortcode (alist-get 'shortcode case))
          (test-type (alist-get 'test_type case)))

      ;; Test recover
      (when (or (string= test-type "B") (string= test-type "R"))
        (let ((recovered (olc-recover shortcode lat lon)))
          (unless (string= recovered fullcode)
            (olctest-record-failure case fullcode recovered))))

      ;; Test shorten
      (when (or (string= test-type "B") (string= test-type "S"))
        ;; Shorten is not implemented
        )
      )))

(defun olctest-validity ()
  "Test validity."
  (olctest-run-tests ("validityTests.csv" case)
    (let* ((code (alist-get 'code case))
           (expected (list (alist-get 'isValid case)
                           (alist-get 'isShort case)
                           (alist-get 'isFull case)))
           (actual (list (not (not (olc-is-valid code)))
                         (not (not (olc-is-short code)))
                         (not (not (olc-is-full code))))))
      (unless (equal expected actual)
        (olctest-record-failure case expected actual)))))


(defun olctest-run-all ()
  "Run all tests."
  (and (olctest-decode)
       (olctest-encode)
       (olctest-shortcodes)
       (olctest-validity)))


