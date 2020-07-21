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

;;; ========================================================================
;;; This program provides basic open location code support in emacs
;;; lisp. The support for recovering shortened codes depends on the
;;; request library and uses OpenStreetMap; please check the terms of
;;; use for the service to ensure that you remain compliant.
;;;
;;; All methods required by the open location code specification are
;;; provided in some form. The implementation passed the tests present
;;; in the open location code github repository at the time of writing
;;; almost cleanly -- there are some minor rounding issues in decode.
;;; ========================================================================


;; This is me being dragged kicking and screaming into the 21st
;; century because the alternative is to cl-lib is to include my own
;; structured data code (which would be overkill) or do it manually
;; (which is a pain in the backside). So cl-lib it is.

(require 'cl-lib)
(require 'request nil t)


;; ========================================================================
;; Errors raised by this package
;; ========================================================================

(define-error 'olc-error "Error in open location code.")
(define-error 'olc-parse-error "Parse error in open location code" 'olc-error)
(define-error 'olc-decode-error "Error decoding open location code" 'olc-error)
(define-error 'olc-encode-error "Error encoding open location code" 'olc-error)
(define-error 'olc-shorten-error "Error shortening open location code" 'olc-error)

;; ========================================================================
;; Mapping of digits to base 20 values
;; ========================================================================

(defvar olc-value-mapping "23456789CFGHJMPQRVWX"
  "Mapping from values to olc base 20 digits.")

(defvar olc-digit-mapping (let ((count 0))
			    (mapcan (lambda (letter)
				      (prog1 (list (cons letter count)
						   (cons (downcase letter) count))
					(setq count (1+ count))))
				    olc-value-mapping))
  "Mapping from olc base 20 digits to values.")

(defsubst olc-digit-value (digit)
  "Return the base 20 value of a digit."
  (cdr (assq digit olc-digit-mapping)))

(defsubst olc-value-digit (value)
  "Return the digit for a value up to 19."
  (elt olc-value-mapping value))


;; ========================================================================
;; Data structures
;; ========================================================================

(cl-defstruct (olc-parse (:copier nil)
                         (:constructor olc-parse-create))
  (pairs nil :read-only t)
  (grid nil :read-only t)
  (short nil :read-only t)
  (precision nil :read-only t))

(cl-defstruct (olc-area (:copier nil)
                        (:constructor olc-area-create))
  (latlo nil :read-only t)
  (lonlo nil :read-only t)
  (lathi nil :read-only t)
  (lonhi nil :read-only t))

(defsubst olc-area-lat (area)
  "Get center latitute of AREA."
  (min (+ (/ (- (olc-area-lathi area) (olc-area-latlo area)) 2) (olc-area-latlo area)) 90))

(defsubst olc-area-lon (area)
  "Get center longitude of AREA."
  (min (+ (/ (- (olc-area-lonhi area) (olc-area-lonlo area)) 2) (olc-area-lonlo area)) 180))


;; ========================================================================
;; (Mostly) internal functions
;; ========================================================================

(defmacro olc-valid-char (char)
  "Check if CHAR is a valid OLC digit."
  `(assq ,char olc-digit-mapping))

(defmacro olc-transform-error (spec &rest body)
  "Catch some errors and throw others."
  (declare (indent 1))
  `(condition-case nil
       ,@body
     (,(elt spec 0) (signal ',(elt spec 1) (list ,@(cddr spec))))))

(defsubst olc-clip-latitude (lat)
  "Clip LAT to -90,90"
  (max -90 (min 90 lat)))

(defsubst olc-normalize-latitude (lat len)
  "Normalize latitude LAT."
  (setq lat (olc-clip-latitude lat))
  (when (= lat 90.0)
    (setq lat (- lat (/ (olc-latitude-precision len) 2.0))))
  lat)


(defsubst olc-normalize-longitude (lon)
  "Normalize longitude LON."
  (while (< lon -180) (setq lon (+ lon 360)))
  (while (>= lon 180) (setq lon (- lon 360)))
  lon)

(defun olc-latitude-precision (len)
  "Compute latitude precision in code of length LEN."
  (if (<= len 10)
      (expt 20 (- (floor (+ 2 (/ len 2)))))
    (/ (expt 20 -3) (expt 5 (- len 10)))))

(defun olc-parse-code (code)
  "Parse an open location code CODE."
  (if (olc-parse-p code)
      code
    (let ((pos 0)
	  (pairs nil)
	  (short nil)
          (precision nil)
	  (grid nil)
	  (padding 0))

      ;; Parse up to four initial pairs
      (catch 'break
	(while (< pos (length code))
          (olc-transform-error (args-out-of-range olc-parse-error "code too short" code (1+ pos))
            (cond ((eq (elt code pos) ?+) (throw 'break nil))
		  ((eq (elt code pos) ?0) (throw 'break nil))
		  ((= (length pairs) 4) (throw 'break nil))
		  ((not (olc-valid-char (elt code pos)))
		   (signal 'olc-parse-error (list "invalid character" pos code)))
		  ((not (olc-valid-char (elt code (1+ pos))))
		   (signal 'olc-parse-error (list "invalid character" (1+ pos) code)))
		  (t (setq pairs (cons (cons (elt code pos) (elt code (1+ pos))) pairs)))))
	  (setq pos (+ pos 2))))

      ;; Measure the padding
      (when (string-match "0+" code pos)
	(setq pos (match-end 0) padding (- (match-end 0) (match-beginning 0))))

      ;; Parse the separator
      (olc-transform-error (args-out-of-range olc-parse-error "code too short" code pos)
        (if (eq (elt code pos) ?+)
	    (setq pos (1+ pos))
	  (signal 'olc-parse-error (list "missing separator" pos code))))

      ;; Check the length of the padding
      (unless (and (= (% padding 2) 0)
                   (<= (+ padding (* 2 (length pairs))) 8))
        (signal 'olc-parse-error (list "incorrect padding length" pos code)))

      ;; Determine if the code is shortened or not
      (setq short (< (+ (* 2 (length pairs)) padding) 8))

      ;; We cant be short and have padding (not sure why)
      (when (and short (> padding 0))
        (signal 'olc-parse-error (list "padded codes can't be shortened" pos code)))

      ;; Determine the precision of the code
      (setq precision (- 8 padding))

      ;; Parse what's after the separator
      (when (< pos (length code))
	(when (> padding 0)
	  (signal 'olc-parse-error (list "padding followed by data" pos code)))

	;; Parse one more pair
        (olc-transform-error (args-out-of-range olc-parse-error "code too short" code (1+ pos))
          (setq pairs (cons (cons (elt code pos) (elt code (1+ pos))) pairs)
	        pos (+ 2 pos)
                precision (+ 2 precision)))

	;; Parse grid
	(while (< pos (length code))
	  (cond ((not (olc-valid-char (elt code pos)))
		 (signal 'olc-parse-error (list "invalid character" pos code)))
		((>= (length grid) 5) (setq pos (1+ pos)))
		(t (setq grid (cons (elt code pos) grid)
			 pos (1+ pos)
                         precision (1+ precision))))))

      ;; Check for an empty code
      (unless pairs
        (signal 'olc-parse-error (list "invalid code" 0 code)))

      ;; Return the result
      (olc-parse-create :pairs (nreverse pairs)
                        :grid (nreverse grid)
                        :short short
                        :precision precision))))


;;; ========================================================================
;;; Public functions
;;; ========================================================================

(defun olc-is-valid (code)
  "Return non-nil if CODE is a valid open location code."
  (condition-case nil
      (olc-parse-code code)
    (olc-parse-error nil)))

(defun olc-is-short (code)
  "Return non-nil if CODE is a valid short open location code.

Note that nil means the code is either not short, or it is
invalid."
  (condition-case nil
      (olc-parse-short (olc-parse-code code))
    (olc-parse-error nil)))

(defun olc-is-full (code)
  "Return non-nil if CODE is a valid long open location code.

Note that nil means the code is either not long, or it is
invalid."
  (condition-case nil
      (not (olc-parse-short (olc-parse-code code)))
    (olc-parse-error nil)))

(defun olc-code-precision (code)
  "Return the precision of CODE."
  (condition-case nil
      (olc-parse-precision (olc-parse-code code))
    (olc-parse-error nil)))

(defun olc-decode (code)
  "Decode open location code CODE.

Returns an `olc-area' structure. Raises `olc-parse-error' if the
code can't be parsed, and `olc-decode-error' if it can't be
decoded (e.g. a padded shortened code, a padded code with grid
coordinates, an empty code, and so forth).

Since this function uses floating point calculations, the results
are not identical to e.g. the C++ reference implementation. The
differences, however, are extremely small."
  (let ((parse (olc-parse-code code))
	(lat -90.0)
	(lon -180.0)
	(size 20.0))

    ;; We only deal with long codes
    (when (olc-parse-short parse)
      (signal 'olc-decode-error code))

    ;; Process the pairs
    (mapc (lambda (pair)
	    (setq lat (+ lat (* size (olc-digit-value (car pair))))
		  lon (+ lon (* size (olc-digit-value (cdr pair))))
		  width size
		  height size
		  size (/ size 20.0)))
	  (olc-parse-pairs parse))

    ;; Process the grid
    (when (olc-parse-grid parse)
      (mapc (lambda (refine)
	      (setq width (/ width 4.0) height (/ height 5.0))
	      (let ((coord (olc-digit-value refine)))
		(setq lat (+ lat (* height (/ coord 4)))
		      lon (+ lon (* width (% coord 4))))))
	    (olc-parse-grid parse)))
    (olc-area-create :latlo lat :lonlo lon :lathi (+ lat height) :lonhi (+ lon width))))


(defun olc-encode (lat lon len)
  "Encode LAT and LON as a LEN length open location code.

The length is automatically clipped to between 2 and
15. `olc-encode-error' is raised if the length is otherwise
invalid (i.e. 3, 5, 7, or 9).

Returns an olc-area structure. Raises olc-encode-error if the
values cannot (legally) be encoded to the selected length."
  (setq len (max 2 (min 15 len)))
  (when (and (< len 11) (/= (% len 2) 0))
    (signal 'olc-encode-error "invalid encoding length"))

  (setq lat (olc-normalize-latitude lat len)
        lon (olc-normalize-longitude lon))

  (let ((code nil)
        (invpreclat (* (expt 20 3) (expt 5 5)))
        (invpreclon (* (expt 20 3) (expt 4 5))))

    ;; Convert lat and lon to integers for the computation
    (setq lat (truncate (+ (* invpreclat lat) (* invpreclat 90)))
          lon (truncate (+ (* invpreclon lon) (* invpreclon 180))))

    ;; Calculate the grid part if needed
    (if (> len 10)
        (dotimes (i 5)
          (setq code (cons (olc-value-digit (+ (* (% lat 5) 4) (% lon 4)))
                           code)
                lat (truncate lat 5)
                lon (truncate lon 4)))
      (setq lat (truncate lat (expt 5 5))
            lon (truncate lon (expt 4 5))))

    ;; Calculate the pairs
    (dotimes (i 5)
      (when (eq i 1) (setq code (cons ?+ code)))
      (setq code (cons (olc-value-digit (% lon 20)) code))
      (setq code (cons (olc-value-digit (% lat 20)) code))
      (setq lat (truncate lat 20)
            lon (truncate lon 20)))

    ;; Truncate the code and add padding
    (let ((truncate (< len 8)))
      (setcdr (nthcdr (- len (if truncate 1 0)) code)
              (nconc (make-list (max 0 (- 8 len)) ?0)
                     (when truncate (list ?+)))))

    (apply 'string code)))


(defun olc-recover (code lat lon &optional format)
  "Recover shortened code CODE from coordinates LAT and LON.

Recovers the closest point to coordinates LAT and LON with a code
that can be shortened to CODE. If FORMAT is `latlon', then the
center of the recovered area (LATITUDE . LONGITUDE) is returned.
If FORMAT is `area' (or any other value), the returned value is an
full open location code."
  (let ((parse (olc-parse-code code)))
    (if (olc-is-full parse)
        (upcase code)
      (setq lat (olc-clip-latitude lat)
            lon (olc-normalize-longitude lon))
      (let* ((padlen (- (olc-parse-precision parse)
                        (* 2 (length (olc-parse-pairs parse)))
                        (length (olc-parse-grid parse))))
             (resolution (expt 20 (- 2 (/ padlen 2))))
             (half-resolution (/ resolution 2.0))
             (area (olc-decode (concat (substring (olc-encode lat lon 10) 0 padlen) code))))
        (cond ((and (< (+ lat half-resolution) (olc-area-lat area))
                    (>= (- (olc-area-lat area) resolution) -90))
               (setq lat (- (olc-area-lat area) resolution)))
              ((and (> (- lat half-resolution) (olc-area-lat area))
                    (<= (+ (olc-area-lat area) resolution) 90))
               (setq lat (+ (olc-area-lat area) resolution)))
              (t (setq lat (olc-area-lat area))))
        (cond ((< (+ lon half-resolution) (olc-area-lon area))
               (setq lon (- (olc-area-lon area) resolution)))
              ((> (- lon half-resolution) (olc-area-lon area))
               (setq lon (+ (olc-area-lon area) resolution)))
              (t (setq lon (olc-area-lon area))))
        (cond ((eq format 'latlon) (cons lat lon))
              (t (olc-encode lat lon (olc-parse-precision parse))))))))


(defun olc-shorten (code lat lon &optional limit)
  "Attempt to shorten CODE with reference LAT and LON.

Shorten CODE, which must be a full open location code, using
latitude LAT and longitude LON as the reference. If LIMIT is
specified, then the code will be shortened by at most that many
digits. If the code can't be shortened, the original code is
returned. `olc-shorten-error' is raised if CODE is a padded or
shortened code, of if LIMIT is not positive and even."
  (let* ((parse (olc-parse-code code))
         (area (olc-decode parse)))
    (when (null limit) (setq limit 12))
    (unless (and (> limit 0) (= 0 (% limit 2)))
      (signal 'olc-shorten-error (list "limit must be even and positive" code)))
    (when (olc-is-short parse)
      (signal 'olc-shorten-error (list "can't shorten shortened codes" code)))
    (when (< (olc-parse-precision parse) 8)
      (signal 'olc-shorten-error (list "can't shorten padded codes" code)))

    (setq lat (olc-clip-latitude lat)
          lon (olc-normalize-longitude lon))

    (let ((coderange (max (abs (- (olc-area-lat area) lat))
                          (abs (- (olc-area-lon area) lon)))))
      (catch 'break
        (dolist (spec '((4 . 0.0025) (3 . 0.05) (2 . 1) (1 . 20)))
          (when (< coderange (* (cdr spec) 0.3))
            (throw 'break (substring code (min limit (* (car spec) 2))))))
        code))))

(defun olc-recover-string (string &optional reference format)
  "Recover a location from a shortened open location code and reference.

When called with one argument, it must be a string consisting of a
shortened open location code followed by whitespace and a geographical
location.

When called with two strings, the first must be a shortened open
location code and the second if the geographical location.

Optionally, the last argument in either case can be a symbol
indicating the format of the return value (see `olc-recover' for
details)."
  (unless (fboundp 'request)
    (error "request library is not loaded"))
  (let (code resp)
    (cond ((and (stringp string) (not (stringp reference)))
           (setq format reference)
           (if (string-match "^\\(\\S-+\\)\\s-+\\(.*\\)$" string)
               (setq code (match-string 1 string)
                     reference (match-string 2 string))
             (signal 'wrong-type-argument string)))
          ((and (stringp string) (stringp reference))
           (setq code string))
          (t (signal 'wrong-type-argument string)))
    (setq resp (request "https://nominatim.openstreetmap.org/search"
                 :params `((q . ,reference)
                           (format . "json")
                           (limit . 1))
                 :parser 'json-read
                 :sync t))
    (when (eq 200 (request-response-status-code resp))
      (olc-recover code
                   (string-to-number (alist-get 'lat (elt (request-response-data resp) 0)))
                   (string-to-number (alist-get 'lon (elt (request-response-data resp) 0)))
                   format))))
