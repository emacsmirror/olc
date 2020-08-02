;;; olc.el --- Open location code library -*-lexical-binding: t;-*-

;; Copyright (C) 2020 David Byers
;;
;; Author: David Byers <david.byers@liu.se>
;; Version: 1.4.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: extensions, lisp
;; URL: https://gitlab.liu.se/davby02/olc
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

;;; Commentary:

;; This program provides basic open location code support in Emacs
;; Lisp.  The support for recovering shortened codes depends on the
;; request library and uses OpenStreetMap; please check the terms of
;; use for the service to ensure that you remain compliant.
;;
;; All methods required by the open location code specification are
;; provided in some form.  The implementation passed the tests present
;; in the open location code github repository at the time of writing
;; almost cleanly -- there are some minor rounding issues in decode.

;;; Code:

;; This is me being dragged kicking and screaming into the 21st
;; century because the alternative is to cl-lib is to include my own
;; structured data code (which would be overkill) or do it manually
;; (which is a pain in the backside). So cl-lib it is.

(require 'cl-lib)
(require 'request nil t)
(require 'json nil t)

;; Silence compiler if request is not on load-path at compile time.

(declare-function request "ext: request" t t)
(declare-function request-response-status-code "ext: request" t t)
(declare-function request-response-data "ext: request" t t)


;;; Variables:


(defvar olc-nominatim-url "https://nominatim.openstreetmap.org/"
  "Base url for the nominatim endpoint.")


;;; Custom errors:


(define-error 'olc-error "Open location code error")

(define-error 'olc-parse-error
  "Error parsing open location code" 'olc-error)

(define-error 'olc-parse-error-unexpected-end
  "Unexpected end parsing open location code"
  'olc-parse-error)

(define-error 'olc-parse-error-invalid-character
  "Invalid character parsing open location code"
  'olc-parse-error)

(define-error 'olc-parse-error-missing-plus
  "Missing plus sign parsing open location code"
  'olc-parse-error)

(define-error 'olc-parse-error-invalid-padding
  "Invalid padding parsing open location code"
  'olc-parse-error)

(define-error 'olc-parse-error-padded-shortcode
  "Padded  short code parsing open location code"
  'olc-parse-error)

(define-error 'olc-parse-error-digit-after-padding
  "Unexpected digit after padding parsing open location code"
  'olc-parse-error)

(define-error 'olc-parse-error-empty-code
  "Empty code when parsing open location code"
  'olc-parse-error)

(define-error 'olc-decode-error
  "Error decoding open location code"
  'olc-error)

(define-error 'olc-decode-error-shortcode
  "Short codes must be recovered before decoding"
  'olc-decode-error)

(define-error 'olc-shorten-error
  "Error shortening open location code."
  'olc-error)

(define-error 'olc-shorten-error-shortcode
  "Code is already shortened"
  'olc-shorten-error)

(define-error 'olc-shorten-error-padded
  "Unable to shorten padded codes"
  'olc-shorten-error)

(define-error 'olc-recover-error
  "Error recovering open location code."
  'olc-error)

(define-error 'olc-recover-error-reference-search-failed
  "Reference location search failed"
  'olc-recover-error)

(define-error 'olc-recover-error-reference-not-found
  "Reference location not found"
  'olc-recover-error)

(define-error 'olc-recover-error-invalid-reference
  "Invalid reference location"
  'olc-recover-error)


;;; Base 20 digits:


(defconst olc-value-mapping "23456789CFGHJMPQRVWX"
  "Mapping from values to olc base 20 digits.")

(defconst olc-digit-mapping
  (let ((count 0))
    (mapcan (lambda (letter)
              (prog1 (list (cons letter count)
                           (cons (downcase letter) count))
                (setq count (1+ count))))
            olc-value-mapping))
  "Mapping from olc base 20 digits to values.")

(defsubst olc-digit-value (digit)
  "Return the base 20 value of DIGIT."
  (cdr (assq digit olc-digit-mapping)))

(defsubst olc-value-digit (value)
  "Return the digit for a VALUE up to 19."
  (elt olc-value-mapping value))


;;; Data structures:


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
  "Get center latitude of AREA."
  (min (+ (/ (- (olc-area-lathi area) (olc-area-latlo area)) 2)
          (olc-area-latlo area))
       90))

(defsubst olc-area-lon (area)
  "Get center longitude of AREA."
  (min (+ (/ (- (olc-area-lonhi area) (olc-area-lonlo area)) 2)
          (olc-area-lonlo area))
       180))


;;; (Mostly) internal functions and variables


(defmacro olc-valid-char (char)
  "Check if CHAR is a valid OLC digit."
  `(assq ,char olc-digit-mapping))

(defmacro olc-transform-error (spec &rest body)
  "Catch some errors and throw others.

SPEC is a list consisting of an error to catch, the error to
raise, and args for the raised error.

\(fn (CATCH SIGNAL &rest ARGS) BODY...)"
  (declare (indent 1) (debug (listp &rest form)))
  `(condition-case nil
       ,@body
     (,(elt spec 0) (signal ',(elt spec 1) (list ,@(cddr spec))))))

(defsubst olc-position-of (char code)
  "Find the leftmost position of CHAR in CODE."
  (let ((index 0))
    (catch 'result
      (mapc (lambda (letter)
              (when (= char letter)
                (throw 'result index))
              (setq index (1+ index)))
            code))))

(defun olc-nominatim-endpoint (path)
  "Build a complete url for nominatim endpoint PATH."
  (concat olc-nominatim-url
          (if (= ?/ (elt olc-nominatim-url (1- (length olc-nominatim-url))))
              "" "/")
          path))

(defsubst olc-clip-latitude (lat)
  "Clip LAT to -90,90."
  (max -90 (min 90 lat)))

(defsubst olc-normalize-latitude (lat len)
  "Normalize latitude LAT for a LEN character log code."
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

(cl-defun olc-parse-code (code)
  "Parse an open location code CODE.

This function changes the match data."
  (if (olc-parse-p code)
      code
    (cl-check-type code stringp)
    (save-match-data
      (let ((pos 0)
            (pairs nil)
            (short nil)
            (precision nil)
            (grid nil)
            (padding 0))

        ;; Parse up to four initial pairs
        (catch 'break
          (while (< pos (length code))
            (olc-transform-error
                (args-out-of-range olc-parse-error-unexpected-end
                                   code (1+ pos))
              (cond ((eq (elt code pos) ?+) (throw 'break nil))
                    ((eq (elt code pos) ?0) (throw 'break nil))
                    ((= (length pairs) 4) (throw 'break nil))
                    ((not (olc-valid-char (elt code pos)))
                     (signal 'olc-parse-error-invalid-character
                             (list code pos (string (elt code pos)))))
                    ((not (olc-valid-char (elt code (1+ pos))))
                     (signal 'olc-parse-error-invalid-character
                             (list code (1+ pos)
                                   (string (elt code (1+ pos))))))
                    (t (setq pairs (cons (cons (elt code pos)
                                               (elt code (1+ pos)))
                                         pairs)))))
            (setq pos (+ pos 2))))

        ;; Measure the padding
        (when (eq pos (string-match "0+" code pos))
          (setq pos (match-end 0)
                padding (- (match-end 0) (match-beginning 0))))

        ;; Parse the separator
        (olc-transform-error
            (args-out-of-range olc-parse-error-unexpected-end
                               code pos)
          (if (eq (elt code pos) ?+)
              (setq pos (1+ pos))
            (signal 'olc-parse-error-missing-plus
                    (list code pos))))

        ;; Check the length of the padding
        (unless (and (= (% padding 2) 0)
                     (<= (+ padding (* 2 (length pairs))) 8))
          (signal 'olc-parse-error-invalid-padding
                  (list code pos)))

        ;; Determine if the code is shortened or not
        (setq short (< (+ (* 2 (length pairs)) padding) 8))

        ;; We cant be short and have padding (not sure why)
        (when (and short (> padding 0))
          (signal 'olc-parse-error-padded-shortcode
                  (list code pos)))

        ;; Determine the precision of the code
        (setq precision (- 8 padding))

        ;; Parse what's after the separator
        (when (< pos (length code))
          (when (> padding 0)
            (signal 'olc-parse-error-digit-after-padding
                    (list code pos (string (elt code pos)))))

          ;; Parse one more pair
          (olc-transform-error (args-out-of-range
                                olc-parse-error-unexpected-end
                                code (1+ pos))
            (cond ((not (olc-valid-char (elt code pos)))
                   (signal 'olc-parse-error-invalid-character
                           (list code pos (string (elt code pos)))))
                  ((not (olc-valid-char (elt code (1+ pos))))
                   (signal 'olc-parse-error-invalid-character
                           (list code (1+ pos) (string (elt code (1+ pos))))))
                  (t (setq pairs (cons (cons (elt code pos)
                                             (elt code (1+ pos)))
                                       pairs)
                           pos (+ 2 pos)
                           precision (+ 2 precision))))))

          ;; Parse grid
          (while (< pos (length code))
            (cond ((not (olc-valid-char (elt code pos)))
                   (signal 'olc-parse-error-invalid-character
                           (list code pos (string (elt code pos)))))
                  ((>= (length grid) 5) (setq pos (1+ pos)))
                  (t (setq grid (cons (elt code pos) grid)
                           pos (1+ pos)
                           precision (1+ precision)))))

        ;; Check for an empty code
        (unless pairs
          (signal 'olc-parse-error-empty-code (list code 0)))

        ;; Return the result
        (olc-parse-create :pairs (nreverse pairs)
                          :grid (nreverse grid)
                          :short short
                          :precision precision)))))


;;; Public functions:

(defconst olc-code-regexp (format "^\\([%s]*\\)\\(0*\\)\\+\\([%s]*\\)$"
                                  olc-value-mapping
                                  olc-value-mapping)
  "Regular expression for parsing codes.")


(cl-defun olc-is-valid (code &key compound)
  "Return non-nil if CODE is a valid open location code.

If compound is non-nil, then return non-nil if CODE looks like a
compound open location code (i.e. everything up to the first
space character is a valid code)."
    (or (olc-parse-p code)
        (save-match-data
          (when (and compound (string-match "\\s-+" code))
            (setq code (substring code 0 (match-beginning 0))))
          (let ((case-fold-search t))

            ;; The code is decomposed into PAIRS PADDING "+" SUFFIX.
            ;;
            ;; Rules:
            ;;
            ;; - For all codes:
            ;;   - Pairs has an even (zero counts) length of at most 8.
            ;;   - Suffix is either zero or between 2 and 8 characters.
            ;;   - One or both of pairs and suffix must not be empty.
            ;;
            ;; - If there is padding:
            ;;   - The suffix must be empty
            ;;   - The length of pairs and padding combined must be 8

            (when (string-match olc-code-regexp code)
              (let ((pair-len (- (match-end 1) (match-beginning 1)))
                    (padd-len (- (match-end 2) (match-beginning 2)))
                    (suff-len (- (match-end 3) (match-beginning 3))))
                (and (and (= 0 (% pair-len 2)) (<= pair-len 8)) ; Check pairs
                     (and (<= suff-len 8) (/= suff-len 1)) ; Check suffix
                     (> (+ pair-len suff-len) 0) ; Check for not empty
                     (or (= padd-len 0)          ; Empty padding...
                         (and (= suff-len 0)     ; ...or suffix
                              (= (+ padd-len pair-len) 8))))))))))

(cl-defun olc-is-short (code &key compound)
  "Return non-nil if CODE is a valid short open location code.

If COMPOUND is non-nil, then return non-nil if CODE looks like a
compound open location code (i.e. everything up to the first
space character is a valid short code).

Note that nil means the code is either not short, or it is
invalid."
  (if (olc-parse-p code)
      (olc-parse-short code)
    (and (olc-is-valid code :compound compound)
         (or (< (length code) 9)
             (and (>= (length code) 9)
                  (not (= (elt code 8) ?+)))))))


(cl-defun olc-is-full (code &key compound)
  "Return non-nil if CODE is a valid long open location code.

If compound is non-nil, then return non-nil if CODE looks like a
compound open location code (i.e. everything up to the first
space character is a valid short code).

Note that nil means the code is either not long, or it is
invalid."
  (if (olc-parse-p code)
      (not (olc-parse-short code))
    (and (olc-is-valid code :compound compound)
         (and (>= (length code) 9)
              (= (elt code 8) ?+)))))


(defun olc-code-precision (code)
  "Return the precision of CODE."
  (condition-case nil
      (olc-parse-precision (olc-parse-code code))
    (olc-parse-error nil)))


(cl-defun olc-encode (lat lon &key (len 10))
  "Encode LAT and LON as a LEN length open location code.

LEN is automatically clipped to between 2 and 15. Invalid values
raise an error."
  (cl-check-type lat number)
  (cl-check-type lon number)
  (cl-check-type len integer)

  (setq len (max 2 (min 15 len)))
  (cl-check-type len (member 2 4 6 8 10 11 12 13 14 15))

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
        (dotimes (_ 5)
          (setq code (cons (olc-value-digit
                            (+ (* (% lat 5) 4) (% lon 4)))
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

    (apply #'string code)))


(cl-defun olc-decode (code &key (format 'area))
  "Decode open location code CODE.

Returns an `olc-area' structure.  Raises `olc-parse-error' if the
code can't be parsed, and `olc-decode-error' if it can't be
decoded (e.g. a padded shortened code, a padded code with grid
coordinates, an empty code, and so forth).

If FORMAT is `area' (the default), the returned value is an full
open location code. If FORMAT is `latlon' it is a list (LATITUDE
LONGITUDE) representing the center of the location.

Since this function uses floating point calculations, the results
are not identical to e.g. the C++ reference implementation.  The
differences, however, are extremely small."
  (cl-check-type code (or stringp olc-parse))
  (cl-check-type format (member latlon area))

  (let* ((parse (olc-parse-code code))
         (latscale (* (expt 20 4) (expt 5 5)))
         (lonscale (* (expt 20 4) (expt 4 5)))
         (lat (* latscale -90))
         (lon (* lonscale -180))
         (latsize (* latscale 20))
         (lonsize (* lonscale 20)))

    ;; We only deal with long codes
    (when (olc-parse-short parse)
      (signal 'olc-decode-error-shortcode (list code)))

    ;; Process the pairs
    (mapc (lambda (pair)
            (setq lat (+ lat (* latsize (olc-digit-value (car pair))))
                  lon (+ lon (* lonsize (olc-digit-value (cdr pair))))
                  latsize (/ latsize 20)
                  lonsize (/ lonsize 20)))
          (olc-parse-pairs parse))

    ;; I'm too tired to figure out why
    (setq latsize (* latsize 20) lonsize (* lonsize 20))

    ;; Process the grid
    (when (olc-parse-grid parse)
      (mapc (lambda (refine)
              (setq latsize (/ latsize 5) lonsize (/ lonsize 4))
              (let ((coord (olc-digit-value refine)))
                (setq lat (+ lat (* latsize (/ coord 4)))
                      lon (+ lon (* lonsize (% coord 4))))))
            (olc-parse-grid parse)))

    (if (eq format 'area)
        (olc-area-create :latlo (/ lat (float latscale))
                         :lonlo (/ lon (float lonscale))
                         :lathi (/ (+ lat latsize) (float latscale))
                         :lonhi (/ (+ lon lonsize) (float lonscale)))
      (list (/ (+ lat (/ latsize 2)) (float latscale))
            (/ (+ lon (/ lonsize 2)) (float lonscale))))))


(cl-defun olc-shorten (code lat lon &key (limit 12))
  "Attempt to shorten CODE with reference LAT and LON.

Shorten CODE, which must be a full open location code, using
latitude LAT and longitude LON as the reference. If LIMIT is
specified, then the code will be shortened by at most that many
digits. If the code can't be shortened, the original code is
returned. `olc-shorten-error' is raised if CODE is a padded or
shortened code, of if LIMIT is not positive and even."
  (cl-check-type lat number)
  (cl-check-type lon number)
  (cl-check-type limit (member 2 4 6 8 10 12))

  (when (olc-is-short code)
    (signal 'olc-shorten-error-shortcode
            (list code)))

  (let* ((parse (olc-parse-code code))
         (area (olc-decode parse)))
    (when (< (olc-parse-precision parse) 8)
      (signal 'olc-shorten-error-padded
              (list code)))

    (setq lat (olc-clip-latitude lat)
          lon (olc-normalize-longitude lon))

    (let ((coderange (max (abs (- (olc-area-lat area) lat))
                          (abs (- (olc-area-lon area) lon)))))
      (catch 'break
        (dolist (spec '((4 . 0.0025) (3 . 0.05) (2 . 1) (1 . 20)))
          (when (< coderange (* (cdr spec) 0.3))
            (throw 'break (substring code
                                     (min limit (* (car spec) 2))))))
        code))))


(cl-defun olc-shorten-compound (code &key (limit 4) (zoom '(1 18)))
  "Attempt to shorten CODE with a geographic reference.

Shorten CODE, which must be a full open location code, finding a
reference near the encoded location.

If LIMIT is non-nil, then the code will be shortened by at most
that many digits. The default is to shorten by at most 4
characters.

If ZOOM is non-nil it is either a number, the one zoom level to
explore, or a list (MIN, MAX), where MIN is smallest zoom level
to explore, and MAX the largest.

If the code can't be shortened, the original code is returned.
`olc-shorten-error' is raised if CODE is a padded or shortened
code, of if LIMIT is not positive and even.

This function makes multiple calls to the OpenStreetMap API, so
it can take some time to complete. If you can set the zoom level
to a single number, then it will make one call only, and is much
faster.
"
  (cl-check-type code stringp)
  (cl-check-type limit (member 2 4 6 8 10 12))
  (cl-check-type zoom (or integer listp))

  (when (olc-is-short code)
    (signal 'olc-shorten-error-shortcode
            (list code)))

  (let* ((parse (olc-parse-code code))
         (area (olc-decode code))
         (zoom-lo (cond ((numberp zoom) zoom)
                        ((listp zoom) (elt zoom 0))
                        (t (signal 'args-out-of-range (list '(1 18) zoom)))))
         (zoom-hi (cond ((numberp zoom) zoom)
                        ((listp zoom) (elt zoom 1))
                        (t (signal 'args-out-of-range (list '(1 18) zoom)))))
         result)

    ;; Check for padding
    (when (< (olc-parse-precision parse) 8)
      (signal 'olc-shorten-error-padded
              (list code)))

    ;; Check that zoom range is not inverted
    (when (or (< zoom-hi zoom-lo)
              (< zoom-hi 1) (> zoom-hi 18)
              (< zoom-lo 1) (> zoom-lo 18))
      (signal 'args-out-of-range (list '(1 18) zoom)))

    ;; Otherwise we may never hit the high limit
    (setq zoom-hi (1+ zoom-hi))

    (catch 'result
      (while (< zoom-lo zoom-hi)
        (let* ((zoom (floor (+ zoom-lo zoom-hi) 2))
               (resp (request-response-data
                      (request
                        (olc-nominatim-endpoint "reverse")
                        :params `((lat . ,(olc-area-lat area))
                                  (lon . ,(olc-area-lon area))
                                  (zoom . ,zoom)
                                  (format . "json"))
                        :parser #'json-read
                        :sync t)))
               (tmp-code
                (when resp
                  (olc-shorten code
                               (string-to-number (alist-get 'lat resp))
                               (string-to-number (alist-get 'lon resp))
                               :limit limit)))
               (padlen
                (when tmp-code (- 8 (olc-position-of ?+ tmp-code)))))

          ;; If resp is nil, then there's no point in going further

          (if (null resp)
              (setq zoom-lo zoom-hi)

            ;; Keep the shortest code we see that has at most limit
            ;; chars removed

            (when (and (<= padlen limit)
                       (or (null result)
                           (< (length tmp-code) (length (car result)))))
              (setq result (cons tmp-code (alist-get 'display_name resp))))

            ;; Zoom in or out
            (if (< padlen limit)
                (setq zoom-lo (1+ zoom))
              (setq zoom-hi zoom)))))
      (if (and result (< (olc-position-of ?+ (car result)) 8))
          (concat (car result) " " (cdr result))
        code))))


(cl-defun olc-recover (code lat lon &key (format 'area))
  "Recover shortened code CODE from coordinates LAT and LON.

Recovers the closest point to coordinates LAT and LON with a code
that can be shortened to CODE. If FORMAT is `area' (the default),
the returned value is an full open location code. If FORMAT is
`latlon' it is a list (LATITUDE LONGITUDE) representing the
center of the location."
  (cl-check-type code stringp)
  (cl-check-type lat number)
  (cl-check-type lon number)
  (cl-check-type format (member area latlon))

  (let ((parse (olc-parse-code code)))
    (if (olc-is-full parse)
        (if (eq format 'latlon)
            (let ((area (olc-decode parse)))
              (list (olc-area-lat area)
                    (olc-area-lon area)))
          (upcase code))
      (setq lat (olc-clip-latitude lat)
            lon (olc-normalize-longitude lon))
      (let* ((padlen (- (olc-parse-precision parse)
                        (* 2 (length (olc-parse-pairs parse)))
                        (length (olc-parse-grid parse))))
             (resolution (expt 20 (- 2 (/ padlen 2))))
             (half-resolution (/ resolution 2.0))
             (area (olc-decode
                    (concat (substring (olc-encode lat lon :len 10)
                                       0 padlen)
                            code))))
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
        (if (eq format 'latlon)
            (list lat lon)
          (olc-encode lat lon :len (olc-parse-precision parse)))))))


(cl-defun olc-recover-compound (code &key ref (format 'area))
  "Recover a location from a compound code CODE.

Optional keyword argument REF indicates the reference to use. If
not specified, the reference is assumed to be embedded into CODE.

If FORMAT is `area' (the default), the returned value is an full
open location code. If FORMAT is `latlon' it is a list (LATITUDE
LONGITUDE) representing the center of the location."
  ;; Make sure we can do requests
  (save-match-data
    (unless (fboundp 'request) (signal 'void-function '(request)))

    ;; Check types (defer check of ref)
    (cl-check-type code stringp)
    (cl-check-type format (member latlon area))

    ;; Process code and check ref
    (cond ((string-match "^\\(\\S-+\\)\\s-+\\(.*\\)$" code)
           (cl-check-type ref null)
           (setq ref (match-string 2 code)
                 code (match-string 1 code)))
          ((olc-is-full code))
          (t (cl-check-type ref stringp)))

    ;; If the code is full then return it
    (if (olc-is-full code)
        (olc-recover code 0 0 :format format)
      (let ((resp (request (olc-nominatim-endpoint "search")
                    :params `((q . ,ref)
                              (format . "json")
                              (limit . 1))
                    :parser #'json-read
                    :sync t)))

        ;; Check that we got a response
        (unless (eq 200 (request-response-status-code resp))
          (signal 'olc-recover-error-reference-search-failed
                  (list code ref)))

        (unless (> (length (request-response-data resp)) 0)
          (signal 'olc-recover-error-reference-not-found
                  (list code ref)))

        (let* ((data (elt (request-response-data resp) 0))
               (lat (alist-get 'lat data))
               (lon (alist-get 'lon data)))

          ;; Check that we have a lat and lon
          (unless (and lat lon)
            (signal 'olc-recover-error-invalid-reference
                    (list code ref)))

          ;; Finally recover the code!
          (olc-recover code
                       (string-to-number lat)
                       (string-to-number lon)
                       :format format))))))


(provide 'olc)

;;; olc.el ends here
