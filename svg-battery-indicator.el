;;; svg-battery-indicator.el --- Display an SVG battery icon  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Hugo Heagren

;; Author: Hugo Heagren <hugo@heagren.com>
;; Keywords: convenience, image
;; Version 1.0
;; Package-Requires: ((emacs "25"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple battery indicator

;;; Code:

(require 'svg)
(require 'battery)

(defgroup svg-battery-indicator nil
  "Group for customizing `svg-battery-indicator'."
  :group 'emacs)

(defcustom svg-battery-indicator-length 35
  "Length in pixels of the battery SVG."
  :group 'svg-battery-indicator
  :type 'integer)

(defun svg-battery-indicator (percentage &optional charging)
  "Return an SVG image descriptor of a battery.

PERCENTAGE is the percentage of current charge, as an integer. If
CHARGING is non-nil a lightning symbol is drawn over the SVG."
  ;; Use colors from `battery.el' faces for charge states
  (let* ((color (face-attribute
		 (cond
		  (charging 'success)
		  ((<= percentage battery-load-critical)
		   'battery-load-critical)
		  ((<= percentage battery-load-low)
		   'battery-load-low)
		  (t 'mode-line))
		 :foreground nil 'inherit))
	 (base-ht (- (frame-char-height) 2))
	 (base-len svg-battery-indicator-length)
	 (x-os 3)      ;; Space on the left for `nub'
	 (sw 2)	       ;; stroke-width
	 (rnd 4)       ;; rounding radius
	 ;; Base svg object
	 (svg (svg-create (+ x-os base-len) base-ht
			  :stroke-width sw
			  :stroke-color "white")))
    ;; Base rectangle
    (svg-rectangle svg x-os 0 base-len base-ht
		   :fill "transparent"
		   :rx rnd :ry rnd)
    ;; End `nub'
    (let ((ht 8))
      (svg-rectangle svg 0 (/ (- base-ht ht) 2) x-os ht
		     :fill "white" :stroke-width 0
		     :rx 2 :ry 2))
    ;; Fill/percentage rectangle
    (let* ((os (* 2 sw))
	   (len (* (- base-len os)
		   (/ percentage 100.0)))
	   (clip-path (svg-clip-path svg :id "clippath")))
      ;; Clipping path
      (svg-rectangle clip-path
		     (+ x-os sw (- base-len os len)) sw
		     len (- base-ht os))
      ;; We `draw' the whole thing, but also apply a clipping path
      ;; to make it the right length.
      (svg-rectangle svg (+ x-os sw) sw (- base-len os) (- base-ht os)
		     :fill color :rx (- rnd 1) :ry (- rnd 1)
		     :stroke-width 0 :clip-path "url(#clippath)"))
    ;; Only draw if we are actually charging
    (and charging
	 ;; Draw a lightning shape over the battery
	 (let ((half-len (+ x-os (/ base-len 2)))
	       (half-ht  (/ base-ht 2)))
	   (svg-path svg
		     `((moveto ((,half-len . 0)))
		       (lineto ((,(- half-len 2)  . ,half-ht)))
		       (lineto ((,(+ half-len 2)  . ,half-ht)))
		       (lineto ((,half-len . ,base-ht))))
		     :stroke-width 2
		     :fill "transparent")))
    ;; Return the image, centered
    (svg-image svg :ascent 'center)))

(defun svg-battery-indicator-set-mode-string (data)
  "Set `battery-mode-line-string' to a battery SVG icon.

Get percentage and charging state from DATA, and pass these to
`svg-battery-indicator' to get an svg. Then set this svg as the
display property of a single-space string and set
`battery-mode-line-string' to the result.

If this function is used in `battery-update-functions', then
`display-battery-mode' will display the svg."
  (let* ((percentage (car (read-from-string (cdr (assq ?p data)))))
	 (charging (string-empty-p (alist-get 98 data)))
	 (svg (svg-battery-indicator percentage charging))
	 ;; NOTE This string has to be non-empty
	 (str (propertize " " 'display svg)))
    (setq battery-mode-line-string str)))

(provide 'svg-battery-indicator)
;;; svg-battery-indicator.el ends here

