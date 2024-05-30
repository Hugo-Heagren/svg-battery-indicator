;;; svg-battery-indicator.el --- Display an SVG battery icon  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Hugo Heagren

;; Author: Hugo Heagren <hugo@heagren.com>
;; Keywords: convenience, image
;; Version: 1.0
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

(defcustom svg-battery-indicator-stroke-face 'mode-line
  "Face used for battery image borders."
  :group 'svg-battery-indicator
  :type 'face)

(defcustom svg-battery-indicator-fill-face 'mode-line-inactive
  "Face used to fill battery image based on percent."
  :group 'svg-battery-indicator
  :type 'face)

(defun svg-battery-indicator--battery (base-height base-length lug-width stroke-width rounding-radius)
  "Generate a basic battery outline.

The battery will have a height of BASE-HEIGHT, and will be
BASE-LENGTH + LUG-WIDTH long, where LUG-WIDTH is the width of the
lug (nub, terminal) at one end of the battery.  Additionally, it
will be drawn with STROKE-WIDTH wide strokes, and corners rounded
by ROUNDING-RADIUS."
  (let ((svg  (svg-create (+ lug-width base-length) base-height
			  :stroke-width stroke-width
			  :stroke-color (face-attribute svg-battery-indicator-stroke-face :foreground nil 'inherit))))
    ;; Base rectangle
    (svg-rectangle svg lug-width 0 base-length base-height
		   :fill "transparent"
		   :rx rounding-radius :ry rounding-radius)
    ;; End `nub'
    (let ((height (round (* 0.5 base-height))))
      (svg-rectangle svg 0 (/ (- base-height height) 2) lug-width height
		     :fill (face-attribute svg-battery-indicator-stroke-face :foreground nil 'inherit)
                     :stroke-width 0
		     :rx 2 :ry 2))
    svg))

(defun svg-battery-indicator (percentage &optional charging)
  "Return an SVG image descriptor of a battery.

PERCENTAGE is the percentage of current charge, as an integer.
If CHARGING is non-nil a lightning symbol is drawn over the SVG."
  ;; Use colors from `battery.el' faces for charge states
  (let* ((base-height (- (frame-char-height) 2))
	 (base-length svg-battery-indicator-length)
	 (stroke-width 2)               ;TODO: What is the fraction of either height or length that should be used?
	 (lug-width (round (* 1.5 stroke-width)))
	 (rounding-radius 4)            ;TODO: See above
	 (svg (svg-battery-indicator--battery base-height base-length lug-width stroke-width rounding-radius)))

    (if (stringp percentage)
        (svg-text svg "?"
                  :fill (face-attribute svg-battery-indicator-stroke-face :foreground nil 'inherit)
                  :font-weight "bold"
                  :font-size (- base-height stroke-width)
                  :stroke-width 0
                  :x (/ base-length 2)
                  :y (- base-height (* 1.5 stroke-width)))
      (let ((color (face-attribute
		    (cond
		     (charging 'success)
                     ((<= percentage battery-load-critical)
		      'battery-load-critical)
		     ((<= percentage battery-load-low)
		      'battery-load-low)
		     (t svg-battery-indicator-fill-face))
		    :foreground nil 'inherit)))
        ;; Fill/percentage rectangle
        (let* ((os (* 2 stroke-width))  ;TODO: Rename, what does `os' stand for?
	       (length (* (- base-length os)
		          (/ percentage 100.0)))
	       (clip-path (svg-clip-path svg :id "clippath")))
          ;; Clipping path
          (svg-rectangle clip-path
		         (+ lug-width stroke-width (- base-length os length)) stroke-width
		         length (- base-height os))
          ;; We `draw' the whole thing, but also apply a clipping path
          ;; to make it the right length.
          (svg-rectangle svg (+ lug-width stroke-width) stroke-width (- base-length os) (- base-height os)
		         :fill color :rx (- rounding-radius 1) :ry (- rounding-radius 1)
		         :stroke-width 0 :clip-path "url(#clippath)"))
        ;; Only draw if we are actually charging
        (when charging
	  ;; Draw a lightning shape over the battery
	  (let ((half-length (+ lug-width (/ base-length 2)))
	        (half-height  (/ base-height 2)))
	    (svg-path svg
		      `((moveto ((,half-length . 0)))
		        (lineto ((,(- half-length stroke-width)  . ,half-height)))
		        (lineto ((,(+ half-length stroke-width)  . ,half-height)))
		        (lineto ((,half-length . ,base-height))))
		      :stroke-width stroke-width
		      :fill "transparent")))))
    ;; Return the image, centered
    (svg-image svg :ascent 'center)))

(defun svg-battery-indicator-status-advice (data)
  "Add an SVG icon associated with ?i in DATA.

Get percentage and charging state from DATA, and pass these to
`svg-battery-indicator' to get an svg.  Return an alist like
DATA, but also including an association of ?i and the returned
SVG.

This has the effect of making the expando \"%i\" available in
`battery-mode-line-format', expanding to a battery SVG."
  (let* ((percentage (car (read-from-string (cdr (assq ?p data)))))
	 (charging (string= (alist-get ?b data) "+"))
	 (svg (svg-battery-indicator percentage charging))
	 ;; NOTE This string has to be non-empty
	 (str (propertize " " 'display svg)))
    `(,@data (?i . ,str))))

(provide 'svg-battery-indicator)
;;; svg-battery-indicator.el ends here

