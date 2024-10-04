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

(defcustom svg-battery-indicator-sizes-function
  #'svg-battery-indicator-fixed-sizes
  "Function to determine the sizes of an indicator.

This function must conform to the following specification:

 - It must take three arguments, a character height, battery
   percentage, and whether or not the battery is charging.
 - It must return an alist with the following (symbol) keys:
   - `height' the height of the battery (should generally less
     than the passed character height)
   - `length' the length of the battery.
   - `stroke-width' the width of generated lines.
   - `lug-width' the width of the battery's lug.
   - `lug-height' the height of the battery's lug.
   - `bolt-x' where (horizontally) the lightning bolt should be
     drawn.
   - `bolt-y' Where the elbow in the lightning bold should be
     drawn."
  :group 'svg-battery-indicator
  :type '(choice
          (function-item svg-battery-indicator-fixed-sizes)
          (function-item svg-battery-indicator-proportional-sizes)
          (function :tag "Custom Function")))

(defcustom svg-battery-indicator-length 35
  "Length of the battery indicator in fixed configuration.

Use when `svg-battery-indicator-sizes-function' is
`svg-battery-indicator-fixed-sizes'."
  :group 'svg-battery-indicator
  :type 'number)

(defcustom svg-battery-indicator-rounding-radius 4
  "Rounding radius of the battery drawing in fixed configuration.

Use when `svg-battery-indicator-sizes-function' is
`svg-battery-indicator-fixed-sizes'."
  :group 'svg-battery-indicator
  :type 'number)

(defcustom svg-battery-indicator-stroke-width 2
  "Battery stroke width in fixed configuration.

Use when `svg-battery-indicator-sizes-function' is
`svg-battery-indicator-fixed-sizes'."
  :group 'svg-battery-indicator
  :type 'number)

(defcustom svg-battery-indicator-lug-width 3
  "Width of the battery lug in fixed configuration.

Use when `svg-battery-indicator-sizes-function' is
`svg-battery-indicator-fixed-sizes'."
  :group 'svg-battery-indicator
  :type 'number)

(defcustom svg-battery-indicator-lug-height 8
  "Height of the battery lug in fixed configuration.

Use when `svg-battery-indicator-sizes-function' is
`svg-battery-indicator-fixed-sizes'."
  :group 'svg-battery-indicator
  :type 'number)

(defcustom svg-battery-indicator-length-multiplier 2.2
  "How long (relative to frame character height) to make the indicator.

Use when `svg-battery-indicator-sizes-function' is
`svg-battery-indicator-proportional-sizes'."
  :group 'svg-battery-indicator
  :type 'number)

(defcustom svg-battery-indicator-stroke-width-fraction 0.17
  "Fraction of height the lines should be.

Use when `svg-battery-indicator-sizes-function' is
`svg-battery-indicator-proportional-sizes'."
  :group 'svg-battery-indicator
  :type 'number)

(defcustom svg-battery-indicator-lug-width-fraction 1.5
  "Width of battery lug (relative to stroke width).

Use when `svg-battery-indicator-sizes-function' is
`svg-battery-indicator-proportional-sizes'."
  :group 'svg-battery-indicator
  :type 'number)

(defcustom svg-battery-indicator-lug-height-fraction 0.5
  "Height of the battery lug (relative to char height).

Use when `svg-battery-indicator-sizes-function' is
`svg-battery-indicator-proportional-sizes'."
  :group 'svg-battery-indicator
  :type 'number)

(defcustom svg-battery-indicator-rounding-radius-fraction 1.5
  "Rounding radius for drawing, relative to stroke width.

Use when `svg-battery-indicator-sizes-function' is
`svg-battery-indicator-proportional-sizes'."
  :group 'svg-battery-indicator
  :type 'number)

(defface svg-battery-indicator-stroke-face '((t :inherit mode-line))
  "Face used to draw battery indicator edges.

Note: the foreground property is used."
  :group 'svg-battery-indicator)

(defface svg-battery-indicator-fill-face '((t :inherit mode-line-inactive))
  "Face used to fill battery image based on percent."
  :group 'svg-battery-indicator)


;;; Size Determination Functions

(defun svg-battery-indicator-fixed-sizes (char-height _battery_percent _chargingp)
  "Generate a set of fixed sizes for the battery indicator.

Use CHAR-HEIGHT for the battery height, otherwise, compute as follows:

 - length is `svg-battery-indicator-length'
 - rounding radius is `svg-battery-indicator-rounding-radius'
 - stroke width is `svg-battery-indicator-stroke-width'
 - lug width is `svg-battery-indicator-lug-width'
 - lug height is `svg-battery-indicator-lug-height'

The position of the bolt is based on half the length and half the
height."
  (let* ((base-height (- char-height 2)))
    `((height . ,base-height)
      (length . ,svg-battery-indicator-length)
      (stroke-width . ,svg-battery-indicator-stroke-width)
      (lug-width . ,svg-battery-indicator-lug-width)
      (lug-height . ,svg-battery-indicator-lug-height)
      (rounding-radius . ,svg-battery-indicator-rounding-radius)
      (bolt-x . ,(+ svg-battery-indicator-lug-width (/ svg-battery-indicator-length 2)))
      (bolt-y . ,(/ base-height 2)))))

(defun svg-battery-indicator-proportional-sizes (char-height _battery_percent _chargingp)
  "Generate sizes for the battery indicator based on height.

This uses CHAR-HEIGHT and the following equations to calculate
the sizes of the battery.

 - length is
   base height * `svg-battery-indicator-length-multiplier',
 - stroke width is
   base height * `svg-battery-indicator-stroke-width-fraction'
 - rounding radius is
   stroke width * `svg-battery-indicator-rounding-radius-fraction'
 - lug width is
   stroke width * `svg-battery-indicator-lug-width-fraction'

The position of the bolt is based on half the length and half the
height."
  (let* ((base-height (- char-height 2))
         (base-length (round (* svg-battery-indicator-length-multiplier base-height)))
         (stroke-width (round (* svg-battery-indicator-stroke-width-fraction base-height)))
         (rounding-radius (* svg-battery-indicator-rounding-radius-fraction stroke-width))
         (lug-width (round (* svg-battery-indicator-lug-width-fraction stroke-width)))
         (lug-height (round (* svg-battery-indicator-lug-height-fraction base-height)))
         (bolt-x (+ lug-width (round base-length 2)))
         (bolt-y (round base-height 2)))
    `((height . ,base-height)
      (length . ,base-length)
      (stroke-width . ,stroke-width)
      (lug-width . ,lug-width)
      (lug-height . ,lug-height)
      (rounding-radius . ,rounding-radius)
      (bolt-x . ,bolt-x)
      (bolt-y . ,bolt-y))))


;;; Draw the Battery

(defun svg-battery-indicator (percentage &optional charging)
  "Return an SVG image of a charging battery.

The battery will be filled PERCENTAGE percent (which should be an
integer 0-100).  If CHARGING, a lightning symbol is drawn over
the image.

The appearance of the battery is controlled by
`svg-battery-indicator-sizes-function',
`svg-battery-indicator-stroke-face', and
`svg-battery-indicator-fill-face'."
  (let-alist (funcall svg-battery-indicator-sizes-function
                      (frame-char-height) percentage charging)
    (let ((svg (svg-create (+ .lug-width .length) .height
                           :stroke-width .stroke-width
                           :stroke-color (face-attribute 'svg-battery-indicator-stroke-face
                                                         :foreground nil 'inherit))))

      ;; Draw base rectangle
      (svg-rectangle svg .lug-width 0 .length .height
                     :fill "transparent"
                     :rx .rounding-radius
                     :ry .rounding-radius)

      ;; Draw the battery's lug
      (svg-rectangle svg 0 (round (- .height .lug-height) 2) .lug-width .lug-height
                     :fill (face-attribute 'svg-battery-indicator-stroke-face
                                           :foreground nil 'inherit)
                     :rx .rounding-radius
                     :ry .rounding-radius)

      (if (stringp percentage)
          (svg-text svg "?"
                    :fill (face-attribute 'svg-battery-indicator-stroke-face
                                          :foreground nil 'inherit)
                    :font-weight "bold"
                    :font-size (- .height .stroke-width)
                    :x (round .length 2)
                    :y (- .height (round (* 1.5 .stroke-width))))
        (let ((color (face-attribute
		      (cond
		       (charging 'success)
                       ((<= percentage battery-load-critical)
		        'battery-load-critical)
		       ((<= percentage battery-load-low)
		        'battery-load-low)
		       (t 'svg-battery-indicator-fill-face))
		      :foreground nil 'inherit)))
          (let* ((os (* 2 .stroke-width))
                 (length (* (- .length os)
                            (/ percentage 100.0)))
                 (clip-path (svg-clip-path svg :id "clippath")))
            (svg-rectangle clip-path
                           (+ .lug-width .stroke-width (- .length os length)) .stroke-width
                           length (- .height os))
            (svg-rectangle svg (+ .lug-width .stroke-width) .stroke-width (- .length os) (- .height os)
                           :fill color
                           :rx .rounding-radius
                           :ry .rounding-radius
                           :stroke-width 0
                           :clip-path "url (#clippath)"))
          (when charging
            (svg-path svg
                      `((moveto ((,.bolt-x . 0)))
                        (lineto ((,(- .bolt-x .stroke-width) . ,.bolt-y)))
                        (lineto ((,(+ .bolt-x .stroke-width) . ,.bolt-y)))
                        (lineto ((,.bolt-x . ,.height))))
                      :stroke-width .stroke-width
                      :fill "transparent"))))
      (svg-image svg :ascent 'center))))

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

