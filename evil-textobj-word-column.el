;;; evil-textobj-word-column.el --- Provides a column text object.

;; Author: Lit Wakefield <noct@openmailbox.org>
;; URL: https://github.com/noctuid/evil-textobj-word-column
;; Created: October 7, 2015
;; Keywords: evil, column, text-object
;; Package-Requires: ((names "0.5") (emacs "24") (cl-lib "0.5"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package is a port of the vim textobj-word-column plugin. It provides a
;; text object for selecting a column whose width is defined by an evil word
;; or WORD. This can be a convenient way to start a visual block selection.

;; For more information see the README in the github repo.

;;; Code:
(require 'cl-lib)

;; `define-namespace' is autoloaded, so there's no need to require
;; `names'. However, requiring it here means it will also work for
;; people who don't install through package.el.
(eval-when-compile (require 'names))

(define-namespace evil-textobj-word-column-
:package evil-textobj-word-column
:group evil

(defun -get-basis (big-word)
  "Move to the start of a column delimiter and return type of the column start.
This function determines the left boundary of the column and the anchor position
for searching for the other column boundaries. If BIG-WORD is non-nil, the basis
will be the start of an evil WORD.
Return nil if no suitable column basis found."
  (if (or (and (looking-at (rx (1+ whitespace)))
               (looking-back (rx bol (0+ whitespace)))))
      nil
    ;; move to start of word if not at start of word
    (when (or (looking-at (rx (0+ whitespace) eol))
              (let ((current-pos (point)))
                (save-excursion
                  (cond (big-word
                         (evil-backward-WORD-begin)
                         (evil-forward-WORD-begin))
                        (t
                         (evil-backward-word-begin)
                         (evil-forward-word-begin)))
                  (not (= current-pos (point))))))
      (if big-word
          (evil-backward-WORD-begin)
        (evil-backward-word-begin)))
    (cond ((looking-back (rx bol))
           'bol)
          ((looking-back (rx whitespace))
           'whitespace)
          (t
           'char))))

(defun -get-bounds (behind-char-type movement-function big-word
                                     &optional max-right-col)
  "Helper function to get the bounds in a direction.
BEHIND-CHAR-TYPE is used as reference to determine where the column ends.
MOVEMENT-FUNCTION is a function to be called (to move up or down).
If BIG-WORD is non-nil, the width of the colum will be that of an evil WORD.
MAX-RIGHT-COL is specified when a previous maximum column position (determined
by the end the word at each line) has been found.
Return a list of of buffer positions designating the column bounds where the
first is the upper left bound and the second is the lower right bound."
  (let ((initial-col (current-column))
        (max-right-col (or max-right-col 0))
        max-pos)
    (cl-flet ((continue-check-function
               (cond ((eq behind-char-type 'whitespace)
                      (lambda () (and (looking-back (rx whitespace))
                                      (not (looking-back (rx bol)))
                                      (not (looking-at (rx eol))))))
                     ;; consider looking at front char too
                     ((eq behind-char-type 'bol)
                      (lambda () (not (looking-at (rx (0+ whitespace) eol)))))
                     ((eq behind-char-type 'char)
                      (lambda ()
                        (cond (big-word
                               (evil-backward-WORD-begin)
                               (evil-forward-WORD-begin))
                              (t
                               (evil-backward-word-begin)
                               (evil-forward-word-begin)))
                        (= initial-col (current-column)))))))
      (save-excursion
        (while (progn
                 (let* ((current-pos (point))
                        (right-col
                         (if (or
                              ;; handles case when at 1 char word at eol
                              (looking-at (rx any eol))
                              (save-excursion
                                (if big-word
                                    (evil-forward-WORD-begin)
                                  (evil-forward-word-begin))
                                ;; handles case where at 1 char word otherwise
                                (= (point) (1+ current-pos))))
                             (current-column)
                           (save-excursion
                             (if big-word
                                 (evil-forward-WORD-end)
                               (evil-forward-word-end))
                             (current-column)))))
                   (setq max-pos current-pos)
                   (when (> right-col max-right-col)
                     (setq max-right-col right-col)))
                 (and (condition-case err
                          (progn (funcall movement-function) t)
                        ('error nil))
                      (= initial-col (current-column))
                      (continue-check-function))))))
    (list max-pos max-right-col)))

(defun -get-top-bounds (behind-char-type big-word &optional max-right-col)
  "Find the upper bounds of a column.
See `evil-textobj-word-column--get-bounds' for documentation of
BEHIND-CHAR-TYPE, BIG-WORD, and MAX-RIGHT-COL."
  (-get-bounds behind-char-type #'evil-previous-line big-word max-right-col))

(defun -get-bottom-bounds (behind-char-type big-word &optional max-right-col)
  "Find the lower bounds of a column.
See `evil-textobj-word-column--get-bounds' for documentation of
BEHIND-CHAR-TYPE, BIG-WORD, and MAX-RIGHT-COL."
  (-get-bounds behind-char-type #'evil-next-line big-word max-right-col))

(defun -create-range (big-word)
  "Return a column range based on the point.
If BIG-WORD is non-nil, use an evil WORD for the width of the column."
  ;; this is necessary so c and d don't alter evil-next/previous-line:
  (evil-normal-state)
  (save-excursion
    (let* ((behind-char-type (-get-basis big-word)) ; ensures at word start
           (top-bounds (when behind-char-type
                         (-get-top-bounds behind-char-type big-word)))
           (top-left-pos (when top-bounds
                           (car top-bounds)))
           (max-right-col (when top-bounds
                            (cadr top-bounds)))
           (bottom-bounds (when top-bounds
                            (-get-bottom-bounds behind-char-type big-word
                                                max-right-col)))
           (bottom-right-pos (when bottom-bounds
                               (goto-char (car bottom-bounds))
                               ;; case where beyond eol?
                               (evil-goto-column (1+ (cadr bottom-bounds)))
                               (point))))
      (when (and top-left-pos bottom-right-pos)
        ;; note:
        ;; 'rectangle will make selection one char to the left (so 1+ above)
        ;; 'block gives more incorrect selection
        (evil-range top-left-pos bottom-right-pos 'rectangle)))))

(evil-define-text-object evil-textobj-word-column-inner-column
  (count &optional beg end type)
  "Select a word column.
COUNT, BEG, END, and TYPE have no effect. This text object cannot take a count."
  (-create-range nil))

(evil-define-text-object evil-textobj-word-column-inner-COLUMN
  (count &optional beg end type)
  "Select a word column.
COUNT, BEG, END, and TYPE have no effect. This text object cannot take a count."
  (-create-range t))

;; end of namespace
)

(provide 'evil-textobj-word-column)
;;; evil-textobj-word-column.el ends here
