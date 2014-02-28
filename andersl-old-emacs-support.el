;;; andersl-old-emacs-support.el -- Features needed to use old Emacs versions.

;; Copyright (C) 2014 Anders Lindgren
;; Copyright (C) 1985-1987, 1993-2013 Free Software Foundation, Inc.

;; Author: Anders Lindgren
;; Version: 0.0.1
;; URL: https://github.com/Lindydancer/andersl-old-emacs-support

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides features needed to use my packages on older
;; Emacs versions. Supported Emacs versions are 22.x, 23.x, and 24.x.
;;
;; In addition, this package can patch known problems in earlier Emacs
;; versions, for things that are related to my packages.
;;
;; I have opted to implement backward compatibility support in a
;; separate package, rather than let each package handle it themselves.
;;
;; One reason for this is to make packages simpler to read. Another is
;; to make life easier for FSF in case they would include my packages
;; in future Emacs distributions.

;;; Code:

(eval-when-compile
  (require 'cl))


;; ------------------------------------------------------------
;; Common functions
;;

(unless (fboundp 'user-error)
  (defalias 'user-error 'error))


;; In newer Emacs versions, a window can be adjusted down to the pixel
;; level. In this case `window-width' include partial-width columns
;; whereas `window-text-width' does not.
(unless (fboundp 'window-text-width)
  (defalias 'window-text-width 'window-width))


;; ------------------------------------------------------------
;; Frame functions
;;

(unless (fboundp 'frame-border-width)
  (defun frame-border-width (&optional frame)
    "Return border width of FRAME in pixels."
    (let ((pair (assq 'internal-border-width (frame-parameters frame))))
      (if pair
          (cdr pair)
        0))))


(unless (fboundp 'frame-scroll-bar-width)
  (defun frame-scroll-bar-width (&optional frame)
    "Return scroll bar width of FRAME in pixels."
    (let ((pair (assq 'scroll-bar-width (frame-parameters frame))))
      (if pair
          (cdr pair)
        0))))


(unless (fboundp 'frame-fringe-width)
  (defun frame-fringe-width (&optional frame)
    "Return fringe width of FRAME in pixels."
    (let ((left-pair (assq 'left-fringe (frame-parameters frame)))
          (right-pair (assq 'right-fringe (frame-parameters frame))))
      (+ (if left-pair (cdr left-pair) 0)
         (if right-pair (cdr right-pair) 0)))))


(unless (fboundp 'frame-text-width)
  (defun frame-text-width (&optional frame)
    "Return text area width of FRAME in pixels."
    (* (frame-width frame)
       (frame-char-width frame))))


(unless (fboundp 'frame-text-height)
  (defun frame-text-height (&optional frame)
    "Return text area height of FRAME in pixels."
    (* (frame-height frame)
       (frame-char-height frame))))


;; ------------------------------------------------------------
;; Special mode
;;
;; Used as base for the major mode used by the font-lock-studio
;; interface buffer.
;;
;; Originates from "simple.el" of Emacs.

(unless (boundp 'special-mode-map)
  (defvar special-mode-map
    (let ((map (make-sparse-keymap)))
      (suppress-keymap map)
      (define-key map "q" 'quit-window)
      (define-key map " " 'scroll-up-command)
      (define-key map [?\S-\ ] 'scroll-down-command)
      (define-key map "\C-?" 'scroll-down-command)
      (define-key map "?" 'describe-mode)
      (define-key map "h" 'describe-mode)
      (define-key map ">" 'end-of-buffer)
      (define-key map "<" 'beginning-of-buffer)
      (define-key map "g" 'revert-buffer)
      map))

  (put 'special-mode 'mode-class 'special)
  (define-derived-mode special-mode nil "Special"
    "Parent major mode from which special major modes should inherit."
    (setq buffer-read-only t)))


;; ------------------------------------------------------------
;; With silent modifications
;;
;; Used by the font-lock-studio.
;;
;; Originates from "subr.el" of Emacs.

(if (not (fboundp 'with-silent-modifications))
    (defmacro with-silent-modifications (&rest body)
      "Execute BODY, pretending it does not modify the buffer.
If BODY performs real modifications to the buffer's text, other
than cosmetic ones, undo data may become corrupted.

This macro will run BODY normally, but doesn't count its buffer
modifications as being buffer modifications.  This affects things
like `buffer-modified-p', checking whether the file is locked by
someone else, running buffer modification hooks, and other things
of that nature.

Typically used around modifications of text-properties which do
not really affect the buffer's content."
      (declare (debug t) (indent 0))
      (let ((modified (make-symbol "modified")))
        `(let* ((,modified (buffer-modified-p))
                (buffer-undo-list t)
                (inhibit-read-only t)
                (inhibit-modification-hooks t)
                deactivate-mark
                ;; Avoid setting and removing file locks and checking
                ;; buffer's uptodate-ness w.r.t the underlying file.
                buffer-file-name
                buffer-file-truename)
           (unwind-protect
               (progn
                 ,@body)
             (unless ,modified
               (restore-buffer-modified-p nil)))))))


;; ------------------------------------------------------------
;; Follow-mode (Emacs bug#16426)
;;
;; In Emacs 24.3, Follow-mode was refactorized. Unfortunately, this
;; also introduced a bug visible in, for example, *grep*.
;;
;; Originally, `follow-adjust-window' was called with `(point)' as the
;; second argument. Unfortunately, this is the value of the point of
;; the current buffer, not of the buffer of the selected window.
;;
;; Originates from "follow.el" of Emacs.

(when (and (fboundp 'follow-adjust-window)
           (equal emacs-major-version 24)
           (equal emacs-minor-version 3))
  (defun follow-post-command-hook ()
    "Ensure that the windows in Follow mode are adjacent after each command."
    (unless (input-pending-p)
      (let ((follow-inside-post-command-hook t)
            (win (selected-window))
            dest)
        ;; Work in the selected window, not in the current buffer.
        (with-current-buffer (window-buffer win)
          (setq dest (point))
          (unless (and (symbolp this-command)
                       (get this-command 'follow-mode-use-cache))
            (setq follow-windows-start-end-cache nil)))
        (follow-adjust-window win dest)))))


;; ------------------------------------------------------------
;; The end.
;;

(provide 'andersl-old-emacs-support)

;;; andersl-old-emacs-support.el ends here
