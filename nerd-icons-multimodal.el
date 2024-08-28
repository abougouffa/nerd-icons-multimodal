;;; nerd-icons-multimodal.el --- Shows icons for each file in several Emacs modes, including dired-mode, archive-mode and tar-mode -*- lexical-binding: t -*-

;; Copyright (C) 2024 Abdelhak Bougouffa <abougouffa@fedoraproject.org>

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (nerd-icons "0.0.1"))
;; URL: https://github.com/abougouffa/nerd-icons-multimodal
;; Keywords: files, icons, archive

;; This program is free software; you can redistribute it and/or modify
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

;; This package is inspired by
;; - `nerd-icons-dired': https://github.com/rainstormstudio/nerd-icons-dired

(require 'arc-mode)
(require 'tar-mode)
(require 'nerd-icons)
(require 'dired)

(defface nerd-icons-multimodal-dir-face
  '((t nil))
  "Face for the directory icon."
  :group 'nerd-icons-faces)

(defcustom nerd-icons-multimodal-v-adjust 0.01
  "The default vertical adjustment of the icon in the archive buffer."
  :group 'nerd-icons
  :type 'number)

(defcustom nerd-icons-multimodal-refresh-commands
  '(tar-new-entry
    tar-rename-entry
    tar-expunge
    archive-resummarize
    archive-mode
    tar-mode
    dired-readin
    dired-revert
    dired-internal-do-deletions
    dired-insert-subdir
    dired-create-directory
    dired-do-redisplay
    dired-kill-subdir
    dired-do-kill-lines
    dired-narrow--internal ; dired-narrow
    dired-subtree-toggle) ; dired-subtree
  "Refresh the buffer icons when executing these commands."
  :group 'nerd-icons
  :type '(repeat function))

(defun nerd-icons-multimodal--add-overlay (pos string)
  "Add overlay to display STRING at POS."
  (let ((ov (make-overlay (1- pos) pos)))
    (overlay-put ov 'nerd-icons-multimodal-overlay t)
    (overlay-put ov 'after-string string)))

(defun nerd-icons-multimodal--overlays-in (beg end)
  "Get all nerd-icons-multimodal overlays between BEG to END."
  (cl-remove-if-not
   (lambda (ov)
     (overlay-get ov 'nerd-icons-multimodal-overlay))
   (overlays-in beg end)))

(defun nerd-icons-multimodal--overlays-at (pos)
  "Get nerd-icons-multimodal overlays at POS."
  (apply #'nerd-icons-multimodal--overlays-in `(,pos ,pos)))

(defun nerd-icons-multimodal--remove-all-overlays ()
  "Remove all `nerd-icons-multimodal' overlays."
  (save-restriction
    (widen)
    (mapc #'delete-overlay
          (nerd-icons-multimodal--overlays-in (point-min) (point-max)))))

(defun nerd-icons-multimodal--get-descriptor ()
  "Like `archive-get-descr' but simpler."
  (let ((no (archive-get-lineno)))
    (when (and (>= (point) archive-file-list-start)
               (< no (length archive-files)))
      (aref archive-files no))))

(defvar nerd-icons-multimodal-functions-alist
  `((next-line .
     ((archive-mode . archive-next-line)
      (tar-mode . tar-next-line)
      (dired-mode . dired-next-line)))
    (filename-at-pt .
     ((archive-mode . ,(lambda () (when-let* ((descr (nerd-icons-multimodal--get-descriptor)) (name (archive--file-desc-int-file-name descr))) name)))
      (tar-mode . ,(lambda () (when-let* ((descr (ignore-errors (tar-current-descriptor))) (name (tar-header-name descr))) name)))
      (dired-mode . ,(lambda () (dired-get-filename 'relative 'noerror)))))
    (move-to-filename .
     ((archive-mode . ,(lambda () (goto-char (line-beginning-position)) (forward-char archive-file-name-indent) (point)))
      (tar-mode . ,(lambda () (goto-char (line-beginning-position)) (goto-char (or (next-single-property-change (point) 'mouse-face) (point))) (point)))
      (dired-mode . ,(lambda () (dired-move-to-filename nil) (point)))))))

(defun nerd-icons-multimodal--call (sym &rest args)
  (if-let* ((func (alist-get major-mode (alist-get sym nerd-icons-multimodal-functions-alist))))
      (apply func args)
    (user-error "Mode `%s' doesn't have a `%s' function defined in `nerd-icons-multimodal-functions-alist'" major-mode sym)))

(defun nerd-icons-multimodal--supported-mode-p ()
  (and (alist-get major-mode (alist-get 'next-line nerd-icons-multimodal-functions-alist)) t))

(defun nerd-icons-multimodal--refresh ()
  "Display the icons of files in a archive buffer."
  (nerd-icons-multimodal--remove-all-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when-let ((name (nerd-icons-multimodal--call 'filename-at-pt)))
        (let ((icon (if (or (string-suffix-p "/" name) (and (eq major-mode 'dired-mode)
                                                            (file-directory-p name)))
                        (nerd-icons-icon-for-dir name
                                                 :weight 'regular
                                                 :face 'nerd-icons-multimodal-dir-face
                                                 :v-adjust nerd-icons-multimodal-v-adjust)
                      (nerd-icons-icon-for-file name :weight 'regular :v-adjust nerd-icons-multimodal-v-adjust)))
              (inhibit-read-only t))
          (if (member name '("." ".."))
              (nerd-icons-multimodal--add-overlay (nerd-icons-multimodal--call 'move-to-filename) "  \t")
            (nerd-icons-multimodal--add-overlay (nerd-icons-multimodal--call 'move-to-filename) (concat icon "\t")))))
      (nerd-icons-multimodal--call 'next-line 1))))

(defun nerd-icons-multimodal--refresh-advice (fn &rest args)
  "Advice function for FN with ARGS."
  (let ((result (apply fn args))) ;; Save the result of the advised function
    (when nerd-icons-multimodal-mode
      (nerd-icons-multimodal--refresh))
    result)) ;; Return the result

(defun nerd-icons-multimodal--setup ()
  "Setup `nerd-icons-multimodal'."
  (setq-local tab-width 1)
  (dolist (cmd nerd-icons-multimodal-refresh-commands)
    (advice-add cmd :around #'nerd-icons-multimodal--refresh-advice))

  ;; Refresh already open buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (nerd-icons-multimodal--supported-mode-p)
        (nerd-icons-multimodal--refresh)))))

(defun nerd-icons-multimodal--teardown ()
  "Functions used as advice when redisplaying buffer."
  (dolist (cmd nerd-icons-multimodal-refresh-commands)
    (advice-remove cmd #'nerd-icons-multimodal--refresh))

  ;; Refresh already open buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (nerd-icons-multimodal--supported-mode-p)
        (nerd-icons-multimodal--remove-all-overlays)))))

(defun nerd-icons-multimodal-refresh ()
  "Refresh the icons in the current buffer."
  (interactive)
  (if (and nerd-icons-multimodal-mode (nerd-icons-multimodal--supported-mode-p))
      (nerd-icons-multimodal--refresh)
    (user-error "Not in a supported major-mode")))

;;;###autoload
(define-minor-mode nerd-icons-multimodal-mode
  "Display nerd-icons icon for each files in a archive buffer."
  :lighter " nerd-icons-multimodal-mode"
  :global t
  (if nerd-icons-multimodal-mode (nerd-icons-multimodal--setup) (nerd-icons-multimodal--teardown)))


(provide 'nerd-icons-multimodal)
;;; nerd-icons-multimodal.el ends here
