;;; shell-pop.el --- Helps you pop up and pop out shell buffer easily.
;;; $Id: shell-pop.el,v 1.3 2010/05/28 14:58:12 whitypig Exp whitypig $

;; Copyright (C) 2009 Free Software Foundation, Inc.

;; Author:        Kazuo YAGI <kyagi@1liner.jp>
;; Maintainer:    Kazuo YAGI <kyagi@1liner.jp>
;; Created:       2009-05-31
;; Last-Updated:  $Date: 2010/05/28 14:58:12 $
;; Revision:      $Revision: 1.3 $
;; Keywords:      shell, terminal, tools
;; Compatibility: GNU Emacs 23.x

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary;
;;
;; This is a utility which helps you pop up and pop out shell buffer
;; window easily. Just do M-x shell-pop, and it is strongly recommmended
;; to assign one hot-key to this function.
;;
;; I hope this is useful for you, and ENJOY YOUR HAPPY HACKING!
;;

;;; Configuration;
;;
;; You can choose your favorite internal mode such as `shell', `terminal',
;; `ansi-term', and `eshell'. Also you can use any shell such as
;; `/bin/bash', `/bin/tcsh', `/bin/zsh' as you like.
;;
;; A configuration sample for your .emacs is as follows.
;;
;; (require 'shell-pop)
;; (shell-pop-set-internal-mode "ansi-term")
;; (shell-pop-set-internal-mode-shell "/bin/zsh")
;; (global-set-key [f8] 'shell-pop)
;;
;; Besides, you can set the window height, the number for the percentage
;; for selected window.
;;
;; (shell-pop-set-window-height 60)
;;

;;; Update Info;
;;
;; $Log: shell-pop.el,v $
;; Revision 1.3  2010/05/28 14:58:12  whitypig
;; Removed some garbage left.
;;
;; Revision 1.2  2010/05/28 14:57:33  whitypig
;; Updated to the latest.
;;
;; Revision 1.10  2010/02/21 14:19:26  kyagi
;; bug fix
;;
;; Revision 1.9  2010/02/21 14:03:43  kyagi
;; bug fix
;;
;; Revision 1.8  2010/02/21 12:55:28  kyagi
;; bug fix
;;
;; Revision 1.7  2010/02/21 11:29:56  kyagi
;; add a function shell-pop-set-window-position
;;
;; Revision 1.6  2010/02/21 11:25:01  kyagi
;; add a option shell-pop-window-position
;;

;;; Code:
(eval-when-compile
  (require 'cl))

(defvar shell-pop-last-buffer nil)
(defvar shell-pop-last-window nil)
(defvar shell-pop-window-height 30) ; percentage for shell-buffer window height
(defvar shell-pop-window-position "bottom")

(defvar shell-pop-internal-mode "shell")
(defvar shell-pop-internal-mode-buffer "*shell*")
(defvar shell-pop-internal-mode-func '(lambda () (shell)))
(defvar shell-pop-internal-mode-shell "/bin/bash")

(defvar shell-pop-internal-mode-list
  ;; mode, buffer, function
  (list
   '("shell"     "*shell*"     '(lambda () (shell)))
   '("terminal"  "*terminal*"  '(lambda () (term shell-pop-internal-mode-shell)))
   '("ansi-term" "*ansi-term*" '(lambda () (ansi-term shell-pop-internal-mode-shell)))
   '("eshell"    "*eshell*"    '(lambda () (eshell)))))

(defvar shell-pop-internal-shell-modes
  '(shell-mode term-mode ansi-term-mode eshell-mode)
  "A list of major modes in which shell runs.")

(defvar shell-pop-internal-mode-buffer-list nil
  "A queue that holds shell buffer.
For example, this holds \"*shell*\", \"*shell<2>*\", and so on.")

(defvar shell-pop-internal-mode-buffer-current-index 0
  "An index in shell-pop-internal-mode-buffer-list.")

(defadvice shell (after shell-func-after-advice () activate)
  "Update shell-pop-internal-mode-buffer-list
so that the newly invoked shell buffer is contained in this list."
  (setq shell-pop-internal-mode-buffer-list (shell-pop-collect-internal-shell-mode-buffer)))

(defun shell-pop-collect-internal-shell-mode-buffer ()
  (let ((bl (buffer-list))
        (ret nil))
    (dolist (b bl)
      (save-excursion
        (set-buffer b)
        (when (memq major-mode shell-pop-internal-shell-modes)
          (add-to-list 'ret b t))))
    (sort ret (lambda (b1 b2)
                (string< (buffer-name b1) (buffer-name b2))))))

(defun shell-pop-set-window-height (number)
  (interactive "nInput the number for the percentage of \
selected window height (10-100): ")
  (setq shell-pop-window-height number))

(defun shell-pop-set-window-position (position)
  (interactive "sInput the position for shell-pop (top|bottom): ")
  (setq shell-pop-window-position position))

(defun shell-pop-set-internal-mode (mode)
  (interactive "sInput your favorite mode (shell|terminal|ansi-term|eshell): ")
  (if (catch 'found
        (dolist (l shell-pop-internal-mode-list)
          (if (string-match mode (car l))
              (progn
                (setq shell-pop-internal-mode-buffer (nth 1 l))
                (setq shell-pop-internal-mode-func (nth 2 l))
                (throw 'found t)))))
      t
    nil))

(defun shell-pop-set-internal-mode-shell (shell)
  (interactive (list (read-from-minibuffer "Input your favorite shell:"
                                           shell-pop-internal-mode-shell)))
  (setq shell-pop-internal-mode-shell shell))

(defun shell-pop-get-shell-buffer-window ()
  "Return a window displaying shell buffer held by `shell-pop-internal-mode-buffer-list',
or nil."
  (let ((w nil))
    (catch 'found
      (dolist (b shell-pop-internal-mode-buffer-list)
        (when (setq w (get-buffer-window b))
          (throw 'found w))
        nil))))

(defun shell-pop ()
  (interactive)
  (if (member (get-buffer (buffer-name)) shell-pop-internal-mode-buffer-list)
      (shell-pop-out)
    (shell-pop-up)))

(defun shell-pop-get-shell-buffer-index-by-window (window)
  "Return an index of WINDOW-displaying shell buffer in shell-pop-internal-mode-buffer-list."
  (let ((b (window-buffer window))
        (n 0))
    (catch 'index
      (dolist (e shell-pop-internal-mode-buffer-list)
        (when (equal e b)
          (throw 'index n))
        (setq n (1+ n)))
      nil)))

(defun shell-pop-up ()
  "Pop up shell buffer."
  (let ((w (shell-pop-get-shell-buffer-window)))
    (setq shell-pop-internal-mode-buffer-list (shell-pop-collect-internal-shell-mode-buffer))
    (cond
     (w
      ;; if shell buffer window is on this frame and is not focused,
      ;; then move to that window.
      (setq shell-pop-internal-mode-buffer-current-index
            (shell-pop-get-shell-buffer-index-by-window w))
      (select-window w))
     (t
      ;; there is no shell buffer window in the current frame
      ;; save shell-pop-last-buffer and shell-pop-last-window to return
      (setq shell-pop-last-buffer (buffer-name))
      (setq shell-pop-last-window (selected-window))
      (when (not (eq shell-pop-window-height 100))
        (split-window (selected-window)
                      (if (string= shell-pop-window-position "bottom")
                          (round (* (window-height)
                                    (/ (- 100 shell-pop-window-height) 100.0)))
                        (round (* (window-height) (/ shell-pop-window-height 100.0)))))
        (when (string= shell-pop-window-position "bottom")
          (other-window 1)))
      ;; a window for shell buffer has been created
      ;; Also I'm not in shell buffer
      (if (null shell-pop-internal-mode-buffer-list)
          ;; if there is no shell buffer available, create one
          (funcall (eval shell-pop-internal-mode-func))
        (switch-to-buffer (nth shell-pop-internal-mode-buffer-current-index
                               shell-pop-internal-mode-buffer-list)))))))

(defun shell-pop-out ()
  "Hide shell buffer.
If the previously acitive buffer no longer exists, we get emacs
to choose an appropriate window."
  (cond ((= (1+ shell-pop-internal-mode-buffer-current-index)
            (length shell-pop-internal-mode-buffer-list))
         ;; move to the non-shell buffer
         (setq shell-pop-internal-mode-buffer-current-index 0)
         (when (not (eq shell-pop-window-height 100))
           (delete-window)
           (when (string= shell-pop-window-position "bottom")
             (select-window shell-pop-last-window)))
         (shell-pop-goto-previous-buffer shell-pop-last-buffer))
        (t
         (setq shell-pop-internal-mode-buffer-current-index
               (mod (1+ shell-pop-internal-mode-buffer-current-index)
                    (length shell-pop-internal-mode-buffer-list)))
         (shell-pop-goto-previous-buffer (nth shell-pop-internal-mode-buffer-current-index
                                               shell-pop-internal-mode-buffer-list)))))

(defun shell-pop-goto-previous-buffer (prev-buffer)
  "Go to the PREV-BUFFER if it exists.
If PREV-BUFFER no longer exists, we get Emacs to choose appropriate one."
  (let ((b (get-buffer prev-buffer)))
    (if b
        (switch-to-buffer
         (if (member
              (get-buffer shell-pop-last-buffer)
              shell-pop-internal-mode-buffer-list)
             (setq shell-pop-last-buffer (buffer-name
                                          (find-if
                                           (lambda (b)
                                             (and (not (member b shell-pop-internal-mode-buffer-list))
                                                  (not (string-match "^ " (buffer-name b)))))
                                           (buffer-list))))
           prev-buffer))
      (switch-to-buffer (other-buffer)))))

(provide 'shell-pop)

;;; shell-pop.el ends here.
