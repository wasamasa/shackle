;;; shackle.el --- Enforce rules for popups

;; Copyright (C) 2014 Vasilij Schneidermann <v.schneidermann@gmail.com>

;; Author: Vasilij Schneidermann <v.schneidermann@gmail.com>
;; URL: https://github.com/wasamasa/shackle
;; Version: 0.0.8
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This global minor mode allows you to easily set up rules for
;; popups in Emacs.

;; See the README for more info:
;; https://github.com/wasamasa/shackle

;;; Code:

(require 'cl-lib)

(defgroup shackle nil
  "Enforce rules for popups"
  :group 'convenience
  :prefix "shackle-")

(defcustom shackle-lighter " ⛓"
  "Lighter for `shackle-mode'"
  :type 'string
  :group 'shackle)

(defcustom shackle-preserve-emacs-defaults t
  "If t, preserve Emacs' defaults for popping up buffers.
Otherwise always override them."
  :type 'boolean
  :group 'shackle)

(defcustom shackle-select-reused-windows nil
  "Make Emacs select reused windows by default.
If t, do this,otherwise allow selection on a case-by-case
basis."
  :type 'boolean
  :group 'shackle)

(defcustom shackle-rules nil
  "Association list of rules what to do with windows.
Each rule consists of a condition and a property list.  The
condition can be a symbol, a string or t.  If it's a symbol,
match the buffer's major mode.  If it's a string, match the name
of the buffer.  Use the following option in the property list to
use regular expression matching:

:regexp t

If the condition is t, turn the property list into the fallback
to use for every invocation of `display-buffer' with an unmatched
condition.

The property list accepts the following keys and values:

:select t

Make sure the window that popped up is selected afterwards.  Use
this option on its own or in combination with :reuse.
Alternatively set `shackle-select-reused-windows' to make this
the default for reused windows.

:same t

Don't pop up any window and reuse the currently active one.

:reuse t

Try reusing a window already displaying the target buffer.  Use
this in combination with `shackle-preserve-emacs-defaults' set to
nil to have the described behaviour for certain buffers only.
Alternatively use it as fallback rule to change only the
`switch-to-buffer' behaviour while keeping this Emacs default.

:frame t

Pop to a frame instead of window.

To make an exception to a fallback rule, use the condition you
want to exclude and either not use the key in question, use a
different value or use a placeholder as key."
  :type '(alist :key-type (choice symbol string)
                :value-type (plist :options
                                   ((:regexp boolean)
                                    (:select boolean)
                                    (:same boolean)
                                    (:frame boolean))))
  :group 'shackle)

(defun shackle--match (buffer-or-name condition plist)
  "Internal match function.
Used by `shackle-match', returns PLIST when BUFFER-OR-NAME
matches CONDITION."
  (let* ((buffer (get-buffer buffer-or-name))
         (buffer-major-mode (buffer-local-value 'major-mode buffer))
         (buffer-name (buffer-name buffer)))
    (when (or (eq t condition) ; fallback case
              (and (symbolp condition)
                   (eq condition buffer-major-mode))
              (and (stringp condition)
                   (or (string= condition buffer-name)
                       (and (plist-get plist :regexp)
                            (string-match condition buffer-name)))))
      plist)))

(defun shackle-match (buffer-or-name)
  "Checks whether BUFFER-OR-NAME matches any rule.
Uses `shackle--match' to decide with `shackle-rules' whether
there is a match, if yes it returns a property list which
`shackle-display-buffer-condition' and
`shackle-display-buffer-action' use."
  (cl-loop for (condition . plist) in shackle-rules
           when (shackle--match buffer-or-name condition plist)
           return plist))

(defun shackle-display-buffer-condition (buffer action)
  "Return key-value pairs when BUFFER match any shackle condition.
Uses `shackle-match'and `shackle-rules', BUFFER and ACTION take
the form `display-buffer-alist' specifies."
  (shackle-match buffer))

(defun shackle-display-buffer-action (buffer alist)
  "Execute an action for BUFFER according to `shackle-rules'.
This uses `shackle-display-buffer' internally, BUFFER and ALIST
take the form `display-buffer-alist' specifies."
  (shackle-display-buffer buffer alist (shackle-match buffer)))

(defun shackle--splittable-frame ()
  "Returns a splittable frame to work on.
This can be either the selected frame or the last frame that's
not displaying a lone minibuffer."
  (let ((selected-frame (selected-frame))
        (last-non-minibuffer-frame (last-nonminibuffer-frame)))
    (or (and (window--frame-usable-p selected-frame)
             (not (frame-parameter selected-frame 'unsplittable)))
        (and (window--frame-usable-p last-non-minibuffer-frame)
             (not (frame-parameter last-non-minibuffer-frame 'unsplittable))))))

(defun shackle--split-some-window (frame alist)
  "Returns a window if splitting any window was successful.
This tries using the largest window for splitting, if all windows
are the same size, the selected one is taken, in case this fails,
the least recently used window is used for splitting."
  (or (window--try-to-split-window (get-largest-window frame t) alist)
      (window--try-to-split-window (get-lru-window frame t) alist)))

(defun shackle-display-buffer (buffer alist &optional plist)
  "Display BUFFER and adhere to the conditions in PLIST.
See `display-buffer-pop-up-window' and
`display-buffer-pop-up-frame' for the basic functionality the
majority of code was lifted from.  Additionally to BUFFER AND
ALIST this function takes an optional PLIST argument which allows
it to do useful things such as selecting the popped up window
afterwards."
  (cond
   ;; start by checking for reusability, optionally select afterwards
   ((or (and (or shackle-preserve-emacs-defaults
                (plist-get plist :reuse))
            (let ((window (display-buffer-reuse-window buffer alist)))
              (prog1 window
                (when (and window
                           (or shackle-select-reused-windows
                               (and (plist-get plist :reuse)
                                    (plist-get plist :select))))
                  (select-window window)))))))
   ;; the next option is using the currently active window
   ((or (plist-get plist :same)
        ;; there is `display-buffer--same-window-action' which
        ;; things like `info' use to reuse the currently selected
        ;; window, it happens to be of the
        ;; (inhibit-same-window . nil) form
        (and shackle-preserve-emacs-defaults
             (and (assq 'inhibit-same-window alist)
                  (not (cdr (assq 'inhibit-same-window alist))))))
    (prog1 (window--display-buffer buffer (selected-window) 'window alist)
      ;; the following is done to ensure a reused window doesn't get
      ;; killed when invoking `quit-window', a command bound per
      ;; default to "q" in buffers derived from `special-mode'
      (set-window-parameter (selected-window) 'quit-restore nil)))
   ;; then handling frames in a very basic manner
   ((plist-get plist :frame)
    (let* ((params (cdr (assq 'pop-up-frame-parameters alist)))
           (pop-up-frame-alist (append params pop-up-frame-alist))
           (fun pop-up-frame-function))
      (when fun
        (let* ((frame (funcall fun))
               (window (frame-selected-window frame)))
          (prog1 (window--display-buffer
                  buffer window 'frame alist
                  display-buffer-mark-dedicated)
            (unless (cdr (assq 'inhibit-switch-frame alist))
              (window--maybe-raise-frame frame)))))))
   ;; and finally, if there's no :frame key-value pair, do the
   ;; surprisingly normal window popup and optionally select
   (t
    (let ((frame (shackle--splittable-frame)))
      (when frame
        (let ((window (shackle--split-some-window frame alist)))
          (prog1 (window--display-buffer
                  buffer window 'window alist
                  display-buffer-mark-dedicated)
            (when (plist-get plist :select)
              (select-window window t))
            (unless (cdr (assq 'inhibit-switch-frame alist))
              (window--maybe-raise-frame (window-frame window))))))))))

;;;###autoload
(define-minor-mode shackle-mode
  "Toggle `shackle-mode'.
This global minor mode allows you to easily set up rules for
popups in Emacs."
  :lighter shackle-lighter
  :global t
  (if shackle-mode
      (add-to-list 'display-buffer-alist
                   '(shackle-display-buffer-condition
                     shackle-display-buffer-action))
    (setq display-buffer-alist
          (remove '(shackle-display-buffer-condition
                    shackle-display-buffer-action)
                  display-buffer-alist))))

;;; shackle.el ends here
