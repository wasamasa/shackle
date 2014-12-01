;;; shackle.el --- Enforce rules for popups

;; Copyright (C) 2014 Vasilij Schneidermann <v.schneidermann@gmail.com>

;; Author: Vasilij Schneidermann <v.schneidermann@gmail.com>
;; URL: https://github.com/wasamasa/shackle
;; Version: 0.1
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
  "Lighter for `shackle-mode'."
  :type 'string
  :group 'shackle)

(defcustom shackle-preserve-emacs-defaults t
  "If t, preserve Emacs' defaults for popping up buffers.
Otherwise always override them."
  :type 'boolean
  :group 'shackle)

(defcustom shackle-select-reused-windows nil
  "Make Emacs select reused windows by default?
If t, do this, otherwise allow selection on a case-by-case
basis."
  :type 'boolean
  :group 'shackle)

(defcustom shackle-default-alignment 'below
  "Default alignment of aligned windows.
It may be one of the following values:

'above: Align above the currently selected window.

'below: Align below the currently selected window.

'left: Align on the left side of the currently selected window.

'right: Align on the right side of the currently selected
window."
  :type '(choice (const :tag "Above" above)
                 (const :tag "Below" below)
                 (const :tag "Left" left)
                 (const :tag "Right" right))
  :group 'shackle)

(defcustom shackle-default-ratio 0.5
  "Default ratio of aligned windows.
Must be a floating point number between 0 and 1.  A value of 0.5
splits the window in half, a value of 0.33 in a third, etc."
  :type 'float
  :group 'shackle)

(defcustom shackle-rules nil
  "Association list of rules what to do with windows.
Each rule consists of a condition and a property list.  The
condition can be a symbol, a string or t.  If it's a symbol,
match the buffer's major mode.  If it's a string, match the name
of the buffer.  Use the following option in the property list to
use regular expression matching:

:regexp and t

If the condition is t, turn the property list into the fallback
to use for every invocation of `display-buffer' with an unmatched
condition.

The property list accepts the following keys and values:

:select and t

Make sure the window that popped up is selected afterwards.  Use
this option on its own or in combination with :reuse.
Alternatively set `shackle-select-reused-windows' to make this
the default for reused windows.

:same and t

Don't pop up any window and reuse the currently active one.

:reuse and t

Try reusing a window already displaying the target buffer.  Use
this in combination with `shackle-preserve-emacs-defaults' set to
nil to have the described behaviour for certain buffers only.
Alternatively use it as fallback rule to change only the
`switch-to-buffer' behaviour while keeping this Emacs default.

:align and t or either of 'above, 'below, 'left and 'right

Align the popped up window at any of the specified sides or the
default size (`shackle-default-alignment') by deleting all other
windows, then restore the window configuration after the window
has been \"dealt\" with by either burying its buffer or deleting
the window.

:ratio and a floating point value between 0 and 1

Use this option to specify a different ratio than the default
value of 0.5 (`shackle-default-ratio`).

:defer and t

Use this option to defer cleaning up an aligned window.  This is
used to avoid errors with Emacs packages that clean up their
buffers themselves and rely on their window being kept open until
they finally delete it themselves, such as `helm'.

:frame and t

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
Used by `shackle-match', when BUFFER-OR-NAME matches CONDITION,
PLIST is returned."
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
  "Check whether BUFFER-OR-NAME is any rule match.
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
  "Return a splittable frame to work on.
This can be either the selected frame or the last frame that's
not displaying a lone minibuffer."
  (let ((selected-frame (selected-frame))
        (last-non-minibuffer-frame (last-nonminibuffer-frame)))
    (or (and (window--frame-usable-p selected-frame)
             (not (frame-parameter selected-frame 'unsplittable)))
        (and (window--frame-usable-p last-non-minibuffer-frame)
             (not (frame-parameter last-non-minibuffer-frame 'unsplittable))))))

(defun shackle--split-some-window (frame alist)
  "Return a window if splitting any window was successful.
This function tries using the largest window on FRAME for
splitting, if all windows are the same size, the selected one is
taken, in case this fails, the least recently used window is used
for splitting.  ALIST is passed to `window--try-to-split-window'
internally."
  (or (window--try-to-split-window (get-largest-window frame t) alist)
      (window--try-to-split-window (get-lru-window frame t) alist)))

(defun shackle--display-buffer-reuse (buffer alist plist)
  "Attempt reusing a window BUFFER is already displayed in.
ALIST is passed to `display-buffer-reuse-window' internally.
Optionally select the window afterwards if either
`shackle-select-reused-windows' is t or PLIST contains
the :select key with t as value."
  (let ((window (display-buffer-reuse-window buffer alist)))
    (prog1 window
      (when (and window
                 (or shackle-select-reused-windows
                     (and (plist-get plist :reuse)
                          (plist-get plist :select))))
        (select-window window)))))

(defun shackle--display-buffer-same (buffer alist)
  "Display BUFFER in the currently selected window.
ALIST is passed to `window--display-buffer' internally."
  (prog1 (window--display-buffer buffer (selected-window) 'window alist)
    ;; the following is done to ensure a reused window doesn't get
    ;; killed when invoking `quit-window', a command bound per
    ;; default to "q" in buffers derived from `special-mode'
    (set-window-parameter (selected-window) 'quit-restore nil)))

(defun shackle--display-buffer-frame (buffer alist)
  "Display BUFFER in a popped up frame.
ALIST is passed to `window--display-buffer' internally."
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

(defun shackle--display-buffer-popup-window (buffer alist plist)
  "Display BUFFER in a popped up window.
ALIST is passed to `window--display-buffer' internally.
Optionally select window afterwards if PLIST contains the :select
key with t as value."
  (let ((frame (shackle--splittable-frame)))
    (when frame
      (let ((window (shackle--split-some-window frame alist)))
        (prog1 (window--display-buffer
                buffer window 'window alist
                display-buffer-mark-dedicated)
          (when (plist-get plist :select)
            (select-window window t))
          (unless (cdr (assq 'inhibit-switch-frame alist))
            (window--maybe-raise-frame (window-frame window))))))))

(defvar shackle--in-progress nil
  "When t, cleanup must not be done yet.
This is because shackle is still doing something with the
windows, like selecting one after displaying it successfully.
Used by `shackle--restore-window-configuration' for aligned
windows.")

(defvar shackle--last-saved-window-configuration nil
  "Stores the last saved window configuration.
Only used for aligned windows.")

(defvar shackle--last-aligned-buffer nil
  "Stores the last aligned buffer.")

(defvar shackle--last-aligned-window nil
  "Stores the last aligned window.")

(defun shackle--restore-window-configuration ()
  "Hook function run to clean up an aligned buffer.
Since the hook in question isn't run with arguments, the
\"arguments\" are taken from `shackle--in-progress',
`shackle--last-aligned-buffer' and
`shackle--last-aligned-window'."
  (when (and (not shackle--in-progress)
             shackle--last-saved-window-configuration
             shackle--last-aligned-buffer
             (or (equal shackle--last-aligned-buffer (last-buffer))
                 (not (window-live-p shackle--last-aligned-window))))
    (unless (plist-get (shackle-match shackle--last-aligned-buffer) :defer)
      (remove-hook 'window-configuration-change-hook
                   'shackle--restore-window-configuration)
      (set-window-configuration (car shackle--last-saved-window-configuration))
      (goto-char (cadr shackle--last-saved-window-configuration))
      (setq shackle--last-saved-window-configuration nil)
      (setq shackle--last-aligned-buffer nil)
      (setq shackle--last-aligned-window nil))))

(defun shackle--display-buffer-aligned-window (buffer alist plist)
  "Display BUFFER in an aligned window.
ALIST is passed to `window--display-buffer' internally.
Optionally use a different alignment and/or ratio if PLIST
contains the :alignment key with an alignment different than the
default one in `shackle-default-alignment' and/or PLIST contains
the :ratio key with a floating point value."
  (let* ((frame (shackle--splittable-frame))
         ;; instead of just deleting every other window except the
         ;; currently selected one, the minibuffer one is dealt with
         ;; specifically by using the most recently used
         ;; non-minibuffer window in that case
         (selected-window (if (window-minibuffer-p)
                              (get-mru-window frame t)
                            (selected-window))))
    (when frame
      (let* ((alignment-argument (plist-get plist :align))
             (alignments '(above below left right))
             (alignment (if (memq alignment-argument alignments)
                            alignment-argument
                          shackle-default-alignment))
             (horizontal (when (memq alignment '(left right)) t))
             (old-size (window-size (frame-root-window) horizontal))
             (ratio (or (plist-get plist :ratio) shackle-default-ratio))
             (new-size (round (* (- 1 ratio) old-size))))
        (if (or (< new-size (if horizontal window-min-width window-min-height))
                (> new-size (- old-size (if horizontal window-min-width
                                          window-min-height))))
            (error "Invalid alignment ratio, aborting")
          (setq shackle--last-saved-window-configuration
                (list (current-window-configuration) (point)))
          (setq shackle--last-aligned-buffer buffer)
          (delete-other-windows selected-window)
          (let* ((window (split-window selected-window new-size alignment)))
            (setq shackle--last-aligned-window window)
            (prog1 (window--display-buffer buffer window 'window alist
                                           display-buffer-mark-dedicated)
              (when (plist-get plist :select)
                (select-window window t))
              (unless (cdr (assq 'inhibit-switch-frame alist))
                (window--maybe-raise-frame (window-frame window)))
              (add-hook 'window-configuration-change-hook
                        'shackle--restore-window-configuration))))))))

(defun shackle-display-buffer (buffer alist plist)
  "Display BUFFER according to ALIST and PLIST.
See `display-buffer-pop-up-window' and
`display-buffer-pop-up-frame' for the basic functionality the
majority of code was lifted from.  Additionally to BUFFER and
ALIST this function takes an optional PLIST argument which allows
it to do useful things such as selecting the popped up window
afterwards."
  (setq shackle--in-progress t)
  (cond
   ((or (and (or shackle-preserve-emacs-defaults
                (plist-get plist :reuse))
             (shackle--display-buffer-reuse buffer alist plist))))
   ((or (plist-get plist :same)
        ;; there is `display-buffer--same-window-action' which
        ;; things like `info' use to reuse the currently selected
        ;; (window, it happens to be of the
        ;; inhibit-same-window . nil) form
        (and shackle-preserve-emacs-defaults
             (and (assq 'inhibit-same-window alist)
                  (not (cdr (assq 'inhibit-same-window alist))))))
    (shackle--display-buffer-same buffer alist))
   ((plist-get plist :frame)
    (shackle--display-buffer-frame buffer alist))
   ((plist-get plist :align)
    (shackle--display-buffer-aligned-window buffer alist plist))
   (t
    (shackle--display-buffer-popup-window buffer alist plist)))
  (setq shackle--in-progress nil))

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

