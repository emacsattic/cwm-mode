;;; cwm-mode.el --- minor mode for managing windows in columns

;; Copyright (C) 2009 Petteri Hintsanen <petterih@iki.fi>

;; Author: Petteri Hintsanen <petterih@iki.fi>
;; Created: 28 Jan 2009
;; Version: 0.1.2
;; Keywords: window, convenience

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; * GNU Emacs 22 or later is required.
;;
;; * For a general overview, see docstring for cwm-mode, or type `C-h
;;   f cwm-mode RET' after loading `cwm-mode.el'.  See also
;;   <http://iki.fi/petterih/cwm-mode.html>.
;;
;; * Most interactive functions have default key bindings with prefix
;;   `C-c .'.  Type `C-c . C-h' to display them.
;;
;; * Please report bugs and suggestions to <petterih@iki.fi>.

;;; Change Log:
;;
;; Version 0.1.2 (15 Aug 2009):
;;
;; * Changed `cwm-maximize-window' to use `enlarge-window' instead of
;;   `set-window-text-height'.
;;
;; * Removed unnecessary FRAME argument from `cwm-display-buffer'.
;;
;; * Fixed `cwm-balance-columns' to work with text-only terminals.
;;
;; Version 0.1.1 (1 Apr 2009):
;;
;; * Fixed `cwm-balance-columns' to handle fringes correctly.
;;
;; Version 0.1 (24 Feb 2009):
;;
;; * New function `cwm-mode-unload-function'.
;;
;; * Function `cwm-toggle-display-buffer' can toggle or explicitly
;;   set `display-buffer-function'.
;;
;; * User option variable names have been renamed to comply with
;;   standard Emacs conventions.
;;
;; * Other minor fixes to comply with standard Emacs conventions.
;;
;; Version 0.0 (28 Jan 2009):
;;
;; * The first version, posted to `gnu.emacs.sources'.

;;; Code:

(eval-when-compile
  (require 'windmove)
  (require 'cl))

;;;; Variables

(defvar cwm-auto-balance-flag t
  "*Non-nil means that windows and columns are automatically balanced.
Balancing is done whenever window configuration changes, i.e.,
windows or columns are split or deleted.")

(defvar cwm-previous-dbf display-buffer-function
  "The old value of `display-buffer-function'.
Function `cwm-toggle-display-buffer' restores
`display-buffer-function' to this value.")

(defvar cwm-recenter-maximized-flag t
  "*Non-nil means that a maximized window is recentered.
Specifically, when this variable is non-nil,
`cwm-maximize-window-maybe' recenters the window after maximizing
it.")

(defvar cwm-split-columns-flag t
  "*Non-nil means that `cwm-move-window' can split columns.
This affects `cwm-move-window-left' and
`cwm-move-window-right'.")

(defvar cwm-split-windows-flag t
  "*Non-nil means that `cwm-move-window' can split windows.
This affects `cwm-move-window-up' and `cwm-move-window-down'.")

;;;; Functions

(defun cwm-display-buffer (buffer not-this-window)
  "Make BUFFER appear in some non-minimized window.
This function is a wrapper for `display-buffer'.  It is intended
to be bound to `display-buffer-function' to prevent the possible
undesirable event with an one-line window displaying BUFFER.

The function calls `display-buffer' with its standard arguments
BUFFER and NOT-THIS-WINDOW.  The returned window is then passed
for `cwm-maximize-window-maybe', effectively maximizing the
window if it is minimized.

Return the window for displaying BUFFER."
  (let ((display-buffer-function nil) ;; prevent infinite recursion
	win)
    (setq win (display-buffer buffer not-this-window))
    (cwm-maximize-window-maybe win)
    win))

(defun cwm-find-other-window (dir &optional arg window)
  "Return the window object in direction DIR.
Call `windmove-find-other-window' with its standard arguments
DIR, ARG and WINDOW to find an adjacent window.  If the adjacent
window is minimized, return the maximized window in the same
column.  Otherwise, return the adjacent window."
  (let ((otherwin (windmove-find-other-window dir arg window)))
    (if (and otherwin (cwm-stacked-column-p otherwin))
        (find-if (lambda (win) (> (window-text-height win) 1))
                 (cwm-window-column otherwin))
      otherwin)))

(defun cwm-fit-window-maybe (&optional window)
  "Fit WINDOW to its buffer if it is minimized and not a minibuffer.
Default WINDOW is the selected window."
  (when (and (cwm-stacked-column-p window)
             (eq (window-text-height window) 1)
             (not (window-minibuffer-p window)))
    (fit-window-to-buffer (or window (selected-window))
                          (cwm-max-window-height window)
                          nil)))

(defun cwm-max-window-height (&optional window)
  "Return the maximum height for WINDOW in its column.
Default WINDOW is the selected window."
  ;; each minimized window requires two lines: one for text and one
  ;; for modeline
  (- (cwm-window-column-height window)
     (* (1- (length (cwm-window-column window))) 2)))

(defun cwm-maximize-window-maybe (&optional window)
  "Maximize WINDOW if it is minimized and not a minibuffer.
Default WINDOW is the selected window."
  (when (and (cwm-stacked-column-p window)
             (eq (window-text-height window) 1)
             (not (window-minibuffer-p window)))
    (cwm-maximize-window window)
    (when cwm-recenter-maximized-flag
      (recenter (/ (cwm-max-window-height) 2)))))

(defun cwm-move-selected-window (dir)
  "Move the selected window to direction DIR.
DIR is `up', `down', `left' or `right'.  See `cwm-move-window'
and `cwm-swap-window'."
  (cond ((memq dir '(left right))
         (select-window (or (cwm-move-window dir)
                            (selected-window))))
        ((memq dir '(up down))
         (select-window (or (cwm-swap-window dir)
                            (selected-window))))))

(defun cwm-move-window (dir &optional window)
  "Move WINDOW to an adjacent column in direction DIR.
Default WINDOW is the selected window.  DIR is `left' or `right'.
If there is an existing column in direction DIR, split the
adjacent window in that column to make room for WINDOW.  If the
column is stacked, enlarge WINDOW after moving it to the column.
If there is no column in direction DIR, make a new column
containing WINDOW unless `cwm-split-columns-flag' is nil.  See
`cwm-new-column'.

Note that no real window objects are relocated.  Instead, new
windows are created and their buffers modified accordingly to
simulate \"window movement\".  Especially all references to
WINDOW and (possibly) to other windows in its column become
invalid, because those windows are deleted.  See
`cwm-new-column'.

Return the new window object representing WINDOW, or nil WINDOW
could not be moved."
  (let ((buf (window-buffer window))
        (otherwin (cwm-find-other-window dir nil window)))
    (if otherwin
        ;; split the existing window
        (progn (if (cwm-stacked-column-p otherwin)
                   ;; minimized window can't be split, so
                   ;; enlarge it temporarily
                   (progn (cwm-maximize-window-maybe otherwin)
                          (split-window otherwin)
                          (cwm-maximize-window otherwin))
                 (split-window otherwin)
                 (when cwm-auto-balance-flag
                   (cwm-balance-column otherwin)))
               (unless (cwm-delete-window window)
                 ;; balance columns if window was the
                 ;; sole window in its column
                 (when cwm-auto-balance-flag
                   (cwm-balance-columns))))
      ;; create a new column
      (when cwm-split-columns-flag
        (let ((pos (cwm-window-column-position window))
              (win-col (cwm-new-column dir window)))
          (unless (eq (length (cadr win-col)) 1)
            (cwm-delete-window (nth pos (cadr win-col))))
          (setq otherwin (car win-col))
          (when cwm-auto-balance-flag
            (cwm-balance-columns)))))
    ;; otherwin is now the new window or nil
    (when otherwin (set-window-buffer otherwin buf))
    otherwin))

(defun cwm-new-column (dir &optional window)
  "Create a new column to direction DIR from WINDOW.
Default WINDOW is the selected window.  If WINDOW is the sole
window in its column, duplicate it.  Otherwise, call
`cwm-split-column-2' to split the column.

Return a list (WIN, COL), where WIN is the new window
representing the new column and COL is the (possibly restored)
column of WINDOW.  See `cwm-split-column-2'."
  (let ((col (cwm-window-column window)))
    (setq window (or window (selected-window)))
    (if (eq (length col) 1)
        ;; single window, duplicate it
        (if (eq dir 'right)
            (list (split-window window nil t) (list window))
          (list window (list (split-window window nil t))))
      ;; multiple windows, delete and restore
      (if (>= (/ (window-width window) 2) window-min-width)
          (cwm-split-column-2 col dir (cwm-stacked-column-p
                                       window))
        (error "Window width %s too small (after splitting)"
               (/ (window-width window) 2))))))

(defun cwm-split-column-2 (column dir stacked)
  "Delete all windows in COLUMN recursively until only one remains.
Split the remaining window horizontally and restore all deleted
windows.  New window is on the DIR side of the windows in COLUMN.
DIR is `left' or `right'.  If STACKED is t, the column is kept
stacked while restoring windows (otherwise there might not be
enough space to split windows).

Return a list (WIN, COL), where WIN is the new window and COL is
the restored column.

This function does not check that there is enough space to split
the column, so caller must ensure that.  See `cwm-new-column'."
  (let* ((win (car column))
         (curbuf (window-buffer win))
         win-column-pair)
    (if (> (length column) 1)
        ;; keep on deleting
        (progn (delete-window win)
               (setq win-column-pair
                     (cwm-split-column-2 (cdr column) dir stacked))
               ;; returned from the recursion, restore column
               (let* ((column (cadr win-column-pair))
                      (topwin (car column)))
                 (setcdr column
                         (cons (split-window topwin) (cdr column)))
                 (if stacked (cwm-maximize-window topwin)
                   (cwm-balance-column topwin))
                 (set-window-buffer topwin curbuf)))
      ;; base case, split horizontally
      (if (eq dir 'left)
          (progn (setq win-column-pair (list win '()))
                 (push (split-window-horizontally)
                       (cadr win-column-pair)))
        (setq win-column-pair (list (split-window-horizontally) '()))
        (push win (cadr win-column-pair))))
    win-column-pair))

(defun cwm-stacked-column-p (&optional window)
  "Return t if the column of WINDOW is stacked, nil otherwise."
  (and (> (length (cwm-window-column window)) 1)
       (or (eq (window-text-height window) 1)
           (eq (window-height window)
               (cwm-max-window-height window)))))

(defun cwm-swap-window (dir &optional window)
  "Swap WINDOW with its adjacent window in direction DIR.
Default WINDOW is the selected window.  DIR is `up' or `down'.
If there is no adjacent window, split the window unless
`cwm-split-windows-flag' is nil.

Return either the adjacent window or the new window, or nil if
WINDOW could not be swapped."
  (let ((buf (window-buffer window))
        (point (window-point window))
        (pos (window-start window))
        (otherwin (windmove-find-other-window dir nil window)))
    (when (window-minibuffer-p otherwin) (setq otherwin nil))
    (if otherwin
        ;; swap
        (progn (set-window-buffer window (window-buffer otherwin))
               (set-window-point window (window-point otherwin))
               (set-window-start window (window-start otherwin))
               (set-window-buffer otherwin buf)
               (set-window-point otherwin point)
               (set-window-start otherwin pos)
               (cwm-maximize-window-maybe otherwin))
      ;; split
      (when cwm-split-windows-flag
        (let ((stacked (cwm-stacked-column-p window)))
          (if (eq dir 'down)
              (setq otherwin (split-window window))
            (setq otherwin (or window (selected-window)))
            (split-window window))
          (if stacked
              (cwm-maximize-window otherwin)
            (when cwm-auto-balance-flag
              (cwm-balance-column window))))))
    otherwin))

(defun cwm-mode-unload-function ()
  "Unload cwm-mode.
Restore old `display-buffer-function', if necessary."
  (cwm-toggle-display-buffer 0)
  nil)

(defun cwm-windmove (dir &optional arg window)
  "Select the window in direction DIR.
Call `cwm-find-other-window' with arguments ARG and WINDOW and
select the returned window.  If no window is at the desired
direction, signal an error."
  (cond ((memq dir '(left right))
         (let ((otherwin (cwm-find-other-window dir arg window)))
           (if otherwin
               (progn (select-window otherwin)
                      (cwm-maximize-window-maybe otherwin))
             (error "No window %s from selected window" dir))))
        ((memq dir '(up down))
         (windmove-do-window-select dir)
         (cwm-maximize-window-maybe))))

(defun cwm-window-column (&optional window)
  "Return the column of WINDOW.
Default WINDOW is the selected window."
  (let (subtree)
    (setq window (or window (selected-window)))
    (setq subtree (bw-find-tree-sub window))
    (if subtree
        (if (car subtree)
            ;; vertical split, column is the nthcdr 2 of list
            (nthcdr 2 subtree)
          ;; horizontal split, column is the window itself
          (list window))
      ;; a single window
      (list window))))

(defun cwm-window-column-height (&optional window)
  "Return the height of the column of WINDOW.
Default WINDOW is the selected window.  Height is the total
height of all windows in the column, including mode lines."
  (let (subtree)
    (setq window (or window (selected-window)))
    (setq subtree (bw-find-tree-sub window))
    (if subtree
        ;; multiple windows
        (- (nth 3 (cadr subtree)) (nth 1 (cadr subtree)))
      ;; a single window
      (window-height window))))

(defun cwm-window-column-position (&optional window)
  "Return the index of WINDOW in its column.
The topmost window has index 0."
  (position (or window (selected-window))
            (cwm-window-column window)))

;;;; Interactive functions

(defun cwm-balance-column (&optional window)
  "Make windows the same heights in the column of WINDOW.
Default WINDOW is the selected window.  If there is not enough
space to balance windows (i.e. there are too many minimized
windows in the column), signal an error."
  (interactive)
  (let* ((col (cwm-window-column window))
         (height (cwm-window-column-height window))
         (lnperwin (/ height (length col)))
         (rmdr (% height (length col)))
         (i 1))
    (when (< lnperwin window-min-height)
      (error "Too many windows in the current column"))
    ;; maximize the topmost window to ensure that there is enough
    ;; space for adjust-window-trailing-edge for each window
    (cwm-maximize-window (car col))
    (dolist (win (butlast col))
      (adjust-window-trailing-edge win (+ (- lnperwin (window-height win))
                                          (if (<= i rmdr) 1 0)) 
                                   nil)
      (setq i (1+ i)))))

(defun cwm-balance-columns ()
  "Make columns the same widths in the current frame."
  (interactive)
  (let ((cols (car (window-tree))))
    ;; check if we really have columns to balance
    (when (and (listp cols) (eq (pop cols) nil))
      (save-selected-window
        (let* ((fringew (if window-system
			    (round (+ (fringe-columns 'left t)
				      (fringe-columns 'right t)))
			  ;; text terminals have vertical split lines
			  ;; instead of fringes
			  1))
               (framew (let ((edges (pop cols)))
                         (- (nth 2 edges) (nth 0 edges))))
               (colsperwin (/ (- framew (* (length cols) fringew))
                              (length cols)))
               (rmdr (% (- framew (* (length cols) fringew))
                        (length cols)))
               (i 1))
          ;; maximize the first column to ensure that there is enough
          ;; space for adjust-window-trailing-edge in each column
          (select-window (if (windowp (car cols)) (car cols)
                           (nth 2 (car cols))))
          (enlarge-window (- (- (frame-width) (* (1- (length cols))
						 (1+ window-min-width)))
			     (window-width))
			  t)
          ;; adjust sizes for columns
          (dolist (col (butlast cols))
            (let ((win (if (windowp col) col (nth 2 col))))
              (adjust-window-trailing-edge win (+ (- colsperwin 
                                                     (window-width win))
                                                  (if (<= i rmdr) 1 0))
                                           t)
              (setq i (1+ i)))))))))

(defun cwm-delete-window (&optional window)
  "Remove WINDOW from the display.
Default WINDOW is the selected window.  If the column of WINDOW
is stacked, it is kept stacked with the next window maximized.
The next window is the window below WINDOW, or window above
WINDOW if WINDOW is the last window in its column.  If the column
is not stacked, it is balanced with `cwm-balance-column'.

Return the next window in the corresponding column.  If WINDOW is
the sole window in its column, return nil."
  (interactive)
  (let* ((col (cwm-window-column window))
         (pos (cwm-window-column-position window))
         (nextpos (cond ((eq (length col) 1) nil)
                        ((eq pos (1- (length col))) (1- pos))
                        (t (1+ pos))))
         (nextwin (if nextpos (nth nextpos col)
                    nil))
         (stacked (cwm-stacked-column-p window)))
    (delete-window window)
    (cond (stacked (cwm-maximize-window nextwin))
          ((and nextwin cwm-auto-balance-flag)
           (cwm-balance-column nextwin))
          (cwm-auto-balance-flag (cwm-balance-columns)))
    (when (and (not window) nextwin) (select-window nextwin))
    nextwin))

(defun cwm-delete-other-windows (&optional window)
  "Make WINDOW fill its column.
Default WINDOW is the selected window."
  (interactive)
  (cwm-maximize-window window t))

(defun cwm-maximize-window (&optional window override)
  "Maximize WINDOW in its column by minimizing other windows.
Default WINDOW is the selected window.  If OVERRIDE is non-nil,
override other windows in the column, that is, cover the whole
column with WINDOW."
  (interactive)
  (save-selected-window 
    (when window (select-window window))
    (if override (enlarge-window (frame-height))
      (let ((window-min-height 1))
        (enlarge-window (- (cwm-max-window-height window) 
                           (window-height)))))))

(defun cwm-move-window-down ()
  "Move the selected window downwards.
If there is an another window below the selected window, swap
these two.  Otherwise, split the selected window and select the
lower one.  See `cwm-swap-window'."
  (interactive)
  (cwm-move-selected-window 'down))

(defun cwm-move-window-left ()
  "Move selected window to the left.
If there is an another window there, split it and move the
selected window into the new space.  Otherwise, make a new column
to the left and move the selected window into it.  See
`cwm-move-window'."
  (interactive)
  (cwm-move-selected-window 'left))

(defun cwm-move-window-right ()
  "Move selected window to the right.
If there is an another window there, split it and move the
selected window into the new space.  Otherwise, make a new column
to the right and move the selected window into it.  See
`cwm-move-window'."
  (interactive)
  (cwm-move-selected-window 'right))

(defun cwm-move-window-up ()
  "Move selected window upwards.
If there is an another window above the selected window, swap
these two.  Otherwise, split the selected window and select the
upper one.  See `cwm-swap-window'."
  (interactive)
  (cwm-move-selected-window 'up))

(defun cwm-split-column (arg)
  "Split the current column.
When called with prefix argument, create new column to the left
of the current column, otherwise to the right."
  (interactive "p")
  (let ((pos (cwm-window-column-position))
        (buf (window-buffer))
        (win-col (if (= arg 4)
                     (cwm-new-column 'left)
                   (cwm-new-column 'right))))
    (set-window-buffer (car win-col) buf)
    (when cwm-auto-balance-flag (cwm-balance-columns))
    (select-window (nth pos (cadr win-col)))))

(defun cwm-split-window ()
  "Split the selected window and balance its column."
  (interactive)
  (if (cwm-stacked-column-p)
      (progn (split-window)
             (cwm-maximize-window))
    (split-window)
    (when cwm-auto-balance-flag (cwm-balance-column))))

(defun cwm-toggle-display-buffer (arg)
  "Toggle whether to automatically maximize minimized windows.
Sometimes Emacs may display buffers in minimized windows, which
is usually undesirable.  To prevent such situations, this
function toggles, enables, or disables `cwm-display-buffer':

  * If ARG is positive, set `display-buffer-function' to
    'cwm-display-buffer and save the old value.

  * If ARG is zero or negative, restore the old value of
    `display-buffer-function'.

  * Without ARG, or if ARG is not a number, toggle
    `display-buffer-function' between 'cwm-display-buffer and its
    old value."
  (interactive "P")
  (unless (numberp arg)
    ;; toggle
    (if (eq display-buffer-function 'cwm-display-buffer) 
        (setq arg 0)
      (setq arg 1)))

  (if (> arg 0) 
      ;; use cwm-display-buffer
      (progn (unless (eq display-buffer-function 'cwm-display-buffer)
               (setq cwm-previous-dbf display-buffer-function)
               (setq display-buffer-function 'cwm-display-buffer))
             (message "cwm-display-buffer enabled globally")
             t)     
    ;; restore old display-buffer-function
    (when (eq display-buffer-function 'cwm-display-buffer)
      (setq display-buffer-function cwm-previous-dbf)
      (message "cwm-display-buffer disabled globally")
      nil)))

(defun cwm-toggle-stacked (&optional window)
  "Stack or unstack windows in the column of WINDOW.
Default WINDOW is the selected window.  If WINDOW is maximized,
balance all windows in the column.  Otherwise, maximize WINDOW."
  (interactive)
  (let ((stacked (cwm-stacked-column-p))
        (height (window-text-height)))
    (cond ((and stacked (> height 1))
           (cwm-balance-column (selected-window)))
          ((and stacked (eq height 1))
           (cwm-maximize-window))
          (t (cwm-maximize-window)))))

(defun cwm-windmove-down ()
  "Select the window below the selected one.
If no window is at the desired direction, signal an error.

This function is almost equivalent to `windmove-down'.  The only
difference is when the window below the current window is
minimized, becomes is maximized."
  (interactive)
  (cwm-windmove 'down))

(defun cwm-windmove-left ()
  "Select the window to the left from the selected one.
If no window is at the desired direction, signal an error.

This function is almost equivalent to `windmove-left'.  The only
difference is when the column to the left of the current column
is stacked.  In such case, the maximized window in the adjacent
column is selected."
  (interactive)
  (cwm-windmove 'left))

(defun cwm-windmove-right ()
  "Select the window to the right from the selected one.
If no window is at the desired direction, signal an error.

This function is almost equivalent to `windmove-right'.  The only
difference is when the column to the right of the current column
is stacked.  In such case, the maximized window in the adjacent
column is selected."
  (interactive)
  (cwm-windmove 'right))

(defun cwm-windmove-up ()
  "Select the window above the selected one.
If no window is at the desired direction, signal an error.

This function is almost equivalent to `windmove-up'.  The only
difference is when the window above the current window is
minimized, it becomes maximized."
  (interactive)
  (cwm-windmove 'up))

;;;; Minor mode definition

(define-minor-mode cwm-mode
  "Toggle Column Window Management mode.
With positive argument, turn on the mode.  With zero or negative
argument, turn off the mode.

In Column Window Management mode, windows can be managed in a
column-like layout.  By \"column\" we mean a set of windows
having the same width and lined vertically on top of each other.
For example, two windows created with a vertical split form a
column.  A single window forms a column by itself.

The selected window can be moved between columns with
`cwm-move-window-left' and `cwm-move-window-right'.  These
functions move the window to the corresponding direction.  If
there is no column in that direction, a new one is created,
unless variable `cwm-split-columns-flag' is nil.

The selected window can be moved up and down within the current
column with `cwm-move-window-up' and `cwm-move-window-down'.
These functions swap the window with the adjacent window in the
corresponding direction.  If there is no window in that
direction, a new one is created, unless variable
`cwm-split-windows-flag' is nil.

The selected window can be split with `cwm-split-window'.  The
selected window can be deleted with `cwm-delete-window'.  All
windows in the current column except the selected window can be
deleted with `cwm-delete-other-windows'.  These functions are
almost identical to `split-window', `delete-window', and
`delete-other-windows', but they try to maintain the layout
properly.

A column with several windows can be \"stacked\" with
`cwm-toggle-stacked'.  In a stacked column, one window is
maximized and the rest are minimized.  A maximized window has
always the largest possible height in its column, while minimized
windows have only one visible line.

The current column can be split with `cwm-split-column'.  This is
like `split-window-horizontally', but works with columns instead
of single windows.

Wrappers for `windmove-...' functions are provided (see Info
node `(emacs)Window Convenience').  Functions `cwm-windmove-left'
and `cwm-windmove-right' are identical to `windmove-left' and
`windmove-right', except that they choose a maximized window in
the corresponding direction if there is one.  Respectively,
`cwm-windmove-up' and `cwm-windmove-down' are like `windmove-up'
and `windmove-down', except that they maximize the selected
window if it is minimized.

Whenever window configurations are modified, windows in modified,
non-stacked columns are (vertically) balanced with
`cwm-balance-column'.  When columns are added or deleted, all
columns in the frame are (horizontally) balanced with
`cwm-balance-columns'.  These actions can be disabled by setting
the variable `cwm-auto-balance-flag' to nil.

Sometimes Emacs may display buffers in minimized windows, which
is usually undesirable.  To avoid such situations, functions
`cwm-maximize-window-maybe' and `cwm-fit-window-maybe' can be
added to suitable hooks or functions.  Function
`cwm-toggle-display-buffer' enables or disables
`cwm-display-buffer' that prevents many common cases (but may
cause surprising side effects).  By default, it is disabled."
  :global t
  :init-value nil
  :lighter nil
  :keymap
  '(("\C-c.0" . cwm-delete-window)
    ("\C-c.1" . cwm-delete-other-windows)
    ("\C-c.2" . cwm-split-window)
    ("\C-c.3" . cwm-split-column)
    ("\C-c.D" . cwm-windmove-down)
    ("\C-c.L" . cwm-windmove-left)
    ("\C-c.R" . cwm-windmove-right)
    ("\C-c.U" . cwm-windmove-up)
    ("\C-c.d" . cwm-move-window-down)
    ("\C-c.l" . cwm-move-window-left)
    ("\C-c.r" . cwm-move-window-right)
    ("\C-c.s" . cwm-toggle-stacked)
    ("\C-c.u" . cwm-move-window-up)))

(provide 'cwm-mode)

;; $Id: cwm-mode.el 218 2009-08-19 18:27:18Z phintsan $
;;; cwm-mode.el ends here
