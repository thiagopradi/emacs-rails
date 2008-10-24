;;; cc-guess.el --- guess indentation values by scanning existing code

;; Copyright (C) 1985,1987,1992-2003, 2004, 2005, 2006 Free Software
;; Foundation, Inc.

;; Author:     1994-1995 Barry A. Warsaw
;; Maintainer: Unmaintained
;; Created:    August 1994, split from cc-mode.el
;; Version:    See cc-mode.el
;; Keywords:   c languages oop

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This file contains routines that help guess the cc-mode style in a
;; particular region/buffer.  It is provided for example and
;; experimentation only.  It is not supported in anyway.  Note that
;; style guessing is lossy!
;;
;; The way this is intended to be run is for you to mark a region of
;; code to guess the style of, then run the command, `cc-guess-region'.

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-bytecomp" nil t)))

(cc-require 'cc-defs)
(cc-require 'cc-engine)


(defvar cc-guessed-style nil
  "Currently guessed style.")

(defvar cc-guess-delta-accumulator nil)
;; Accumulated sampled indent information.  Information is represented
;; in a list.  Each element in it has following structure:
;; 
;;  (syntactic-symbol ((indentation-delta1 . number-of-times1)
;; 		       (indentation-delta2 . number-of-times2)
;; 		       ...))
;; 
;; This structure is built by `cc-guess-accumulate-delta'.
;; 
;; Here we call the pair (indentation-delta1 . number-of-times1) a
;; counter.  `cc-guess-sort-delta-accumulator' sorts the order of
;; counters by number-of-times.

(defconst cc-guess-conversions
  '((c . c-lineup-C-comments)
    (inher-cont . c-lineup-multi-inher)
    (string . -1000)
    (comment-intro . c-lineup-comment)
    (arglist-cont-nonempty . c-lineup-arglist)
    (arglist-close . c-lineup-close-paren)
    (cpp-macro . -1000)))
  

(defun cc-guess (&optional accumulate)
  "Apply `cc-guess-region' on the whole current buffer.

If given a prefix argument (or if the optional argument ACCUMULATE is
non-nil) then the previous guess is extended, otherwise a new guess is
made from scratch."
  (interactive "P")
  (cc-guess-region (point-min) (point-max) accumulate))

(defun cc-guess-install ()
  "Set the indentation style from the last guessed style (`cc-guessed-style')."
  (interactive)
  (setq c-offsets-alist (cc-guess-merge-styles cc-guessed-style
					       c-offsets-alist)))

(defun cc-guess-region (start end &optional accumulate)
  "Set the indentation style by examining the indentation in a region of code.
Every line of code in the region is examined and the indentation
values of the various syntactic symbols in `c-offset-alist' are
guessed.  Frequencies of use are taken into account, so minor
inconsistencies in the indentation style shouldn't produce wrong
guesses.

The guessed style is put into `cc-guessed-style'.  It's also merged
into `c-offsets-alist'.  Guessed offsets takes precedence over
existing ones on `c-offsets-alist'.

If given a prefix argument (or if the optional argument ACCUMULATE is
non-nil) then the previous guess is extended, otherwise a new guess is
made from scratch.

Note that the larger the region to guess in, the slower the guessing."
  (interactive "r\nP")
  (let ((delta-accumulator (when accumulate cc-guess-delta-accumulator))
	(reporter (when (fboundp 'make-progress-reporter)
		    (make-progress-reporter "Sampling Indentation " start end))))
    ;;
    ;; Sampling stage
    ;;
    (save-excursion
      (goto-char start)
      (while (< (point) end)
	(c-save-buffer-state
	    ((syntax (c-guess-basic-syntax))
	     (relpos (car (cdr (car syntax))))
	     (symbol (car (car syntax))))
	  ;; TBD: for now I can't guess indentation when more than 1
	  ;; symbol is on the list, nor for symbols without relpos's
	  ;;
	  ;; I think it is too stricted for ((topmost-intro) (comment-intro)).
	  ;; -- Masatake
	  (unless (or ; (/= 1 (length syntax))
		   (not (numberp relpos))
		   (eq (line-beginning-position)
		       (line-end-position)))
	    (setq delta-accumulator (cc-guess-accumulate-delta
				     delta-accumulator
				     symbol
				     (- (progn (back-to-indentation)
					       (current-column) )
					(save-excursion
					  (goto-char relpos)
					  (current-column)))))))
	(when reporter (progress-reporter-update reporter (point)))
	(forward-line 1)))
    (when reporter (progress-reporter-done reporter))
    ;;
    ;; Guessing stage
    ;;
    (setq delta-accumulator (cc-guess-sort-delta-accumulator  
			     delta-accumulator)
	  cc-guess-delta-accumulator delta-accumulator)
    (let* ((typical-style (cc-guess-make-style delta-accumulator))
	   (merged-style (cc-guess-merge-styles 
			  (copy-list cc-guess-conversions)
			  typical-style)))
      (setq cc-guessed-style merged-style
	    c-offsets-alist (cc-guess-merge-styles
			     merged-style
			     c-offsets-alist)))))

(defun cc-guess-accumulate-delta (accumulator symbol delta)
  ;; Added SYMBOL and DELTA to ACCUMULATOR.  See
  ;; `cc-guess-delta-accumulator' about the structure of ACCUMULATOR.
  (let* ((entry    (assoc symbol accumulator))
	 (counters (cdr entry))
	 counter)
    (if entry
	(progn
	  (setq counter (assoc delta counters))
	  (if counter
	      (setcdr counter (1+ (cdr counter)))
	    (setq counters (cons (cons delta 1) counters))
	    (setcdr entry counters))
	  accumulator)
      (cons (cons symbol (cons (cons delta 1) nil)) accumulator))))

(defun cc-guess-sort-delta-accumulator (accumulator)
  ;; Sort the each element of ACCUMULATOR by the number-of-times.  See
  ;; `cc-guess-delta-accumulator' for more details.
  (mapcar
   (lambda (entry)
     (let ((symbol (car entry))
	   (counters (cdr entry)))
       (cons symbol (sort counters 
			  (lambda (a b)
			    (if (> (cdr a) (cdr b))
				t
			      (and 
			       (eq (cdr a) (cdr b))
			       (< (car a) (car b)))))))))
   accumulator))
	
(defun cc-guess-make-style (accumulator)
  ;; Throw away the rare cases in accumulator and make a style structure.
  (mapcar 
   (lambda (entry)
     (cons (car entry) 
	   (car (car (cdr entry)))))
   accumulator))

(defun cc-guess-merge-styles (strong weak)
  ;; Merge two styles into one.  When two styles has the same symbol
  ;; entry, give STRONG priority over WEAK.
  (mapc
   (lambda (weak-elt)
     (unless (assoc (car weak-elt) strong)
       (setq strong (cons weak-elt strong))))
   weak)
  strong)

(defun cc-guess-view-style ()
  "Show `cc-guessed-style'."
  (interactive)
  (with-output-to-temp-buffer "*Indentation Guessing Result*"
    (pp cc-guessed-style)))


(cc-provide 'cc-guess)
;;; cc-guess.el ends here
