;;; targets.el --- Extension of evil text objects. -*- lexical-binding: t -*-

;; Author: Fox Kiester <noct@openmailbox.org>
;; URL: https://github.com/noctuid/targets.el
;; Created: November 29, 2016
;; Keywords: evil text-object convenience
;; Package-Requires: ((emacs "24.4")  (cl-lib "0.5") (evil "1.1.0") (avy "0.4.0"))
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
;; This package is like a combination of the targets, TextObjectify, anyblock,
;; and expand-region vim plugins.

;; For more information see the README in the github repo.

;; TODO gensyms/once-only and debug declarations
;;; Code:
(require 'cl-lib)
(require 'evil)

;;; * Settings
(defgroup targets nil
  "Provides extensions to evil's text objects."
  :group 'evil
  :prefix 'targets-)

(defcustom targets-seek-functions
  (list #'targets-seek-forward #'targets-seek-backward)
  "Functions to try in order when text object selection fails.
A non-nil return value of a successfully run function signals that the point
prior to the seek should be added to the jump list."
  :group 'targets
  :type '(repeat :tag "Seek functions" function))

(defvar targets-inside-text-objects-map (make-sparse-keymap)
  "Keymap for inside text objects.")

(defvar targets-around-text-objects-map (make-sparse-keymap)
  "Keymap for around text objects.")

(defvar targets-pair-text-objects
  '((paren "(" ")" pair)
    (bracket "[" "]" pair)
    (curly "{" "}" pair)
    (angle "<" ">" pair)))

(defvar targets-quote-text-objects
  '((single-quote "'" nil quote)
    (double-quote "\"" nil quote)
    (smart-single-quote "‘" "’" quote :bind nil)
    (smart-double-quote "“" "”" quote :bind nil)
    (emacs-smart-single-quote "`" "'" quote :bind nil)
    (back-quote "`" nil quote)))

(defvar targets-separator-text-objects
  '((comma "," nil separator)
    (period "." nil separator)
    (semi-colon ";" nil separator)
    (colon ":" nil separator)
    (plus "+" nil separator)
    (hyphen "-" nil separator)
    (equal "=" nil separator)
    (tilde "~" nil separator)
    (underscore "_" nil separator)
    (asterisk "*" nil separator)
    (hash "#" nil separator)
    (slash "/" nil separator)
    (backslash "\\" nil separator)
    (ampersand "&" nil separator)
    (dollar "$" nil separator)))

(defvar targets-object-text-objects
  '((word 'evil-word nil object :keys "w")
    (WORD 'evil-WORD nil object :keys "W")
    (symbol 'evil-symbol nil object :keys "o")
    (sentence 'evil-sentence nil object :keys "s")
    (paragraph 'evil-paragraph nil object :keys "p" :linewise t)))

(defvar targets-text-objects
  (append targets-pair-text-objects
          targets-quote-text-objects
          targets-separator-text-objects
          targets-object-text-objects)
  "A list of text objects to be definite with `targets-setup'.
Each item should be a valid arglist for `targets-define-to'.")

(defcustom targets-default-text-object nil
  "The default text object to use for `targets-last-text-object'.
This is used whenever there is no last text object stored for the current
  state (operator or visual). Note that the last text object for visual state is
  cleared after exiting visual state."
  :group 'targets
  :type 'function)

(defvar targets--last-visual-text-object
  "Holds the last text object used in visual state.")

(defvar targets--last-operator-text-object
  "Holds the last text object used in operator state.")

(defcustom targets-avy-style nil
  "Method for displaying avy overlays.
Use `avy-style' if nil."
  :group 'targets
  :type '(choice
          (const :tag "Pre" pre)
          (const :tag "At" at)
          (const :tag "At Full" at-full)
          (const :tag "Post" post)
          (const :tag "De Bruijn" de-bruijn)))

(defcustom targets-avy-keys nil
  "Keys used for selecting urls.
Use `avy-keys' if nil."
  :group 'targets
  :type '(repeat :tag "Keys" (choice (character :tag "char"))))

(defcustom targets-avy-background 'use-avy
  "When non-nil, a gray background will be added during the selection.
Use `avy-background' if 'use-avy."
  :type 'boolean)

(defcustom targets-avy-all-windows nil
  "Determine the list of windows to consider in search of text objects.
Use `avy-all-windows' if 'use-avy."
  :type
  '(choice
    (const :tag "All Frames" all-frames)
    (const :tag "This Frame" t)
    (const :tag "This Window" nil)))

(defcustom targets-avy-all-windows-alt nil
  "The alternative `targets-avy-all-windows' for use with
\\[universal-argument]. Use `avy-all-windows-alt' if 'use-avy."
  :type '(choice
          (const :tag "Current window" nil)
          (const :tag "All windows on the current frame" t)
          (const :tag "All windows on all frames" all-frames)))

;;; * User-customizable Functions
(defun targets-push-jump-p (old-pos new-pos)
  "Whether or not to push to the evil jump list after a successful seek.
This default function will push to the jump list when OLD-POS and NEW-POS are
not on the same line. Override or redefine this function to change this behavior
or change `targets-seek-functions' completely instead."
  (not (= (line-number-at-pos old-pos) (line-number-at-pos new-pos))))

(defun targets-bound (&optional backwards)
  "Return the bound to be used when seeking forwards or backwards.
BACKWARDS specifies that the bound should be for seeking backwards. This
function is used both when there is no text object at the point and for next and
last text objects. This default function bounds seeking to the beginning and end
of the window. Override or redefine this function to change this behavior or
change `targets-seek-functions' completely instead."
  (if backwards
      (window-start)
    (window-end)))

;;; * Seeking Functions
(defun targets-seek-forward (open _ type &optional count bound)
  "Seek forward to the text object specified by OPEN and TYPE COUNT times.
If BOUND is non-nil, do not seek beyond BOUND. If successful, this function will
move the point to beginning of the match and return its position."
  (setq count (or count 1))
  (setq bound (or bound (targets-bound) (point-max)))
  (let ((orig-pos (point))
        case-fold-search)
    (cl-case type
      ((pair separator)
       (forward-char)
       (if (and (<= (point) bound)
                (re-search-forward (regexp-quote open) bound t count))
           (goto-char (match-beginning 0))
         (backward-char)))
      (quote
       (let ((evil-forward-quote-char (string-to-char open)))
         (ignore-errors (end-of-thing 'evil-quote))
         ;; count is broken for evil-forward-quote
         (let ((pos (point)))
           (dotimes (_ count)
             (forward-thing 'evil-quote))
           (if (or (= (point) pos)
                   (> (point) bound))
               (goto-char orig-pos)
             (beginning-of-thing 'evil-quote)))))
      (object
       (ignore-errors (end-of-thing open))
       (let ((pos (point)))
         (forward-thing open count)
         (when (or (= (point) pos)
                   (> (point) bound)
                   ;; may not actually be at thing
                   ;; TODO guarunteed to return non-nil on success?
                   ;; seems to be the case
                   (not (ignore-errors (beginning-of-thing open))))
           (goto-char orig-pos)))))
    (unless (= (point) orig-pos)
      (point))))

(defun targets-seek-backward (open close type &optional count bound)
  "Seek backward to the text object specified by OPEN, CLOSE, and TYPE COUNT
times. If BOUND is non-nil, do not seek beyond BOUND. If successful, return the
matched position (otherwise nil)."
  (setq count (or count 1))
  (setq bound (or bound (targets-bound t) (point-min)))
  (let ((orig-pos (point))
        case-fold-search)
    (cl-case type
      (pair
       (let* ((last-pos orig-pos)
              (parenp (and (= (length open) 1)
                           (= (length close) 1)))
              (open-char (when parenp
                           (string-to-char open)))
              (close-char (when parenp
                            (string-to-char close)))
              (quote-regexp (concat "\\("
                                    (regexp-quote open)
                                    "\\|"
                                    (regexp-quote close)
                                    "\\)")))
         (cl-dotimes (_ count)
           (let ((prev-paren (save-excursion
                               (re-search-backward quote-regexp bound t)
                               (point)))
                 (open-paren (save-excursion
                               (if parenp
                                   (evil-up-paren open-char close-char -1)
                                 (evil-up-block open close -1))
                               ;; evil-up-block moves to bob if it fails
                               (unless (= (point) 1)
                                 (point)))))
             (when (= prev-paren last-pos)
               (cl-return-from nil))
             (goto-char prev-paren)
             (when (and open-paren (= prev-paren open-paren))
               (re-search-backward quote-regexp bound t)
               (when (= (point) prev-paren)
                 (cl-return-from nil)))
             (setq last-pos (point))))))
      (separator
       (when (re-search-backward (regexp-quote open) bound t count)
         ;; required to not select current separator
         (backward-char)))
      (quote
       (let* ((evil-forward-quote-char (string-to-char open))
              (bnd (bounds-of-thing-at-point 'evil-quote)))
         (when (and bnd
                    ;; the char after the string is given in the bnd
                    (not (= (point) (cdr bnd))))
           (goto-char (car bnd)))
         (when (or (= -1 (forward-thing 'evil-quote (- count)))
                   (< (point) bound))
           (goto-char orig-pos))))
      (object
       (ignore-errors (beginning-of-thing open))
       (let ((pos (point)))
         (forward-thing open (- count))
         (when (or (= (point) pos)
                   (< (point) bound)
                   (not (ignore-errors (beginning-of-thing open))))
           (goto-char orig-pos)))))
    (unless (= (point) orig-pos)
      (point))))

(defvar targets--reset-position nil)
(defvar targets--reset-window nil)

(defun targets--reset-position ()
  "Called after next and last text objects to restore the cursor position.
The point is not restored if there is a selection."
  (unless (region-active-p)
    (when targets--reset-position
      (jump-to-register 'targets--reset-position)
      (setq targets--reset-position nil))
    (when targets--reset-window
      (let* ((window targets--reset-window)
             (frame (window-frame window)))
        (unless (equal frame (selected-frame))
          (select-frame-set-input-focus frame))
        (select-window window))
      (setq targets--reset-window nil))))

;;; * Avy-related Functions
(with-eval-after-load 'avy
  (defun targets--collect-text-objects (open type select-func)
    "Collect all locations of visible text objects based on OPEN and TYPE.
SELECT-FUNC is used to determine if there is a text object at the beginning of
the visible regions of the window as `targets-seek-foraward' will seek past the
current text object."
    (let (all-to-positions)
      (avy-dowindows current-prefix-arg
        (save-excursion
          (dolist (bounds (avy--find-visible-regions (window-start)
                                                     (window-end)))
            (goto-char (car bounds))
            (let ((current-window (get-buffer-window))
                  to-pos
                  to-positions)
              ;; add a text object at the beginning of the window
              ;; as the eol of an invisible line can be visible in org buffers,
              ;; don't do this if the point is at the eol
              (when (and (not (looking-at (rx eol)))
                         (let ((range (funcall select-func)))
                           (and range (>= (car range) (car bounds)))))
                (push (cons (point) current-window) to-positions))
              (while (setq to-pos (targets-seek-forward open nil type
                                                        1 (cdr bounds)))
                (push (cons to-pos current-window) to-positions)
                (goto-char to-pos))
              (setq all-to-positions (append all-to-positions
                                             (nreverse to-positions)))))))
      all-to-positions))

  (defun targets--save-point-and-jump (pos)
    "Put the point in the targets--reset-position register nad jump to POS."
    (point-to-register 'targets--reset-position)
    (goto-char pos))

  (defun targets--avy-seek (command open _ type select-func)
    "Seek to a text object specified by OPEN and TYPE using avy for selection.
COMMAND will be used as the name given to `avy-with', so that `avy-styles-alist'
and `avy-keys-alist' can be customize for COMMAND. SELECT-FUNC is used to
determine if there is a text object at the beginning of the visible regions of
the window. A text object at the beginning of the window will only included if
it starts at or after the beginning of the window."
    (let ((avy-style (or targets-avy-style avy-style))
          (avy-keys (or targets-avy-keys avy-keys))
          (avy-background (if (eq targets-avy-background 'use-avy)
                              avy-background
                            targets-avy-background))
          (avy-all-windows (if (eq targets-avy-all-windows 'use-avy)
                               avy-all-windows
                             targets-avy-all-windows))
          (avy-all-windows-alt (if (eq targets-avy-all-windows-alt 'use-avy)
                                   avy-all-windows-alt
                                 targets-avy-all-windows-alt))
          ;; doesn't seem to be necessary
          ;; (scroll-margin 0)
          )
      (avy-with command
        (let ((avy-action #'targets--save-point-and-jump)
              (tos (targets--collect-text-objects open type select-func)))
          (if (not tos)
              (message "No text objects found.")
            (avy--process tos (avy--style-fn avy-style))))))))

;;; * Text Object Definers
;; ** Helpers
(defun targets--select-to (to-type select-type linewise open close beg end type
                                   count)
  "Return a range corresponding to the matched text object.
The text object is specified by TO-TYPE (pair, quote, separator, or object),
SELECT-TYPE (inner, a, inside, or around), LINEWISE, OPEN, and CLOSE. BEG, END,
and TYPE specify visual selection information. COUNT is the number of text
objects."
  (let* ((open-char (cl-case to-type
                      (pair (when (and (= (length open) 1)
                                       (= (length close) 1))
                              (string-to-char open)))
                      (quote (string-to-char open))))
         (close-char (when (and open-char (eq to-type 'pair))
                       (string-to-char close)))
         (inclusive (when (memq select-type '(a around))
                      t))
         (range
          (save-excursion
            (cl-case to-type
              (pair
               (evil-select-paren (or open-char open) (or close-char close)
                                  beg end type count inclusive))
              (separator
               (if inclusive
                   (let ((range
                          (evil-select-paren open open beg end type count t)))
                     ;; reduce range
                     (when range
                       (setf (cadr range) (1- (cadr range))))
                     range)
                 (evil-select-paren open open beg end type count)))
              (quote
               (if inclusive
                   ;; because don't want whitespace
                   (let ((range
                          (evil-select-quote open-char beg end type count)))
                     (when range
                       ;; expand range
                       (setf (car range) (1- (car range))
                             (cadr range) (1+ (cadr range))))
                     range)
                 (evil-select-quote open-char beg end type count)))
              (object
               (if inclusive
                   (evil-select-an-object open beg end type count linewise)
                 (evil-select-inner-object open beg end type count
                                           linewise)))))))
    (when range
      (save-excursion
        (when (and (not inclusive)
                   (not (eq to-type 'object))
                   (looking-at (regexp-quote open)))
          ;; so point will still be inside range for inner text objects
          (goto-char (match-end 0)))
        ;; ignore range when evil seeks
        (unless (or (> (car range) (point))
                    (< (cadr range) (point)))
          (cl-case select-type
            ((inner a linewise)
             range)
            (inside
             (goto-char (car range))
             (skip-chars-forward " \t")
             (setf (car range) (point))
             (goto-char (cadr range))
             (skip-chars-backward " \t")
             (setf (cadr range) (point))
             range)
            (around
             (when (eq to-type 'separator)
               (setf (cadr range) (1+ (cadr range))))
             (goto-char (cadr range))
             (skip-chars-forward " \t")
             (cond ((= (point) (cadr range))
                    (goto-char (car range))
                    (skip-chars-backward " \t")
                    (setf (car range) (point)))
                   (t
                    (setf (cadr range) (point))))
             range)))))))

(defun targets--select-to-with-seeking
    (to-type select-type linewise open close beg end type count)
  "Return a range corresponding to the matched text object.
If unsuccessful, seek using the functions in `targets-seek-functions' to attempt
to find a matching text object. Push the initial position when seeking if
`targets-push-jump' run with the inital and final positions returns non-nil. See
`targets--select-to' for more details."
  (let ((seek-functions targets-seek-functions)
        (orig-pos (point))
        range
        push-jump-p)
    (while (and (not
                 (setq range
                       (ignore-errors
                         (save-excursion
                           (targets--select-to to-type select-type linewise open
                                               close beg end type count)))))
                seek-functions)
      (goto-char orig-pos)
      (funcall (pop seek-functions) open close to-type)
      (setq push-jump-p (targets-push-jump-p orig-pos (point)))
      ;; discard visual info if seeking
      (setq beg nil end nil))
    (when (and range push-jump-p)
      (evil-set-jump orig-pos))
    range))

(defun targets--define-keys (keymap function prefix keys)
  "In KEYMAP, bind multiple keys to FUNCTION.
The keys are created by using PREFIX to prefix each key in KEYS."
  (when prefix
    (while keys
      (define-key keymap (concat (if (stringp prefix)
                                     prefix
                                   nil)
                                 (pop keys))
        function))))

;; ** targets-define-to
;;;###autoload
(cl-defmacro targets-define-to (name open close to-type &key
                                     linewise
                                     mode
                                     bind
                                     (next-key "n")
                                     (last-key "l")
                                     (remote-key "r")
                                     keys
                                     more-keys)
  "The main text object definition facility provided by targets.
NAME is used to name the resulting text objects (e.g. targets-inner-NAME). OPEN,
CLOSE, and TO-TYPE hold the required information to create the text objects.
TO-TYPE is one of pair, quote, separator, or object. OPEN and CLOSE should be
strings. CLOSE is only used for pair text objects. LINEWISE is only used for
object type text objects.

If BIND is non-nil, additionally bind all of the created text objects. NEXT-KEY,
LAST-KEY, and REMOTE-KEY can be changed to alter the intermediate keys used for
next and last text objects. If they are nil, those text objects will not be
bound at all. IF KEYS is not specified, it will default to OPEN and CLOSE. If
TO-TYPE is object or OPEN or CLOSE are regexps/multiple characters (for TO-TYPE
pair or separator), KEYS must be specified if BIND is non-nil. MORE-KEYS can be
used to specify keys to be used in addtion to OPEN/CLOSE."
  (let* ((name (if (symbolp name)
                   (symbol-name name)
                 name))
         (inner-name (intern (concat "targets-inner-" mode name)))
         (a-name (intern (concat "targets-a-" mode name)))
         (inside-name (intern (concat "targets-inside-" mode name)))
         (around-name (intern (concat "targets-around-" mode name)))
         (next-inner-name (intern
                           (concat "targets-inner-next-" mode name)))
         (next-a-name (intern (concat "targets-a-next-" mode name)))
         (next-inside-name (intern
                            (concat "targets-inside-next-" mode name)))
         (next-around-name (intern
                            (concat "targets-around-next-" mode name)))
         (last-inner-name (intern
                           (concat "targets-inner-last-" mode name)))
         (last-a-name (intern (concat "targets-a-last-" mode name)))
         (last-inside-name (intern
                            (concat "targets-inside-last-" mode name)))
         (last-around-name (intern
                            (concat "targets-around-last-" mode name)))
         (remote-inner-name (intern
                             (concat "targets-inner-remote-" mode name)))
         (remote-a-name (intern (concat "targets-a-remote-" mode name)))
         (remote-inside-name (intern
                              (concat "targets-inside-remote-" mode name)))
         (remote-around-name (intern
                              (concat "targets-around-remote-" mode name)))
         (select-inner `(targets--select-to-with-seeking
                         ',to-type 'inner ,linewise ,open ,close beg end type
                         count))
         (select-a `(targets--select-to-with-seeking
                     ',to-type 'a ,linewise ,open ,close beg end type count))
         ;; linewise is only applicable for objects (no inside/around for)
         (select-inside `(targets--select-to-with-seeking
                          ',to-type 'inside nil ,open ,close
                          beg end type count))
         (select-around `(targets--select-to-with-seeking
                          ',to-type 'around nil ,open ,close
                          beg end type count))
         (more-keys (when more-keys
                      (if (listp more-keys)
                          more-keys
                        (list more-keys))))
         (keys (if keys
                   (if (listp keys)
                       keys
                     (list keys))
                 (cl-case to-type
                   (pair (list open close))
                   ((quote separator)
                    (list open)))))
         (keys (append keys more-keys)))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         ,(concat "Select inner " name ".")
         (targets--set-last-text-object #',inner-name)
         ,select-inner)

       (evil-define-text-object ,a-name (count &optional beg end type)
         ,(concat "Select a " name ".")
         (targets--set-last-text-object #',a-name)
         ,select-a)

       ,(unless (eq to-type 'object)
          `(evil-define-text-object ,inside-name (count &optional beg end type)
             ,(concat "Select inside " name ".")
             (targets--set-last-text-object #',inside-name)
             ,select-inside))

       ,(unless (eq to-type 'object)
          `(evil-define-text-object ,around-name (count &optional beg end type)
             ,(concat "Select around " name ".")
             (targets--set-last-text-object #',around-name)
             ,select-around))

       ,@(mapcar (lambda (info)
                   `(evil-define-text-object ,(cl-first info)
                      (count &optional beg end type)
                      ,(concat "Select" (cl-third info) name ".")
                      (targets--set-last-text-object #',(cl-first info))
                      (point-to-register 'targets--reset-position)
                      (setq targets--reset-position t)
                      (when (targets-seek-forward ,open nil ',to-type count)
                        ;; purposely don't give visual info since seeking
                        (setq beg nil end nil)
                        ;; count should only be used for seeking
                        (setq count 1)
                        ,(cl-second info))))
                 (append
                  (list (list next-inner-name select-inner " the next inner ")
                        (list next-a-name select-a " the next outer "))
                  (unless (eq to-type 'object)
                    (list
                     (list next-inside-name select-inside " inside the next ")
                     (list next-around-name select-around
                           " around the next ")))))

       ,@(mapcar
          (lambda (info)
            `(evil-define-text-object ,(cl-first info)
               (count &optional beg end type)
               ,(concat "Select" (cl-third info) name ".")
               (targets--set-last-text-object #',(cl-first info))
               (point-to-register 'targets--reset-position)
               (setq targets--reset-position t)
               (when (targets-seek-backward ,open ,close ',to-type count)
                 (setq beg nil end nil count 1)
                 ,(cl-second info))))
          (append
           (list (list last-inner-name select-inner " the last inner ")
                 (list last-a-name select-a " the last outer "))
           (unless (eq to-type 'object)
             (list
              (list last-inside-name select-inside " inside the last ")
              (list last-around-name select-around " around the last ")))))

       ,@(mapcar
          (lambda (info)
            `(evil-define-text-object ,(cl-first info)
               (count &optional beg end type)
               ,(concat "Select" (cl-third info) name " using avy.")
               (require 'avy)
               (targets--set-last-text-object #',(cl-first info))
               (setq targets--reset-position t)
               (setq targets--reset-window (get-buffer-window))
               ;; fix repeat info
               (when (evil-repeat-recording-p)
                 (setq
                  evil-repeat-info
                  `(((lambda ()
                       (setq prefix-arg ,current-prefix-arg)
                       (setq unread-command-events
                             ',(listify-key-sequence (this-command-keys)))
                       (call-interactively #',evil-this-operator)))))
                 (evil-repeat-stop))
               (if (numberp
                    ;; will push point to register if succeeds
                    (targets--avy-seek ',(cl-first info) ,open ,close
                                       ',to-type
                                       (lambda ()
                                         ,(cl-second info))))
                   ,(cl-second info)
                 (point-to-register 'targets--reset-position)
                 ;; or else the overlays will remain
                 (keyboard-quit)
                 nil)))
          (append
           (list (list remote-inner-name select-inner " some inner ")
                 (list remote-a-name select-a " some outer "))
           (unless (eq to-type 'object)
             (list
              (list remote-inside-name select-inside " inside some ")
              (list remote-around-name select-around " around some ")))))

       ,(when bind
          `(progn
             ,@(mapcar (lambda (info)
                         `(targets--define-keys ,(cl-first info)
                                                ,(cl-second info)
                                                ,(cl-third info)
                                                ',keys))
                       (append
                        (list
                         `(evil-inner-text-objects-map #',inner-name t)
                         `(evil-outer-text-objects-map #',a-name t)
                         `(evil-inner-text-objects-map #',next-inner-name
                                                       ,next-key)
                         `(evil-outer-text-objects-map #',next-a-name
                                                       ,next-key)
                         `(evil-inner-text-objects-map #',last-inner-name
                                                       ,last-key)
                         `(evil-outer-text-objects-map #',last-a-name
                                                       ,last-key)
                         `(evil-inner-text-objects-map #',remote-inner-name
                                                       ,remote-key)
                         `(evil-outer-text-objects-map #',remote-a-name
                                                       ,remote-key))

                        (unless (eq to-type 'object)
                          (list
                           `(targets-inside-text-objects-map #',inside-name t)
                           `(targets-around-text-objects-map #',around-name t)
                           `(targets-inside-text-objects-map #',next-inside-name
                                                             ,next-key)
                           `(targets-around-text-objects-map #',next-around-name
                                                             ,next-key)
                           `(targets-inside-text-objects-map #',last-inside-name
                                                             ,last-key)
                           `(targets-around-text-objects-map #',last-around-name
                                                             ,last-key)
                           `(targets-inside-text-objects-map
                             #',remote-inside-name ,remote-key)
                           `(targets-around-text-objects-map
                             #',remote-around-name ,remote-key))))))))))

;;; * Specific Text Objects
(defun targets--set-last-text-object (to)
  "Helper to set the last text object to TO."
  (if (evil-visual-state-p)
      (setq targets--last-visual-text-object to)
    (setq targets--last-operator-text-object to)))

(defun targets--clear-last-visual-text-object ()
  "Helper to clear `targets--last-visual-text-object'."
  (setq targets--last-visual-text-object nil))

(add-hook 'evil-visual-state-exit-hook #'targets--clear-last-visual-text-object)

;;;###autoload
(defun targets-last-text-object ()
  "Run the last text object or fall back to `targets-default-text-object'."
  (interactive)
  (call-interactively (or (if (evil-visual-state-p)
                              targets--last-visual-text-object
                            targets--last-operator-text-object)
                          targets-default-text-object)))

;;; * Setup
(add-hook 'post-command-hook #'targets--reset-position)

(defun targets--setup (inside-key around-key next-key last-key remote-key)
  "Set up basic configuration for targets.el.
See `targets-setup' for more details."
  ;; bind inside and around keymaps
  (when inside-key
    (define-key evil-operator-state-map
      inside-key targets-inside-text-objects-map)
    (if (string= inside-key "I")
        (define-key evil-visual-state-map
          inside-key '(menu-item
                       "maybe-targets-inside-text-objects-map"
                       nil
                       :filter (lambda (&optional _)
                                 (if (eq (evil-visual-type) 'block)
                                     #'evil-insert
                                   targets-inside-text-objects-map))))
      (define-key evil-visual-state-map
        inside-key targets-inside-text-objects-map)))

  (when around-key
    (define-key evil-operator-state-map
      around-key targets-around-text-objects-map)
    (if (string= around-key "A")
        (define-key evil-visual-state-map
          around-key '(menu-item
                       "maybe-targets-around-text-objects-map"
                       nil
                       :filter (lambda (&optional _)
                                 (if (eq (evil-visual-type) 'block)
                                     #'evil-append
                                   targets-around-text-objects-map))))
      (define-key evil-visual-state-map
        around-key targets-around-text-objects-map)))

  ;; unbind intermediate keys
  (when next-key
    (define-key evil-inner-text-objects-map next-key nil)
    (define-key evil-outer-text-objects-map next-key nil))
  (when last-key
    (define-key evil-inner-text-objects-map last-key nil)
    (define-key evil-outer-text-objects-map last-key nil))
  (when remote-key
    (define-key evil-inner-text-objects-map remote-key nil)
    (define-key evil-outer-text-objects-map remote-key nil)))

;;;###autoload
(cl-defmacro targets-setup (&optional bind &key
                                      (inside-key "I")
                                      (around-key "A")
                                      (next-key "n")
                                      (last-key "l")
                                      (remote-key "r"))
  "Perform basic setup for targets.el.
All text objects in `targets-text-objects' are created and optionally bound.
BIND, NEXT-KEY, LAST-KEY, and REMOTE-KEY are all passed to `targets-define-to'.
They can be individually overridden in the entries in `targets-text-objects'.
INSIDE-KEY and AROUND-KEY are bound to `targets-inside-text-objects-map' and
`targets-around-text-objects-map' respectively. If they are not changed from
their default \"I\" and \"A\", they will be bound for the char and line visual
types but not for the block visual type. If NEXT-KEY, LAST-KEY, or REMOTE-KEY
are specified as nil, the corresponding text objects will not be bound.
Otherwise, those intermediate keys will be unbound before `targets-define-to' is
run."
  `(progn
     (targets--setup ,inside-key ,around-key ,next-key ,last-key ,remote-key)
     ;; create and bind text objects
     ,@(mapcar (lambda (to-args)
                 `(targets-define-to ,@(append to-args
                                               (list :bind bind
                                                     :next-key next-key
                                                     :last-key last-key
                                                     :remote-key remote-key))))
               targets-text-objects)))

(provide 'targets)
;;; targets.el ends here
