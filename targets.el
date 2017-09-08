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

(defvar targets-user-text-objects nil
  "Defines user text objects.
These take precedence over text objects in `targets-text-objects'.")

(defvar targets-text-objects
  (append targets-pair-text-objects
          targets-quote-text-objects
          targets-separator-text-objects
          targets-object-text-objects)
  "A list of text objects to be defined with `targets-setup'.
Each item should be a valid arglist for `targets-define-to'.")

(defvar targets-composite-text-objects nil
  "A list of composite text objects to be defined with `targets-setup'.
Each item should be a valid arglist for `targets-define-composite-to'.")

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

(defvar targets-settings-alist nil
  "An alist of text object names to settings.
This variable allows specifying settings specific to individual text objects.
Each car can be a sybmol or a regexp. Each cdr is a list of variable bindings
for the matched text object(s) (like the first argument to `let'). Only the
bindings for the first matched text object will be used.")

;;; * User-customizable Functions
(defcustom targets-push-jump-p #'targets-push-jump-p
  "Function that determines whether to push to the evil jump list after seeking.
See the default function `targets-push-jump-p' for information on how this
function should behave."
  :type 'function)


(defcustom targets-bound #'targets-bound
  "Function that determines the bound when seeking.
See the default function `targets-bound' for information on how this function
should behave."
  :type 'function)

(defun targets-push-jump-p (old-pos new-pos)
  "Whether or not to push to the evil jump list after a successful seek.
This default function will push to the jump list when OLD-POS and NEW-POS are
not on the same line. A custom function can be used by changing the
`targets-push-jump-p' variable."
  (not (= (line-number-at-pos old-pos) (line-number-at-pos new-pos))))

(defun targets-bound (&optional backwards)
  "Return the bound to be used when seeking forwards or backwards.
BACKWARDS specifies that the bound should be for seeking backwards. This
function is used both when there is no text object at the point and for next and
last text objects. This default function bounds seeking to the beginning and end
of the window. A custom function can be used by changing the `targets-bound'
variable."
  (if backwards
      (window-start)
    (window-end)))

;;; * Seeking Functions
(defun targets--min (&rest numbers)
  "Same as `min' but remove nils from NUMBERS."
  (apply #'min (remove nil numbers)))

(defun targets--max (&rest numbers)
  "Same as `max' but remove nils from NUMBERS."
  (apply #'max (remove nil numbers)))

(defun targets-seek-forward (open _ type &optional count bound)
  "Seek forward to the text object specified by OPEN and TYPE COUNT times.
If BOUND is non-nil, do not seek beyond BOUND. If successful, this function will
move the point to beginning of the match and return its position."
  (setq count (or count 1))
  (setq bound (targets--min bound (funcall targets-bound) (point-max)))
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
  (setq bound (targets--max bound (funcall targets-bound t) (point-min)))
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
    (let ((open (if (listp open)
                    open
                  (list open)))
          (type (if (listp type)
                    type
                  (list type)))
          all-to-positions)
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
                (push (point) to-positions))
              (dotimes (i (length open))
                (while (setq to-pos (targets-seek-forward
                                     (nth i open) nil (nth i type)
                                     1 (cdr bounds)))
                  (push to-pos to-positions)
                  (goto-char to-pos))
                (goto-char (car bounds)))
              (setq all-to-positions
                    (append all-to-positions
                            (mapcar (lambda (x) (cons x current-window))
                                    (sort (delete-dups to-positions) #'<))))))))
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
  (save-excursion
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
  (save-excursion
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
      range)))

(defun targets--define-keys (hooks prefix infix keys def)
  "Helper function used to bind keys.
When HOOKS is non-nil, add a function to each hook to bind the keys locally.
Otherwise bind the keys in the global visual and operator state maps. The keys
will be constructed by combining PREFIX, INFIX, and entries in KEYS. If PREFIX
or INFIX is nil, no keybindings will be made. If INFIX is non-nil but not a
string, it will be excluded. All keys will be bound to DEF."
  (when (and prefix infix)
    (while keys
      (let ((key (concat prefix
                         (if (stringp infix)
                             infix
                           nil)
                         (pop keys))))
        (if hooks
            (dolist (hook hooks)
              (add-hook
               hook
               `(lambda ()
                  (define-key evil-visual-state-local-map ,key #',def)
                  (define-key evil-operator-state-local-map ,key #',def))
               t))
          (define-key evil-operator-state-map key def)
          (define-key evil-visual-state-map key def))))))

(defun targets--local-visual-setup (&optional A)
  "Locally bind I or A to behave as normal for visual block selections."
  (if A
      (make-local-variable 'targets-around-text-objects-map)
    (make-local-variable 'targets-inside-text-objects-map))
  (define-key evil-visual-state-local-map (if A "A" "I")
    `(menu-item
      ""
      nil
      :filter (lambda (&optional _)
                (if (eq (evil-visual-type) 'block)
                    ,(if A #''evil-append #''evil-insert)
                  ,(if A
                       'targets-around-text-objects-map
                     'targets-inside-text-objects-map))))))

(defun targets--define-local-I ()
  "Locally bind I to ensure that it acts as `evil-insert' for visual block."
  (targets--local-visual-setup))

(defun targets--define-local-A ()
  "Locally bind A to ensure that it acts as `evil-append' for visual block."
  (targets--local-visual-setup t))

(defmacro targets--let (name letbinds body)
  "Helper for binding text object specific settings.
NAME corresponds to the name of the current text object and will be used to
match settings in `targets-settings-alist'. LETBINDS can contain default
bindings that bindings in `targets-settings-alist' will override."
  (declare (indent 2))
  `(let (,@(append
            letbinds
            (or (cdr (assq name targets-settings-alist))
                (assoc-default
                 name
                 targets-settings-alist
                 (lambda (regexp name)
                   (and (stringp regexp)
                        (string-match regexp (symbol-name name))))))))
     ,@body))

(defmacro targets--define-text-object (name docstring letbinds &rest body)
  "Wrapper for `evil-define-text-object'.)
NAME and DOCSTRING are the name and docstring for the text object. The last
targets text object will be set to NAME. LETBINDS will be bound around BODY
along with any matched settings from `targets-settings-alist'. When an option is
in both, the latter will take precedence."
  (declare (indent 3))
  `(evil-define-text-object ,name (count &optional beg end type)
     ,docstring
     (targets--set-last-text-object #',name)
     (targets--let ,name ,letbinds
       ,body)))

;; ** targets-define-to
;;;###autoload
(cl-defmacro targets-define-to (name open close to-type &key
                                     linewise
                                     let
                                     bind
                                     (inner-key "i")
                                     (a-key "a")
                                     (inside-key "I")
                                     (around-key "A")
                                     (next-key "n")
                                     (last-key "l")
                                     (remote-key "r")
                                     keys
                                     more-keys
                                     hooks)
  "The main text object definition facility provided by targets.
NAME is used to name the resulting text objects (e.g. targets-inner-NAME). OPEN,
CLOSE, and TO-TYPE hold the required information to create the text objects.
TO-TYPE is one of pair, quote, separator, or object. OPEN and CLOSE should be
strings. CLOSE is only used for pair text objects. LINEWISE is only used for
object type text objects.

LET can be specified as a list of bindings (just like the first argument to
`let') to locally bind variables in each of the created text objects.

If BIND is non-nil, additionally bind all of the created text objects.
INNER-KEY, A-KEY, INSIDE-KEY, and AROUND-KEY can be changed to alter the prefix
keys used for the corresponding text object types. NEXT-KEY, LAST-KEY, and
REMOTE-KEY can be changed to alter the intermediate keys used for next, last,
and remote text objects. If any of these are nil, the corresponding text objects
will not be bound at all. IF KEYS is not specified, it will default to OPEN and
CLOSE. If TO-TYPE is object or OPEN or CLOSE are regexps/multiple
characters (for TO-TYPE pair or separator), KEYS must be explicitly specified if
BIND is non-nil. MORE-KEYS can be used to specify keys to be used in addtion to
OPEN/CLOSE. Both KEYS and MORE-KEYS can be a single key or a list of keys.

If HOOKS is non-nil, add functions to locally bind the keys to the specified
hooks instead of binding the keys globally. HOOKS can be either a single hook or
a list of hooks."
  (let* ((name (if (symbolp name)
                   (symbol-name name)
                 name))
         (inner-name (intern (concat "targets-inner-" name)))
         (a-name (intern (concat "targets-a-"  name)))
         (inside-name (intern (concat "targets-inside-" name)))
         (around-name (intern (concat "targets-around-"  name)))
         (next-inner-name (intern (concat "targets-inner-next-" name)))
         (next-a-name (intern (concat "targets-a-next-" name)))
         (next-inside-name (intern (concat "targets-inside-next-" name)))
         (next-around-name (intern (concat "targets-around-next-" name)))
         (last-inner-name (intern (concat "targets-inner-last-" name)))
         (last-a-name (intern (concat "targets-a-last-" name)))
         (last-inside-name (intern (concat "targets-inside-last-" name)))
         (last-around-name (intern (concat "targets-around-last-" name)))
         (remote-inner-name (intern (concat "targets-inner-remote-" name)))
         (remote-a-name (intern (concat "targets-a-remote-" name)))
         (remote-inside-name (intern (concat "targets-inside-remote-" name)))
         (remote-around-name (intern (concat "targets-around-remote-" name)))
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
         (hooks (if (listp hooks)
                    hooks
                  (list hooks)))
         (keys (append keys more-keys)))
    `(progn
       (targets--define-text-object ,inner-name
           ,(concat "Select inner " name ".")
           ,let
         ,select-inner)

       (targets--define-text-object ,a-name
           ,(concat "Select a " name ".")
           ,let
         ,select-a)

       ,(unless (eq to-type 'object)
          `(targets--define-text-object ,inside-name
               ,(concat "Select inside " name ".")
               ,let
             ,select-inside))

       ,(unless (eq to-type 'object)
          `(targets--define-text-object ,around-name
               ,(concat "Select around " name ".")
               ,let
             ,select-around))

       ,@(mapcar (lambda (info)
                   `(targets--define-text-object ,(cl-first info)
                        ,(concat "Select" (cl-third info) name ".")
                        ,let
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
            `(targets--define-text-object ,(cl-first info)
                 ,(concat "Select" (cl-third info) name ".")
                 ,let
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
            `(targets--define-text-object ,(cl-first info)
                 ,(concat "Select" (cl-third info) name " using avy.")
                 ,let
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
             (when (and ,around-key ',hooks
                        (string= ,around-key "A"))
               (dolist (hook ',hooks)
                 (add-hook hook #'targets--define-local-A t)))
             (when (and ,inside-key ',hooks
                        (string= ,inside-key "I"))
               (dolist (hook ',hooks)
                 (add-hook hook #'targets--define-local-I t)))
             ,@(mapcar (lambda (info)
                         `(targets--define-keys ',hooks
                                                ,(cl-first info)
                                                ,(cl-second info)
                                                ',keys
                                                ,(cl-third info)))
                       (append
                        (list
                         `(,inner-key t #',inner-name)
                         `(,a-key t #',a-name)
                         `(,inner-key ,next-key #',next-inner-name)
                         `(,a-key ,next-key #',next-a-name)
                         `(,inner-key ,last-key #',last-inner-name)
                         `(,a-key ,last-key #',last-a-name)
                         `(,inner-key ,remote-key #',remote-inner-name)
                         `(,a-key ,remote-key #',remote-a-name))
                        (unless (eq to-type 'object)
                          (list
                           `(,inside-key t #',inside-name)
                           `(,around-key t #',around-name)
                           `(,inside-key ,next-key #',next-inside-name)
                           `(,around-key ,next-key #',next-around-name)
                           `(,inside-key ,last-key #',last-inside-name)
                           `(,around-key ,last-key #',last-around-name)
                           `(,inside-key ,remote-key #',remote-inside-name)
                           `(,around-key ,remote-key
                                         #',remote-around-name))))))))))

;; ** targets-define-composite-to
(defun targets--composite-seek (text-objects &optional backwards count)
  "Seek forward to any text object in TEXT-OBJECTS.
If BACKWARDS is non-nil seek backwards. If COUNT is non-nil, seek that many
times. If successful, return the matched position (otherwise nil)."
  (let ((orig-pos (point)))
    (cl-dotimes (_ (or count 1))
      (let (positions)
        (dolist (seek-args text-objects)
          (save-excursion
            (setq seek-args (cl-subseq seek-args 0 3))
            (push (if backwards
                      (apply #'targets-seek-backward seek-args)
                    (apply #'targets-seek-forward seek-args))
                  positions)))
        (setq positions (delq nil positions))
        (if positions
            (goto-char (if backwards
                           (apply #'max positions)
                         (apply #'min positions)))
          (cl-return-from nil))))
    (unless (= (point) orig-pos)
      (point))))

(defun targets--select-composite-to-with-seeking (beg end text-objects)
  "Like `targets--select-to-with-seeking' but for composite text objects.
BEG and END correspond to a previous range or current visual selection.
TEXT-OBJECTS is a list of lists of arguments for
`targets--select-to-with-seeking'."
  (let* (ranges
         after-ranges
         before-ranges
         ;; ignore 1-char visual selection (visual selection just started)
         ;; necessary when moving point forward to account for inner/inside
         ;; text object ranges
         (visualp (and beg end (not (= (- end beg) 1)))))
    (dolist (to-args text-objects)
      (let ((range (apply #'targets--select-to-with-seeking to-args))
            (open (cl-fourth to-args))
            (to-type (car to-args)))
        (save-excursion
          (when range
            (when (and (not (eq to-type 'object))
                       (looking-at (regexp-quote open)))
              ;; so point will still be inside range for inner/inside text
              ;; objects
              (goto-char (match-end 0))
              (skip-chars-forward " \t"))
            (cond
             ;; new range must not be contained within the current one
             ((and visualp (>= (car range) beg) (<= (cadr range) end)))
             ((> (car range) (if visualp beg (point)))
              (push range after-ranges))
             ((< (cadr range) (if visualp end (point)))
              (push range before-ranges))
             (t
              (push range ranges)))))))
    (cond (ranges
           ;; favor the smallest around the point/region
           (car (sort ranges (lambda (x y) (< (- (cadr x) (car x))
                                              (- (cadr y) (car y)))))))
          ((setq ranges (or ranges after-ranges before-ranges))
           ;; favor the closest when seeking
           (car (cl-sort ranges (if after-ranges
                                    #'<
                                  #'>)
                         :key (if after-ranges
                                  #'car
                                #'cadr)))))))

(defun targets--select-composite
    (select-type beg end type count user-to-args)
  "Select a composite text object.
This function does the necessary processing before running
`targets--select-composite-to-with-seeking' and allows for counts (for expanding
a region)."
  (let (range
        new-range
        ;; disable seeking with count
        (targets-seek-functions (if (= count 1)
                                    targets-seek-functions
                                  nil)))
    (cl-dotimes (_ count)
      (setq new-range
            (funcall #'targets--select-composite-to-with-seeking
                     beg end
                     (mapcar (lambda (to-arg)
                               (list (cl-third to-arg)
                                     select-type
                                     (cl-fourth to-arg)
                                     (cl-first to-arg)
                                     (cl-second to-arg)
                                     beg
                                     end
                                     type
                                     1))
                             user-to-args)))
      (if new-range
          (setq beg (car new-range)
                end (cadr new-range)
                range new-range)
        (cl-return-from nil)))
    range))

(cl-defmacro targets-define-composite-to (name to-args &key
                                               let
                                               bind
                                               (inner-key "i")
                                               (a-key "a")
                                               (inside-key "I")
                                               (around-key "A")
                                               (next-key "n")
                                               (last-key "l")
                                               (remote-key "r")
                                               keys
                                               hooks)
  "Define a composite text object.
NAME is used to name the resulting text objects (e.g. targets-inner-NAME).
TO-ARGS is a list of list of arguments like you would pass to
`targets-define-to': ((open close type &key linewise)...).

LET, BIND, INNER-KEY, A-KEY, INSIDE-KEY, AROUND-KEY, NEXT-KEY, LAST-KEY,
REMOTE-KEY, KEYS, and HOOKS all behave the same as in `targets-define-to', but
there is no MORE-KEYS. KEYS must always be manually specified."
  (declare (indent 1))
  (let* ((name (if (symbolp name)
                   (symbol-name name)
                 name))
         (inner-name (intern (concat "targets-inner-" name)))
         (a-name (intern (concat "targets-a-" name)))
         (inside-name (intern (concat "targets-inside-" name)))
         (around-name (intern (concat "targets-around-" name)))
         (next-inner-name (intern (concat "targets-inner-next-" name)))
         (next-a-name (intern (concat "targets-a-next-" name)))
         (next-inside-name (intern (concat "targets-inside-next-" name)))
         (next-around-name (intern (concat "targets-around-next-" name)))
         (last-inner-name (intern (concat "targets-inner-last-" name)))
         (last-a-name (intern (concat "targets-a-last-" name)))
         (last-inside-name (intern (concat "targets-inside-last-" name)))
         (last-around-name (intern (concat "targets-around-last-" name)))
         (remote-inner-name (intern (concat "targets-inner-remote-" name)))
         (remote-a-name (intern (concat "targets-a-remote-" name)))
         (remote-inside-name (intern (concat "targets-inside-remote-" name)))
         (remote-around-name (intern (concat "targets-around-remote-" name)))
         (select-inner `(targets--select-composite
                         'inner beg end type count ',to-args))
         (select-a `(targets--select-composite
                     'a beg end type count ',to-args))
         (select-inside `(targets--select-composite
                          'inside beg end type count ',to-args))
         (select-around `(targets--select-composite
                          'around beg end type count ',to-args))
         (keys (if (listp keys)
                   keys
                 (list keys)))
         (hooks (if (listp hooks)
                    hooks
                  (list hooks))))
    `(progn
       (targets--define-text-object ,inner-name
           ,(concat "Select inner " name ".")
           ,let
         ,select-inner)

       (targets--define-text-object ,a-name
           ,(concat "Select a " name ".")
           ,let
         ,select-a)

       (targets--define-text-object ,inside-name
           ,(concat "Select inside " name ".")
           ,let
         ,select-inside)

       (targets--define-text-object ,around-name
           ,(concat "Select around " name ".")
           ,let
         ,select-around)

       ,@(mapcar
          (lambda (info)
            `(targets--define-text-object ,(cl-first info)
                 ,(concat "Select" (cl-third info) name ".")
                 ,let
               (point-to-register 'targets--reset-position)
               (setq targets--reset-position t)
               (when (targets--composite-seek ',to-args nil count)
                 ;; purposely don't give visual info since seeking
                 (setq beg nil end nil)
                 ;; count should only be used for seeking
                 (setq count 1)
                 ,(cl-second info))))
          (list (list next-inner-name select-inner " the next inner ")
                (list next-a-name select-a " the next outer ")
                (list next-inside-name select-inside " inside the next ")
                (list next-around-name select-around " around the next ")))

       ,@(mapcar
          (lambda (info)
            `(targets--define-text-object ,(cl-first info)
                 ,(concat "Select" (cl-third info) name ".")
                 ,let
               (targets--set-last-text-object #',(cl-first info))
               (point-to-register 'targets--reset-position)
               (setq targets--reset-position t)
               (when (targets--composite-seek ',to-args t count)
                 (setq beg nil end nil count 1)
                 ,(cl-second info))))
          (list (list last-inner-name select-inner " the last inner ")
                (list last-a-name select-a " the last outer ")
                (list last-inside-name select-inside " inside the last ")
                (list last-around-name select-around " around the last ")))

       ,@(mapcar
          (lambda (info)
            `(targets--define-text-object ,(cl-first info)
                 ,(concat "Select" (cl-third info) name " using avy.")
                 ,let
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
                    (targets--avy-seek ',(cl-first info)
                                       ',(mapcar #'cl-first to-args)
                                       ',(mapcar #'cl-second to-args)
                                       ',(mapcar #'cl-third to-args)
                                       (lambda ()
                                         ,(cl-second info))))
                   ,(cl-second info)
                 (point-to-register 'targets--reset-position)
                 ;; or else the overlays will remain
                 (keyboard-quit)
                 nil)))
          (list (list remote-inner-name select-inner " some inner ")
                (list remote-a-name select-a " some outer ")
                (list remote-inside-name select-inside " inside some ")
                (list remote-around-name select-around " around some ")))

       ,(when bind
          `(progn
             (when (and ,around-key ',hooks
                        (string= ,around-key "A"))
               (dolist (hook ',hooks)
                 (add-hook hook #'targets--define-local-A t)))
             (when (and ,inside-key ',hooks
                        (string= ,inside-key "I"))
               (dolist (hook ',hooks)
                 (add-hook hook #'targets--define-local-I t)))
             ,@(mapcar (lambda (info)
                         `(targets--define-keys ',hooks
                                                ,(cl-first info)
                                                ,(cl-second info)
                                                ',keys
                                                ,(cl-third info)))
                       (append
                        (list
                         `(,inner-key t #',inner-name)
                         `(,a-key t #',a-name)
                         `(,inner-key ,next-key #',next-inner-name)
                         `(,a-key ,next-key #',next-a-name)
                         `(,inner-key ,last-key #',last-inner-name)
                         `(,a-key ,last-key #',last-a-name)
                         `(,inner-key ,remote-key #',remote-inner-name)
                         `(,a-key ,remote-key #',remote-a-name)
                         `(,inside-key t #',inside-name)
                         `(,around-key t #',around-name)
                         `(,inside-key ,next-key #',next-inside-name)
                         `(,around-key ,next-key #',next-around-name)
                         `(,inside-key ,last-key #',last-inside-name)
                         `(,around-key ,last-key #',last-around-name)
                         `(,inside-key ,remote-key #',remote-inside-name)
                         `(,around-key ,remote-key
                                       #',remote-around-name)))))))))

;;; * Specific Text Objects
(defun targets--set-last-text-object (to)
  "Helper to set the last text object to TO."
  (if (evil-visual-state-p)
      (setq targets--last-visual-text-object to)
    (setq targets--last-operator-text-object to)))

(defun targets--clear-last-visual-text-object ()
  "Helper to clear `targets--last-visual-text-object'."
  (setq targets--last-visual-text-object nil))

;;;###autoload
(defun targets-last-text-object ()
  "Run the last text object or fall back to `targets-default-text-object'."
  (interactive)
  (let ((to (or (if (evil-visual-state-p)
                    targets--last-visual-text-object
                  targets--last-operator-text-object)
                targets-default-text-object)))
    (when to
      (targets--let targets-last-text-object nil
        ((call-interactively to))))))

;;; * Setup
(defun targets--setup (inside-key around-key next-key last-key remote-key)
  "Set up basic configuration for targets.el.
See `targets-setup' for more details."
  (add-hook 'post-command-hook #'targets--reset-position)
  (add-hook 'evil-visual-state-exit-hook
            #'targets--clear-last-visual-text-object)
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
        around-key targets-around-text-objects-map))))

;;;###autoload
(cl-defmacro targets-setup (&optional bind &key
                                      (inner-key "i")
                                      (a-key "a")
                                      (inside-key "I")
                                      (around-key "A")
                                      (next-key "n")
                                      (last-key "l")
                                      (remote-key "r"))
  "Perform basic setup for targets.el.
All text objects in `targets-text-objects' are created and optionally bound.
BIND, INNER-KEY, A-KEY, INSIDE-KEY, AROUND-KEY, NEXT-KEY, LAST-KEY, and
REMOTE-KEY are all passed to `targets-define-to'. They can be individually
overridden in the entries in `targets-text-objects'. INSIDE-KEY and AROUND-KEY
are bound to `targets-inside-text-objects-map' and
`targets-around-text-objects-map' respectively. If they are not changed from
their default \"I\" and \"A\", they will be bound for the char and line visual
types but not for the block visual type. If any of the key arguments are
specified as nil, the corresponding text objects will not be bound."
  `(progn
     (targets--setup ,inside-key ,around-key ,next-key ,last-key ,remote-key)
     ;; create and bind text objects
     ,@(mapcar (lambda (to-args)
                 `(targets-define-to ,@(append to-args
                                               (list :bind bind
                                                     :inner-key inner-key
                                                     :a-key a-key
                                                     :inside-key inside-key
                                                     :around-key around-key
                                                     :next-key next-key
                                                     :last-key last-key
                                                     :remote-key remote-key))))
               (append targets-user-text-objects
                       (cl-set-difference
                        targets-text-objects
                        targets-user-text-objects
                        :key #'car)))
     ,@(mapcar (lambda (to-args)
                 `(targets-define-composite-to
                      ,@(append to-args
                                (list :bind bind
                                      :inner-key inner-key
                                      :a-key a-key
                                      :inside-key inside-key
                                      :around-key around-key
                                      :next-key next-key
                                      :last-key last-key
                                      :remote-key remote-key))))
               targets-composite-text-objects)))

(provide 'targets)
;;; targets.el ends here
