;;; targets.el --- Extension of evil text objects. -*- lexical-binding: t -*-

;; Author: Fox Kiester <noct@openmailbox.org>
;; URL: https://github.com/noctuid/targets.el
;; Created: November 29, 2016
;; Keywords: evil text-object
;; Package-Requires: ((cl-lib "0.5") (evil "1.1.0"))
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

;;; Code:
(require 'cl-lib)
(require 'evil)

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

(defun targets-push-jump-p (old-pos new-pos)
  "Whether or not to push to the evil jump list after a successful seek.
This default function will push to the jump list when OLD-POS and NEW-POS are
not on the same line. Override or redefine this function to change this behavior
or change `targets-seek-functions' completely instead."
  (not (= (line-number-at-pos old-pos) (line-number-at-pos new-pos))))

(defun targets-seek-forward (open _ type &optional count &rest _)
  "Seek forward to the text object specified by OPEN and TYPE COUNT times."
  (setq count (or count 1))
  (let ((orig-pos (point)))
    (cl-case type
      ((pair separator)
       (re-search-forward (regexp-quote open) nil t count))
      (quote
       (let ((evil-forward-quote-char (string-to-char open)))
         (ignore-errors
           (end-of-thing 'evil-quote))
         ;; count is broken for evil-forward-quote
         (let ((pos (point)))
           (dotimes (_ count)
             (forward-thing 'evil-quote))
           (if (= (point) pos)
               (goto-char orig-pos)
             (forward-char -1)))))
      ;; this happens for next/last text objects not after normal failure
      (object
       (end-of-thing open)
       (let ((pos (point)))
         (forward-thing open count)
         (if (= (point) pos)
             (goto-char orig-pos)
           (beginning-of-thing open)))))
    (targets-push-jump-p orig-pos (point))))

(defun targets-seek-backward (open close type &optional count &rest _)
  "Seek backward to the text object specified by OPEN, CLOSE, and TYPE COUNT
times."
  (setq count (or count 1))
  (let ((orig-pos (point)))
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
                               (re-search-backward quote-regexp nil t)
                               (point)))
                 (open-paren (save-excursion
                               (if parenp
                                   (evil-up-paren open-char close-char -1)
                                 (evil-up-block open close -1))
                               ;; evil-up-block moves to bob if it fails
                               (unless (= (point) 1)
                                 (point)))))
             (when (= prev-paren last-pos)
               (return-from nil))
             (goto-char prev-paren)
             (when (and open-paren (= prev-paren open-paren))
               (re-search-backward quote-regexp nil t))
             (setq last-pos (point))))))
      (separator
       (when (re-search-backward (regexp-quote open) nil t count)
         (forward-char -1)))
      (quote
       (let* ((evil-forward-quote-char (string-to-char open))
              (bnd (bounds-of-thing-at-point 'evil-quote)))
         (when (and bnd
                    ;; the char after the string is given in the bnd
                    (not (= (point) (cdr bnd))))
           (goto-char (car bnd)))
         (if (= -1 (forward-thing 'evil-quote (- count)))
             (goto-char orig-pos)
           (forward-char 1))))
      (object
       (beginning-of-thing open)
       (let ((pos (point)))
         (forward-thing open (- count))
         (if (= (point) pos)
             (goto-char orig-pos)
           ;; may be redundant for some things (e.g. evil-word)
           (beginning-of-thing open)))))
    (targets-push-jump-p orig-pos (point))))

;; TODO gensyms/once-only

(defmacro targets--repeat-with-seeking
    (open close type select-form &optional seek-functions)
  "Return the range of the text object specified by OPEN, CLOSE, and TYPE.
SELECT-FORM is the form used to determine this range. If it is unsuccessful,
seek using the functions in SEEK-FUNCTIONS to attempt to find the text object.
If the last seek function run returns a non-nil value, push the point to the
jump list."
  `(let ((last-pos (point))
         (seek-functions (or ,seek-functions targets-seek-functions))
         (range (ignore-errors ,select-form))
         push-jump-p)
     (when (and range
                (or (> (car range) last-pos)
                    (< (cadr range) last-pos)))
       (setq range nil))
     ;; intentional capture; discard visual information when seeking
     (setq beg nil end nil)
     (while (and (not range) seek-functions)
       (setq push-jump-p (funcall (pop seek-functions) ,open ,close ,type))
       (setq range (ignore-errors ,select-form)))
     (when (and range push-jump-p)
       (evil-set-jump))
     range))

(defun targets--define-keys (keymap function prefix keys)
  "In KEYMAP, bind multiple keys to FUNCTION.
The keys are created by using PREFIX to prefix each key in KEYS."
  (while keys
    (define-key keymap (concat prefix (pop keys)) function)))

(defvar targets--reset-position nil)
(defvar targets--reset-from-next-p nil)
(defvar targets--non-destructive-operators '(evil-yank))

(defun targets--reset-position ()
  "Called after next and last text objects to restore the cursor position.
The point is not restored in visual state."
  (when (and targets--reset-position
             (not (evil-visual-state-p)))
    (if targets--reset-from-next-p
        (goto-char targets--reset-position)
      (jump-to-register 'targets--reset-position))
    (setq targets--reset-position nil)))

;;;###autoload
(cl-defmacro targets-define-to (name open close to-type &key
                                     seek-functions
                                     linewise
                                     mode
                                     bind
                                     (next-key "n")
                                     (last-key "l")
                                     keys
                                     more-keys)
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
         (keys (append keys more-keys))
         (open-char (cl-case to-type
                      (pair (when (and (= (length open) 1)
                                       (= (length close) 1))
                              (string-to-char open)))
                      (quote (string-to-char open))))
         (close-char (when (and open-char (eq to-type 'pair))
                       (string-to-char close)))
         (thing open))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         ,(concat "Select inner " name ".")
         (targets--repeat-with-seeking
          ,open ,close ',to-type
          ,(cl-case to-type
             (pair
              `(evil-select-paren ,(or open-char open) ,(or close-char close)
                                  beg end type count))
             (separator
              `(evil-select-paren ,open ,open beg end type count))
             (quote
              `(evil-select-quote ,open-char beg end type count))
             (object
              `(evil-select-inner-object ,thing beg end type count ,linewise)))
          ,seek-functions))

       (evil-define-text-object ,a-name (count &optional beg end type)
         ,(concat "Select a " name ".")
         (targets--repeat-with-seeking
          ,open ,close ',to-type
          ,(cl-case to-type
             (pair
              `(evil-select-paren ,(or open-char open) ,(or close-char close)
                                  beg end type count t))
             (quote
              `(let ((range
                      (evil-select-quote ,open-char beg end type count)))
                 (when range
                   ;; expand range
                   (setf (car range) (1- (car range))
                         (cadr range) (1+ (cadr range))))
                 range))
             (separator
              `(let ((range
                      (evil-select-paren ,open ,open beg end type count t)))
                 ;; reduce range
                 (when range
                   (setf (cadr range) (1- (cadr range))))
                 range))
             (object
              `(evil-select-an-object ,thing beg end type count ,linewise)))
          ,seek-functions))

       ,(unless (eq to-type 'object)
          `(evil-define-text-object ,inside-name (count &optional beg end type)
             ,(concat "Select inside " name ".")
             (let ((range (,inner-name count beg end type)))
               (when range
                 (setq range (append range (list :expanded t)))
                 (goto-char (car range))
                 (skip-chars-forward " \t")
                 (setf (car range) (point))
                 (goto-char (cadr range))
                 (skip-chars-backward " \t")
                 (setf (cadr range) (point)))
               range)))

       ,(unless (eq to-type 'object)
          `(evil-define-text-object ,around-name (count &optional beg end type)
             ,(concat "Select around " name ".")
             (let ((range (,a-name count beg end type)))
               (when range
                 (setq range (append range (list :expanded t)))
                 ,(when (eq to-type 'separator)
                    '(setf (cadr range) (1+ (cadr range))))
                 (goto-char (cadr range))
                 (skip-chars-forward " \t")
                 (cond ((= (point) (cadr range))
                        (goto-char (car range))
                        (skip-chars-backward " \t")
                        (setf (car range) (point)))
                       (t
                        (setf (cadr range) (point)))))
               range)))

       ,@(mapcar (lambda (info)
                   `(evil-define-text-object ,(cl-first info)
                      (count &optional beg end type)
                      ,(concat "Select" (cl-third info) name ".")
                      (setq targets--reset-position (point)
                            targets--reset-from-next-p t)
                      (targets-seek-forward ,open ,close ',to-type count)
                      ;; purposely don't give visual info since seeking
                      (let ((range (,(cl-second info))))
                        (when range
                          (append range (list :expanded t))))))
                 (append
                  (list (list next-inner-name inner-name " the next inner ")
                        (list next-a-name a-name " the next outer "))
                  (unless (eq to-type 'object)
                    (list
                     (list next-inside-name inside-name " inside the next ")
                     (list next-around-name around-name " around the next ")))))

       ,@(mapcar (lambda (info)
                   `(evil-define-text-object ,(cl-first info)
                      (count &optional beg end type)
                      ,(concat "Select" (cl-third info) name ".")
                      (point-to-register 'targets--reset-position)
                      (setq targets--reset-position t
                            targets--reset-from-next-p nil)
                      (targets-seek-backward ,open ,close ',to-type count)
                      (let ((range (,(cl-second info))))
                        (when range
                          (append range (list :expanded t))))))
                 (append
                  (list (list last-inner-name inner-name " the last inner " nil)
                        (list last-a-name a-name " the last outer " t))
                  (unless (eq to-type 'object)
                    (list
                     (list last-inside-name inside-name " inside the last "
                           nil)
                     (list last-around-name around-name " around the last "
                           t)))))
       ,(when bind
          `(progn
             ,@(mapcar (lambda (info)
                         `(targets--define-keys ,(cl-first info)
                                                ,(cl-second info)
                                                ,(cl-third info)
                                                ',keys))
                       (append
                        (list
                         `(evil-inner-text-objects-map #',inner-name nil)
                         `(evil-outer-text-objects-map #',a-name nil)
                         `(evil-inner-text-objects-map #',next-inner-name
                                                       ,next-key)
                         `(evil-outer-text-objects-map #',next-a-name
                                                       ,next-key)
                         `(evil-inner-text-objects-map #',last-inner-name
                                                       ,last-key)
                         `(evil-outer-text-objects-map #',last-a-name
                                                       ,last-key))
                        (unless (eq to-type 'object)
                          (list
                           `(targets-inside-text-objects-map #',inside-name nil)
                           `(targets-around-text-objects-map #',around-name nil)
                           `(targets-inside-text-objects-map #',next-inside-name
                                                             ,next-key)
                           `(targets-around-text-objects-map #',next-around-name
                                                             ,next-key)
                           `(targets-inside-text-objects-map #',last-inside-name
                                                             ,last-key)
                           `(targets-around-text-objects-map #',last-around-name
                                                             ,last-key))))))))))

;;;###autoload
(cl-defmacro targets-setup (&optional bind &key
                                      (inside-key "I")
                                      (around-key "A")
                                      (next-key "n")
                                      (last-key "l"))
  `(progn
     (define-key evil-visual-state-map
       ,inside-key targets-inside-text-objects-map)
     (define-key evil-operator-state-map
       ,inside-key targets-inside-text-objects-map)
     (define-key evil-visual-state-map
       ,around-key targets-around-text-objects-map)
     (define-key evil-operator-state-map
       ,around-key targets-around-text-objects-map)
     (define-key evil-inner-text-objects-map ,next-key nil)
     (define-key evil-inner-text-objects-map ,last-key nil)
     (define-key evil-outer-text-objects-map ,next-key nil)
     (define-key evil-outer-text-objects-map ,last-key nil)
     ;; add post command hook
     ,@(mapcar (lambda (to-args)
                 `(targets-define-to ,@(append to-args (list :bind bind
                                                             :next-key next-key
                                                             :last-key last-key))))
               targets-text-objects)
     (add-hook 'post-command-hook #'targets--reset-position)))

(provide 'targets)
;;; targets.el ends here
