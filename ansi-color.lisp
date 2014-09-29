;;;; ansi-color.lisp

(in-package #:ansi-color)

;;; "ansi-color" goes here. Hacks and glory await!

(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))

;; an association list holding pairs of keyword:string
(defvar *attributes*
  (pairlis '(:reset
             :bold :underlined :blink :inverted :concealed
             :black :red :green :yellow
             :blue :magenta :cyan :white
             :bg-black :bg-red :bg-green :bg-yellow
             :bg-blue :bg-magenta :bg-cyan :bg-white)
           '("0"
             "1" "4" "5" "7" "8"
             "30" "31" "32" "33" "34" "35" "36" "37"
             "40" "41" "42" "43" "44" "45" "46" "47")))

;; print an escape sequence that corresponds to ATTR
(defun princ-attr (attr)
  (princ (format nil "~c[~am" #\Esc (cdr (assoc attr *attributes*)))))

;; print a variable number of arguments, which can be strings or keywords
;; corresponding to entries in *attributes*
(defun princa (&rest rest)
  (let ((head (car (flatten rest))))
    (when head
      (if (keywordp head)
          (princ-attr head)
          (princ head))
      (princa (cdr (flatten rest))))))

;; a demo
(defun ansi-demo ()
  (mapc (lambda (x)
          (princa (car x) "This is " (symbol-name (car x)) "." :reset)
          (fresh-line))
        *attributes*))
