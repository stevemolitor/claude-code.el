;;; test-example.el --- Example file for testing openDiff -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains some simple functions to test Claude's openDiff feature.

;;; Code:

(defun calculate-factorial (n)
  "Calculate factorial of N."
  (if (<= n 1)
      1
    (* n (calculate-factorial (- n 1)))))

(defun fibonacci (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fibonacci (- n 1))
              (fibonacci (- n 2))))))

(defun greet (name)
  (message "Hello, %s!" name))

(defun sum-list (numbers)
  (apply '+ numbers))

(provide 'test-example)
;;; test-example.el ends here