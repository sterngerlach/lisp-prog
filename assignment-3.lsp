
; assignment-3.lsp
; (load "assignment-3.lsp")

(defun add-up (lst)
    (if (null lst) 0 (+ (first lst) (add-up (rest lst)))))

(defun laugh (n)
    (if (zerop n)
        nil
        (cons 'HA (laugh (- n 1)))))

(defun count-down (n)
    (if (zerop n)
        nil
        (cons n (count-down (- n 1)))))

(defun square-list (n)
    (if (null lst) nil 
        (cons (expt (first lst) 2)
              (square-list (rest lst)))))

(defun anyoddp (lst)
    (if (null lst)
        nil
        (if (oddp (first lst))
            t
            (anyoddp (rest lst)))))

(defun extract-numbers (lst)
    (cond ((null lst) nil)
          ((numberp (first lst))
              (cons (first lst) (extract-numbers (rest lst))))
          (t (extract-numbers (rest lst)))))

(defun count-odd (lst)
    (cond ((not lst) 0)
          ((oddp (first lst)) (+ 1 (count-odd (rest lst))))
          (t (count-odd (rest lst)))))

