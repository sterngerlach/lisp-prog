
; recursion.lsp

(defun fact (n)
    (if (zerop n) 1 (* n (fact (- n 1)))))

(defun fact2 (n)
    (cond ((zerop n) 1)
          (t (* n (fact2 (- n 1))))))

(defun fib (n)
    (cond ((zerop n) 1)
          ((equal n 1) 1)
          (t (+ (fib (- n 1)) (fib (- n 2))))))

(defun comb (n m)
    (if (or (zerop m) (equal n m)) 1
        (+ (comb (- n 1) m) (comb (- n 1) (- m 1)))))

(defun ack (x y)
    (cond ((zerop x) (+ y 1))
          ((and (> x 0) (zerop y)) (ack (- x 1) 1))
          ((and (> x 0) (> y 0)) (ack (- x 1) (ack x (- y 1))))))

(defun my-length (l)
    (if (null l) 0 (+ 1 (my-length (rest l)))))

(defun my-nth (n l)
    (if (zerop n) (first l) (my-nth (- n 1) (rest l))))

(defun my-append (x y)
    (if (null x) y (cons (first x) (my-append (rest x) y))))

