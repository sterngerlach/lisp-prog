
; car-cdr-recursion.lsp

(defun tr-fact (n)
    (tr-fact-body n 1))

(defun tr-fact-body (n ret)
    (if (= n 1) ret
        (tr-fact-body (- n 1) (* n ret))))

(defun tr-rev (lst)
    (tr-rev-body lst ()))

(defun tr-rev-body (lst ret)
    (if (= 0 (length lst)) ret
        (tr-rev-body (rest lst) (append (list (first lst)) ret))))

(defun tr-count-down (n)
    (tr-count-down-body n ()))

(defun tr-count-down-body (n ret)
    (if (= 0 n) ret
        (tr-count-down-body (- n 1) (append ret (list n)))))

