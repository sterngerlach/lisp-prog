
; assignment-5.lsp

(defun add1 (lst)
    (mapcar #'(lambda (n) (+ n 1)) lst))

(defun greater-than-five-p (lst)
    (mapcar #'(lambda (n) (> n 5)) lst))

(defun flip (lst)
    (mapcar #'(lambda (sw) (if (eq sw 'on) 'off 'on)) lst))

(defun pick (lst)
    (remove-if #'(lambda (n) (or (> n 5) (< n 1))) lst))

(defun count-the (lst)
    (length (remove-if-not #'(lambda (w) (eq w 'the)) lst)))

(defun my-intersection (lst0 lst1)
    (remove-if-not #'(lambda (x) (member x lst1)) lst0))

(defun roughly-equal (x k)
    (remove-if-not #'(lambda (n) (< (abs (- n k)) 10)) x))

; 関数fは恒等関数
(defun f (x) (mapcar #'(lambda (e) e) x))

(defun drawline (n)
    (if (<= n 1) (format t "*")
        (progn (format t "*") (drawline (- n 1)))))

(defun drawbox (m n)
    (if (<= n 1) (progn (drawline m) (format t "~%"))
        (progn (drawline m) (format t "~%") (drawbox m (- n 1)))))

