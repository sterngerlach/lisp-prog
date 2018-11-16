
; union.lsp

(defun average (x y)
    (let ((sum (+ x y)))
        (list x y 'average 'is (/ sum 2.0))))

(defun price-change (old new)
    (let* ((diff (- new old))
           (proportion (/ diff old))
           (percentage (* proportion 100.0)))
          (list 'changed 'by percentage 'percent)))

(defun mass-to-energy (m)
    (let ((c 300000.0))
         (* m c c)))

