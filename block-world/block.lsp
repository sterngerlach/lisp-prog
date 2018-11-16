
; block.lsp

(defun match-element (l r)
    (or (eq l r) (eq r '?)))

(defun match-triple (k0 k1)
    (every #'match-element k0 k1))

(defun fetch (pat)
    (remove-if-not
        #'(lambda (k) (match-triple k pat))
        blockdata))

(defun color-pattern (b)
    (list b 'color '?))

(defun supporters (b)
    (mapcar #'first
        (fetch (list '? 'supports b))))

(defun description (b)
    (remove-if #'(lambda (x) (eq x b))
        (reduce #'append
            (fetch (list b '? '?)))))

(defun supp-cube (b)
    (find-if #'(lambda (x) (eq x 'cube))
        (mapcar #'(lambda (x) (first (last x)))
            (reduce #'append
                (mapcar #'(lambda (x) (fetch (list x 'shape '?)))
                    (supporters b))))))

