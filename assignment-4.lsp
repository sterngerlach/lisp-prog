
; assignment-4.lsp

(defun rev (lst)
    (if (equal 1 (length lst))
        lst
        (append (rev (rest lst)) (list (first lst)))))

(defun rm (elem lst)
    (cond ((null lst) nil)
          ((equal elem (first lst)) (rm elem (rest lst)))
          (t (cons (first lst) (rm elem (rest lst))))))

(defun mem (elem lst)
    (cond ((null lst) nil)
          ((equal elem (first lst)) lst)
          (t (mem elem (rest lst)))))

(defun beforep (e0 e1 lst)
    (mem e1 (mem e0 lst)))

(defun rmdup (lst)
    (cond ((null lst) nil)
          ((mem (first lst) (rest lst))
              (rmdup (rest lst)))
          (t (append (list (first lst)) (rmdup (rest lst))))))

(defun wa (lst0 lst1)
    (rmdup (append lst0 lst1)))

(defun seki (lst0 lst1)
    (cond ((or (null lst0) (null lst1)) nil)
          ((mem (first lst0) lst1)
              (append (list (first lst0)) (seki (rest lst0) lst1)))
          (t (seki (rest lst0) lst1))))

(defun sa (lst0 lst1)
    (cond ((null lst0) nil)
          ((mem (first lst0) lst1) (sa (rest lst0) (rm (first lst0) lst1)))
          (t (append (list (first lst0)) (sa (rest lst0) lst1)))))

(defun lookup (key dict)
    (cond ((null dict) nil)
          ((equal (first (first dict)) key) (first dict))
          (t (lookup key (rest dict)))))

(defun sum-tree0 (tree)
    (cond ((not tree) 0)
          ((atom tree) (if (numberp tree) tree 0))
          (t (+ (sum-tree (first tree)) (sum-tree (rest tree))))))

(defun sum-tree1 (tree)
    (cond ((not tree) 0)
          ((symbolp tree) 0)
          ((numberp tree) tree)
          (t (+ (sum-tree (first tree)) (sum-tree (rest tree))))))

(defun flatten (tree)
    (cond ((not tree) nil)
          ((atom tree) (list tree))
          (t (append (flatten (first tree)) (flatten (rest tree))))))

(defun sleepy (tree)
    (cond ((not tree) nil)
          ((atom tree) 'z)
          (t (cons (sleepy (first tree)) (sleepy (rest tree))))))

