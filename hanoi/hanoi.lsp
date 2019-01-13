
; ハノイの塔
; n枚の板を棒fromから棒toへ棒tmpを使って移動する

(defun myhanoi (n from to tmp)
    (cond ((= n 1)
               (format t "板~Sを~Sから~Sへ~%" n from to))   ; 1枚なら素直に移動できる
           (t (myhanoi (- n 1) from tmp to)                 ; 上のn-1枚をまず退避
               (format t "板~Sを~Sから~Sへ~%" n from to)    ; 一番下の板を移動
               (myhanoi (- n 1) tmp to from))))             ; 退避したn-1枚を移動

