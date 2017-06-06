;;---- common-functions ----
(defun filter (pred lst)
  "引数リストlstの要素の中から、predが成立しない要素を取り除いたリストを返す"
  (cond ((null lst) nil)
 ((funcall pred (car lst))
  (cons (car lst) (filter pred (cdr lst))))
 (t (filter pred (cdr lst)))))
(defun is_~filep (x)
  "~が含まれる要素の場合、tを返す"
  (cond ((string-match "~" x) t)
 (t nil)))
(defun is-directory (x)
  "渡された引数がディレクトリを表すかチェックする"
  (let ((l (length x)))
    (not (mismatch (subseq x (- l 1) l) "/")))) ;末尾/はディレクトリとみなす
;;---- parameters ----
(setq *target-directory* "D:/work/log/")
(setq *target-log-files* (filter (lambda (x) (not (is_~filep x))) (directory *target-directory*)))
(setq *del-target-file*  (filter (lambda (x) (is_~filep x)) (directory *target-directory*)))
;;---- sub routines ----
;; clean-up backup files
(defun clean-up-backup (x)
  "ファイル名に~を含むバックアップファイルを削除する"
  (cond ((and (not (null x)) (not (is-directory x)))
  (delete-file x)
  (print (format nil "~a was deleted." x)))
 (t (print (format nil "~a is a directory." x) ))))
(defun clean (parent-dir files)
  "引数で渡されたディレクトリ内をお掃除する"
  (cond
   ((null files) nil)
   ((list files)
    (clean-up-backup (concatenate 'string parent-dir (car files)))
    (clean parent-dir (cdr files)))
   ((atom files) (print-clock-in-out files))
   (t nil)))
;; get clock-in and clock-out
(defun print-clock-in-out (x)
  "Clock ({time}) な形で記述されている行から、{time}部の文字列を取り出して出力する"
  (cond ((and (not (null x)) (not (is-directory x)))
  (print x)
  (with-open-file (in x :direction :input)
    (let (c)
      (while (setq buff (read-line in nil))
        (cond ((string-match "Clock" buff)
        (print (concatenate 'string "\t" (subseq buff (+ (string-match "(" buff) 1)(- (length buff) 1))))))))))
 (t nil)))
(defun print-clock-in-out-on-directory (parent-dir files)
  "処理対象のparent-dirディレクトリのファイルリスト中のファイルについて、ファイル名と共に時間情報を書き出す"
  (cond
   ((null files) nil)
   ((list files)
    (print-clock-in-out (concatenate 'string parent-dir (car files)))
    (print-clock-in-out-on-directory parent-dir (cdr files)))
   ((atom files) (print-clock-in-out files))
   (t nil)))
;;--- main routine
(print-clock-in-out-on-directory *target-directory* *target-log-files*)
(clean *target-directory* *del-target-file*)
