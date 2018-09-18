(defun csv--get-current-field ()
  (let* ((beg (progn (csv-beginning-of-field) (point)))
	 (end (progn (csv-end-of-field) (point))))
    (buffer-substring-no-properties beg end)))

(defun csv--get-one-record ()
  (beginning-of-line)
  (let ((record nil))
    (while (not (eolp))
      (setq record (append record (list (csv--get-current-field))))
      (if (not (eolp))
	  (progn
	    (csv-forward-field 1)
	    (csv-beginning-of-field)))
     )
    record
    )
  )

(defun csv--to-yaml-string (header record &optional indent)
  (if (not indent)
      (setq indent ""))
  (let* ((result "")
	 (key (car header))
	 (val (car record))
	 (header (cdr header))
	 (record (cdr record))
	)
    (setq result (concat result
			 indent "- " key ":\t" val "\n" ))
    (while record
      (setq
       key (car header)
       val (car record)
       header (cdr header)
       record (cdr record))
      (setq result (concat result
			 indent "  " key ":\t" val "\n" )) ; 本当はTabはいけない
      )
    result))


;;; Entory point
(defun csv-to-yaml ()
  (interactive)
  (require 'csv-mode)
  (let ((records nil)
	header
	(oldbuf (current-buffer))
	)
    ;; バッファの先頭に行く
    (goto-char (point-min))
    ;; 全部のレコードを読みだす (csv--get-one-record) をappendする
    (while (not (eobp))
      (setq records (append records (list (csv--get-one-record))))
      (if (not (eobp))
	  (progn
	    (csv-forward-field 1)
	    (csv-beginning-of-field)))
      )
    ;; 先頭行を header にする
    ;; 残りの行を records にする
    (setq header  (car records)
	  records (cdr records))
    ;; 出力するバッファを作る．中身はeraseする
    (switch-to-buffer-other-window "*csv-to-yml*")
    (erase-buffer)
    ;; (erase-buffer)
    ;; recordss の各record に対して
    (while records
      (setq record  (car records)
	    records (cdr records))
      (insert (csv--to-yaml-string header record))
      )
    )
;    (switch-to-buffer-other-window oldbuf)
)
