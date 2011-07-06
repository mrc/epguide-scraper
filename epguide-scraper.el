(require 'cl)
(require 'parse-csv)

(defun extract-csv-rows-from-epguide-html (content)
  "CSV rows are contained within the sole <textarea> on the page."
  (let* ((splitted (split-string content "\n" t))
         (begin (position-if (lambda (s) (string-match "^<textarea" s)) splitted))
         (end (position-if (lambda (s) (string-match "</textarea>" s)) splitted)))
    (subseq splitted (+ 1 begin) end)))


(defun parse-guide (csv-rows)
  "Extract a list of episodes from CSV data."
  (let ((parsed (mapcar 'csv->list csv-rows)))
    (cdr parsed)))

(defun episode-number    (ep) (nth 0 ep))
(defun episode-season    (ep) (nth 1 ep))
(defun episode-in-series (ep) (nth 2 ep))
(defun episode-airdate   (ep)
  "Convert epguides.com time (like 14/Mar/79) to ISO date (1979-03-14)."
  (let* ((airdate-string (nth 4 ep))
         (time (parse-time-string airdate-string))
         (decoded-time (append (list 0 0 0) (subseq time 3 6)))
         (encoded-time (apply 'encode-time decoded-time)))
    (format-time-string "%Y-%m-%d" encoded-time)))
(defun episode-title     (ep) (nth 5 ep))
(defun episode-special-p (ep) (string-equal (nth 6 ep) "y"))
