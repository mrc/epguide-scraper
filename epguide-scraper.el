(require 'cl)
(require 'parse-csv)

(put 'error-converting-date 'error-conditions '(error error-converting-date))
(put 'error-converting-date 'error-message "Error converting date")

(defun extract-csv-rows-from-epguide-html (content)
  "CSV rows are contained within the sole <pre> on the page."
  (let* ((splitted (split-string content "\n" t))
         (begin (position-if (lambda (s) (string-match "^<pre" s)) splitted))
         (end (position-if (lambda (s) (string-match "</pre>" s)) splitted)))
    (subseq splitted (+ 1 begin) end)))

(defun parse-guide (csv-rows)
  "Extract a list of episodes from CSV data."
  (let ((parsed (mapcar 'csv->list csv-rows)))
    (cdr parsed)))

(defun episode-number    (ep) (string-to-number (nth 0 ep)))
(defun episode-season    (ep) (string-to-number (nth 1 ep)))
(defun episode-in-season (ep) (string-to-number (nth 2 ep)))
(defun episode-airdate   (ep)
  "Convert epguides.com time (like 14/Mar/79) to ISO date (1979-03-14).
Raise error-converting-date signal for any conversion error."
  (condition-case err
      (let* ((airdate-string (nth 4 ep))
             (time (parse-time-string airdate-string))
             (decoded-time (append (list 0 0 0) (subseq time 3 6)))
             (encoded-time (apply 'encode-time decoded-time)))
        (format-time-string "%Y-%m-%d" encoded-time))
    (error
     (signal 'error-converting-date (list (nth 4 ep) err)))))
(defun episode-airdate-or-nil (ep)
  (condition-case nil
      (episode-airdate ep)
    (error-converting-date nil)))
(defun episode-title     (ep) (nth 5 ep))
(defun episode-special-p (ep) (string-equal (nth 6 ep) "y"))

(defun episode-to-agenda (ep)
  (format "** TODO %-3d %2d-%02d   <%s -0700>  %s"
          (episode-number ep)
          (episode-season ep)
          (episode-in-season ep)
          (episode-airdate-or-nil ep)
          (episode-title ep)))

(defun convert-guide-to-org-agenda (guide)
  "Convert a list of episodes to a list of org-agenda lines."
  (mapcar 'episode-to-agenda guide))

(defun extract (content)
  (mapconcat 'identity
             (convert-guide-to-org-agenda
              (parse-guide
               (extract-csv-rows-from-epguide-html
                content)))
             "\n"))

(defun retrieve-agenda-from-epguide-show (show-number)
  (save-current-buffer
    (with-current-buffer
     (url-retrieve-synchronously
      (format "http://epguides.com/common/exportToCSV.asp?rage=%d"
              show-number))
     (extract (buffer-string)))))
