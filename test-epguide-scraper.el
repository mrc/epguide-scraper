(require 'ert)
(require 'ert-extras)

(defvar *fawlty-towers*
  "
<html>
<head>
<title>list output</title>
</head>
<body>
<pre>
number,season,episode,production code,airdate,title,special?
1,1,1,\"\",19/Sep/75,\"A Touch of Class\",n
2,1,2,\"\",26/Sep/75,\"The Builders\",n
3,1,3,\"\",03/Oct/75,\"The Wedding Party (a.k.a. Sex)\",n
4,1,4,\"\",10/Oct/75,\"The Hotel Inspectors\",n
5,1,5,\"\",17/Oct/75,\"Gourmet Night (a.k.a. Gourmet's Paradise)\",n
6,1,6,\"\",24/Oct/75,\"The Germans\",n
7,2,1,\"\",19/Feb/79,\"Communication Problems (a.k.a. Theft)\",n
8,2,2,\"\",26/Feb/79,\"The Psychiatrist\",n
9,2,3,\"\",05/Mar/79,\"Waldorf Salad\",n
10,2,4,\"\",12/Mar/79,\"The Kipper and the Corpse (a.k.a. Death)\",n
11,2,5,\"\",26/Mar/79,\"The Anniversary\",n
12,2,6,\"\",25/Oct/79,\"Basil the Rat (a.k.a. Rats)\",n
</pre>
</body>
</html>
")

(ert-deftest extract-csv-rows-from-epguide-html ()
  "Pull just the CSV rows from epguide csv results page."
  (let ((rows (extract-csv-rows-from-epguide-html *fawlty-towers*)))
    (should (= 13 (length rows)))))

(ert-deftest parse-guide-from-csv-rows ()
  "Parse the CSV rows into a list of episodes."
  (let* ((rows (extract-csv-rows-from-epguide-html *fawlty-towers*))
         (guide (parse-guide rows))
         (ep9 (nth 8 guide)))
    (are
     (= 12 (length guide))
     (= (episode-number ep9) 9)
     (= (episode-season ep9) 2)
     (= (episode-in-season ep9) 3)
     (string-equal (episode-airdate ep9) "1979-03-05")
     (equal (episode-airdate-as-decoded-time ep9 nil) (encode-time 0 0 0 5 3 1979))
     (string-equal (episode-title ep9) "Waldorf Salad")
     (not (episode-special-p ep9)))))

(ert-deftest convert-guide-to-org-agenda ()
  (let* ((rows (extract-csv-rows-from-epguide-html *fawlty-towers*))
         (guide (parse-guide rows))
         (agenda (convert-guide-to-org-agenda guide))
         (ep9 (nth 8 agenda)))
    (are
     (= 12 (length agenda))
     (string-equal ep9 "** TODO 9    2-03   <1979-03-05 -0700>  Waldorf Salad"))))

(ert-deftest cope-with-screwy-airdate ()
  "For any parse errors on the airdate, just use nil."
  (let ((screwy-airdate-row '("147" "8" "21" "7ACX10" "UNAIRED" "Partial Terms of Endearment" "n")))
    (should-error (episode-airdate screwy-airdate-row)
                  :type 'error-converting-date)
    (should (null (episode-airdate-or-nil screwy-airdate-row)))))

(ert-deftest identify-early-episodes ()
  (let* ((04-mar-79 (encode-time 0 0 0 4 3 1979))
         (05-mar-79 (encode-time 0 0 0 5 3 1979))
         (06-mar-79 (encode-time 0 0 0 6 3 1979))
         (ep-date 05-mar-79))
    (are
     (is-date-before ep-date 06-mar-79)
     (not (is-date-before ep-date 05-mar-79))
     (not (is-date-before ep-date 04-mar-79))
     (is-date-before '(4 4) '(5 5))
     (is-date-before '(4 6) '(5 5)))))

(ert-deftest reject-early-episodes ()
  (let* ((04-mar-79 (encode-time 0 0 0 4 3 1979))
         (epguide-reject-date 04-mar-79)
         (rows (extract-csv-rows-from-epguide-html *fawlty-towers*))
         (guide (parse-guide rows)))
    (are
     (= 4 (length guide))
     (equal '(9 10 11 12) (mapcar 'episode-number guide)))))
