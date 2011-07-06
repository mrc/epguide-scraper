(require 'ert)

(defvar *fawlty-towers*
"
<html>
<head>
<title>list output</title>
</head>
<body onload=\"list.select();\">
<textarea id=\"list\" rows=\"30\" cols=\"70\">
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
</textarea>
</body>
</html>
")

(defmacro are (test &rest tests)
  "Evaluate a list of tests with should"
  `(progn
     (should ,test)
     (unless (null ',tests)
       (are ,@tests))))

(ert-deftest extract-csv-rows-from-epguide-html ()
  "Pull just the CSV rows from epguide csv results page."
  (let ((rows (extract-csv-rows-from-epguide-html *fawlty-towers*)))
    (should (= 13 (length rows)))))

(ert-deftest parse-guide-from-csv-rows ()
  "Parse the CSV rows into a list of episodes."
  (let* ((rows (extract-csv-rows-from-epguide-html *fawlty-towers*))
         (guide (parse-guide rows))
         (ep9 (nth 8 guide)))
    (should (= 12 (length guide)))
    (should (string-equal (episode-number ep9) "9"))
    (should (string-equal (episode-season ep9) "2"))
    (should (string-equal (episode-in-series ep9) "3"))
    (should (string-equal (episode-airdate ep9) "1979-03-05"))
    (should (string-equal (episode-title ep9) "Waldorf Salad"))
    (should (not (episode-special-p ep9)))))
