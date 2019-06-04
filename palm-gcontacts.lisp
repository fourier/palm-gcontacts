;;;; palm-gcontacts.lisp

(in-package #:palm-gcontacts)

(defparameter *palm-to-google-mapping*
  ;; indexes in Google format:
  ;; 0: Name
  ;; 1: Given Name
  ;; 2: Additional Name
  ;; 3: Family Name
  ;; 4: Yomi Name
  ;; 5: Given Name Yomi
  ;; 6: Additional Name Yomi
  ;; 7: Family Name Yomi
  ;; 8: Name Prefix
  ;; 9: Name Suffix
  ;; 10: Initials
  ;; 11: Nickname
  ;; 12: Short Name
  ;; 13: Maiden Name
  ;; 14: Birthday
  ;; 15: Gender
  ;; 16: Location
  ;; 17: Billing Information
  ;; 18: Directory Server
  ;; 19: Mileage
  ;; 20: Occupation
  ;; 21: Hobby
  ;; 22: Sensitivity
  ;; 23: Priority
  ;; 24: Subject
  ;; 25: Notes
  ;; 26: Language
  ;; 27: Photo
  ;; 28: Group Membership
  ;; 29: E-mail 1 - Type
  ;; 30: E-mail 1 - Value
  ;; 31: E-mail 2 - Type
  ;; 32: E-mail 2 - Value
  ;; 33: Phone 1 - Type
  ;; 34: Phone 1 - Value
  ;; 35: Phone 2 - Type
  ;; 36: Phone 2 - Value
  ;; 37: Phone 3 - Type
  ;; 38: Phone 3 - Value
  ;; 39: Phone 4 - Type
  ;; 40: Phone 4 - Value
  ;; 41: Address 1 - Type
  ;; 42: Address 1 - Formatted
  ;; 43: Address 1 - Street
  ;; 44: Address 1 - City
  ;; 45: Address 1 - PO Box
  ;; 46: Address 1 - Region
  ;; 47: Address 1 - Postal Code
  ;; 48: Address 1 - Country
  ;; 49: Address 1 - Extended Address
  ;; 50: Organization 1 - Type
  ;; 51: Organization 1 - Name
  ;; 52: Organization 1 - Yomi Name
  ;; 53: Organization 1 - Title
  ;; 54: Organization 1 - Department
  ;; 55: Organization 1 - Symbol
  ;; 56: Organization 1 - Location
  ;; 57: Organization 1 - Job Description
  ;; 58: Website 1 - Type
  ;; 59: Website 1 - Value
  ;;
  ;; Indexes in Palm format:
  ;; 0: Last name
  ;; 1: First name
  ;; 2: Title
  ;; 3: Company
  ;; 4: Phone1
  ;; 5: Phone2
  ;; 6: Phone3
  ;; 7: Phone4
  ;; 8: Phone5
  ;; 9: Address
  ;; 10: City
  ;; 11: State
  ;; 12: Zip Code
  ;; 13: Country
  ;; 14: Custom 1
  ;; 15: Custom 2
  ;; 16: Custom 3
  ;; 17: Custom 4
  ;; 18: Note
  ;; 19: Private
  ;; 20: Category
  (alist-hash-table
   '((0 . 3)
     (1 . 1)
     (2 . 53)
     (3 . 51)
     (4 . 34)
     (5 . 36)
     (6 . 38)
     (7 . 40)
     (9 . 43)
     (10 . 44)
     (11 . 46)
     (12 . 47)
     (13 . 48)
     (8 . 30)
     (18 . 25)))
  "A hash table with the key being the index in
Palm csv file row, and value the index of corresponding
entry in Google Contacts csv row")
      

(defun de-swedify (string)
  (coerce
   (flet ((de-swedify-char (c)
            (let ((n (char-code c)))
              (cond ((or (= n 229) (= n 228)) #\a)
                    ((or (= n 196) (= n 197)) #\A)
                    ((= n 246) #\o)
                    ((= n 214) #\O)
                    (t c)))))
     (loop for char across string
           unless (= 13 (char-code char))
           collect (de-swedify-char char)))
   'string))


(defun read-file-deswedify (filename)
  "Read the contents of the file FILENAME encoded in UTF8,
getting rid of accents in Swedish characters on a way (Palm
doesn't support them anyway.
Return the byte array"
  (de-swedify
   (alexandria:read-file-into-string filename)))

(defun parse-google-csv (filename)
  (cdr (cl-csv:read-csv (read-file-deswedify filename))))
                
(defun parse-csv-header (filename)
  (car (cl-csv:read-csv (read-file-deswedify filename))))


(defun google-csv-to-palm (csv)
  (loop for record in csv
        collect
        (loop for i below 20
              for g-idx = (gethash i *palm-to-google-mapping*)
              if g-idx
                collect (elt record g-idx)
              else
                collect "")))

(defun export-palm-csv (palm filename)
  (let ((output-csv
          (with-output-to-string (s)
            (cl-csv:write-csv palm :stream s :always-quote t))))
    (write-byte-vector-into-file
     (babel:string-to-octets output-csv :encoding :cp1251 :errorp nil)
     filename
     :if-exists :supersede)
    (values)))


(defun convert (from to)
  (export-palm-csv (google-csv-to-palm (parse-google-csv from)) to))


(defun command-line ()
  (or 
   #+SBCL sb-ext:*posix-argv*  
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

    
(defun usage (appname)
  (format *standard-output* "Google Contacts CSV to Palm Pilot CSV converter.~%")
  (format *standard-output* (concatenate 'string  "Usage: " appname " google-contacts.csv output.csv~%"))
  #+SBCL (sb-ext:exit)
  #+LISPWORKS (lispworks:quit)
  )

(defun main ()
  (let ((cmdargs (command-line)))
    (if (< (length cmdargs) 3)
        (usage (car cmdargs))
        (convert (second cmdargs) (third cmdargs))))
  #+SBCL (sb-ext:exit)
  #+LISPWORKS (lispworks:quit))




