;;;; palm-gcontacts.asd

(asdf:defsystem #:palm-gcontacts
  :description "Converter Google Contacts CSV file to Palm Pilot CSV"
  :author "Alexey Veretennikov <EMAIL-REDACTED>"
  :licence "GPL"
  :depends-on (#:alexandria
               #:split-sequence
               #:cl-csv
               #:babel)
  :serial t
  :components ((:file "package")
               (:file "palm-gcontacts")))

