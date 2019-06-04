# Converter of the Google Contacts CSV export to Palm Pilot CSV

In order to compile one must have OSX, sbcl, quicklisp installed and configured.
Run 
```
make
```
to compile.
Usage: 

```
./palm-gcontacts google-contacts.csv output.csv
```
where google-contacts.csv - path to the exported Google Contacts in Google CSV format.
output.csv - file ready to be pilot-address'ed to Palm Pilot.

Note that it is one-way export, the data from Palm could not be imported back to Google Contacts (as some fields are lost)

The input encoding is UTF-8, the Palm CSV file is in CP1251.
The Swedish characters will be replaced with ASCII ones.
å a
ö o
ä a
and their corresponding capital versions.
