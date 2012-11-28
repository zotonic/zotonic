
.. include:: meta-mod_import_csv.rst

Module which adds "import CSV" button to the admin status screen.

The dropbox folder of the site is also watched for CSV files.

To determine whether it can import a file, it uses a notification::

  #import_csv_definition{basename, filename}

If the notification is not returning anything, it tries to map the
columns found in the first row of the CSV file to :term:`resource`
column names.

.. todo:: Add more documentation
