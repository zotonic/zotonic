
.. include:: meta-info.rst

Returns a JSON object with information about the current user and the system::
               
  http://localhost:8000/api/base/info

Returns a JSON object like this::

  {
    "user": {
        "user_name": "Site Administrator",
        "user_id": 1
    },
    "site": {
        "zotonic_version": "0.9-dev",
        "language": "nl"
    }
  }
