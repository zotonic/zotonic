.. versionadded:: 0.8

Validate input date against a given date format.

A quick guide to validating a date in Zotonic::

   <input type="text" id="my_date" name="my_date" value="" />
   {% validate id="my_date" type={date separator="\" format="b"} %} 

This code validates when the user enters a date in the following format::

   yyyy\mm\dd

Arguments:

=========  =============================================  =======
Argument   Description                                    Example
=========  =============================================  =======
separator  Character used to separate date parts, 
           such as / - \. Defaults to "-".                separator="-"
format     Date format, big endian (starting with year), 
           little endian (starting with day) or middle 
           endian (starting with month). 
           Defaults to "l" (little).                      format="m"
=========  =============================================  =======


`Edit <https://github.com/zotonic/zotonic/edit/master/doc/ref/validators/doc-date.rst>`_
