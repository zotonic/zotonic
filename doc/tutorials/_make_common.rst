2. Type ``make`` in the root of Zotonic (there where the Makefile is located).

3. When the build is done, start Zotonic in debug mode::

     bin/zotonic debug

4. You see Zotonic starting up, lots of messages passing by.

5. Now point your browser to::
	
     http://localhost:8000/
	
6. You now should see a welcome message, "Powered by Zotonic". The
   site you are looking at is called ``zotonic_status``, which is the
   default site for when there are no "real" sites configured
   yet. **note**: don't bother to log in yet on this screen; that will
   come later (if you're interested, see :ref:`tutorial-zotonic_status`)

7. Stop Zotonic, by pressing ctrl-c twice in the terminal.

8. OK! Now continue with :ref:`tutorial-install-addsite`.
