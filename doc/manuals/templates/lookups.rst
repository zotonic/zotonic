.. _manual-lookup-system:

Template locations and the lookup system
=========================================

All templates are located in the :file:`templates` directory of modules.
Templates are referred to by their full filename. When a template is inside a directory then the full path of the template must be given. 

For example, say we have two templates:

  | mod_example/templates/foobar.tpl
  | mod_example/templates/email/email_base.tpl

The above are referred to as ``foobar.tpl`` and ``email/email_base.tpl``.
Just ``email_base.tpl`` will not find the email template.

All templates of all modules are grouped together, regardless in which module they are defined and the module name is never given as part of the template name.


Module priority and overriding templates
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Similar named templates can be defined in multiple modules.
Which template is selected depends on the priority of the module.

The :dfn:`module priority` is a number defined in the module’s code and is usually a number between 1 and 1000.
A lower number gives a higher priority.
Templates in a module with higher priority hide templates in a module with lower priority.

When two modules have the same priority then the modules are sorted by their name.
That means that, given the same priority number, ``mod_aloha`` has higher priority than ``mod_hello``.

This mechanism allows any module (or site) to replace every single template in the system with its own version.

.. note:: Including all similar named templates

    | The tag ``{% all include "foobar.tpl" %}`` will include all templates named :file:`foobar.tpl`.
    | Where the tag ``{% include "foobar.tpl" %}`` only includes the highest priority :file:`foobar.tpl`.
    
    See also :ref:`tag-all-include` and :ref:`tag-all-catinclude`.


Module priority and overriding lib files
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Exactly the same module priority is also valid for all files in the :file:`lib` directory of modules.

This allows any module to change the static css, javascript, images, favicon.ico, robots.txt and other static files with its own version.


.. _manual-lookup-system-ua:

User Agent selection
^^^^^^^^^^^^^^^^^^^^

The module priority is a very powerful mechanism for extending and adapting Zotonic.

But what if a page requested with a mobile phone should be served with a different template than the same page requested with a desktop computer?

For this there is another template selection mechanism, based on the categorization of the device requesting the page.

User agent classes
------------------

Every request Zotonic classifies the device using the *User-Agent* request header. The possible classifications are:

 text
   Screen readers, feature phones, text only browsers.

 phone
   Smart phones, capable of javascript and having a touch interface or other pointing device.

 tablet
   Big screen, javascript, modern browser and touch interface.

 desktop
   Big screen, javascript, modern browser and pointing device.

The selected class is available in ``m.req.ua_class`` or from Erlang ``z_user_agent:get_class/1``.

.. note:: More properties can be found using ``m.req.ua_props`` or ``z_user_agent:get_props/1``.

The four user agent classes map to subdirectories of the :file:`templates` directory:

  | mod_example/templates/desktop/...
  | mod_example/templates/phone/...
  | mod_example/templates/tablet/...
  | mod_example/templates/text/...

All templates that are not in those sub-directories are categorized as *generic*.


Lookup by user agent class
--------------------------

The template system follows a strict hierarchy between the different user agent classes:

	desktop → tablet → phone → text → generic

Where the system starts looking from the current user agent class to the right.
So for a phone, the templates in the :file:`tablet` and :file:`desktop` directories will never be considered.


Combination of user agent and module priority
---------------------------------------------

The user agent class and the module priority are two dimensions of the template selection process.

The module priority is more important than the user agent class.

A mismatch in user agent class (e.g. a desktop template when looking for a phone version) will never be selected.
A sub-optimal version (e.g. a generic or text version instead of phone version) will be selected if that sub-optimal version resides in a module with higher priority than the module with the better matching version.

The *all include* tag will select the best version from all modules. Again skipping any user agent mismatches.


.. note:: Building templates and mobile first.

    The lookup strategy for templates conforms to a *mobile first* strategy.
    When adding a page or building a site, the idea is to start with the simplest, text only, version of the site.
    The text only version is then placed in the :file:`templates/text` directory. 
    Next will be adding more features, markup and interaction for the phone version. 
    Only then moving up to the big screen for tablet (touch) or desktop (mouse).


.. note:: Seeing which template is selected.

    The `mod_development` implements a screen where it is possible to see in real time which templates are included and
    compiled. The full path of all templates can be seen, giving insight in the template selection process.

    See also :ref:`mod_development`

