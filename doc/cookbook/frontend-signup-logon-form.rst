.. highlight:: erlang

Customizing the sign up and sign in form
========================================

You want to change parts of the form, or change its appearance.


Sign up form
------------

The sign up form is called with dispatch rule ``signup``, implemented by :ref:`mod_signup`. This module must be enabled to view the form.

The form is built from quite a number of sub templates. While this creates some level of complexity (to find out what goes where), this also provides the flexibility to change the resulting output without breaking the functional code.

For details, see "Template structure Sign up form" below.


Overriding templates
....................

Some sub templates are more critical than others.

While it is technically possible to override the sub templates, this will likely end up with non-working pages. The Page Controller expects to receive certain values from the submitted form.

These form fields are expected to have a value::

    email
    name_first
    name_surname
    username
    password1
    signup_tos_agree


If you want to hide a part of the form that contains a critical value, for instance the terms and conditions checkbox, you cannot just create an empty file ``_signup_form_fields_tos.tpl`` in your project, because the form value ``signup_tos_agree`` will no longer be passed.

Instead you need to pass the value through the form using a hidden input field::

    <input type="hidden" name="signup_tos_agree" value="1" />

Non-critical templates can be overridden safely. For instance ``_signup_title.tpl`` will only change the title above the form, so this can be done without affecting the functionality. You could decide to add a brand logo to the title, or remove the title altogether.


Changing the configuration
..........................

The template file ``_signup_config.tpl`` is made to quickly define what parts of the sign up page should be shown. Copy the file from ``mod_signup`` to your project and change the values.

For convenience, critical variables are not exposed in the config file.

By default, two form parts are set to hidden. Make them visible by changing the value from 0 to 1::

    show_signup_name_prefix         // surname prefix ("van" Gogh)
    show_signup_password2           // repeat password

Two other configuration values deal with appearance: ``style_boxed`` and ``style_width`` - see below.


Changing appearance
...................

Form fields are displayed using the Bootstrap 3 HTML structure. If you need a radical different layout that cannot be managed with CSS alone, you will need to override the form field templates.

The width of the form can be set with configuration value ``style_width``. This should be a CSS value like "300px". If no value is set, the form will expand to maximum width - in which case the width needs to be set by CSS.

A form background can be set with configuration value ``style_boxed``.

CSS styles are defined in ``css/logon.css`` (mod_authentication).


Password length
...............

The minimum password length is 8 characters, and can be changed in config value ``mod_authentication.password_min_length``.


Sign up form in a modal dialog
..............................

Instead of directing the user to the sign up page, the form can be shown in a modal dialog::

    <a id="{{ #signup }}" href="#">{_ Sign up _}</a>
    {% wire
        id=#signup
        action={
            dialog_open
            title="Sign up"
            template="signup.tpl"
        }
    %}

However, this does not work well if the user wants to switch to sign in: clicking the link will load the sign in page, removing the dialog.

A better approach is to use the sign in templates and pass ``logon_state``::

    <a id="{{ #signup }}" href="#">{_ Sign up _}</a>
    {% wire
        id=#signup
        action={
            dialog_open
            title="Sign up"
            template="logon_modal.tpl"
            logon_state="signup"
        }
    %}

If the user now clicks on the link to sign in, the new form is shown in the same dialog.


Sign in form
------------

For a general understanding of the templates and configuration, first read the section on Sign Up form above.

The sign in form is called with dispatch rule ``logon``, implemented by :ref:`mod_authentication`. This module will normally be enabled.

Sign In covers a number of associated functions:

* Sign in (both public and admin)
* Request password reset
* Feedback after reset
* Create new password
* Feedback when account needs verification

This makes the template structure more complex than the sign up form.


Overriding templates
....................

The templates are quite minimal and will probably not need structural changes. Most likely candidates for changing are titles and perhaps removing/adding extra links.

For details see "Template structure Sign Up form" below.


Changing the configuration
..........................

The two configuration values in template file ``_logon_config.tpl`` deal with appearance: ``style_boxed`` and ``style_width`` (see below). Copy the file from ``mod_authentication`` to your project and change the values.


Changing appearance
...................

Form fields are displayed using the Bootstrap 3 HTML structure. To change the layout, look at the form field templates:

* _logon_login_form_fields.tpl
* _logon_reminder_form_fields
* _logon_reset_form_fields

The width of the form can be set with configuration value ``style_width``. This should be a CSS value like "300px". If no value is set, the form will expand to maximum width - in which case the width needs to be set by CSS

A form background can be set with configuration value ``style_boxed``.

CSS styles are defined in ``css/logon.css`` (mod_authentication).


Reference: Template structure Sign up form
------------------------------------------

Template tree:

.. code-block:: none

    signup.tpl                                   // sign up page
    `-- _signup_config.tpl                       // template and field configuration
        `-- _signup.tpl                          // if signed in, redirects to user page
            `-- _signup_box.tpl                  // form box components
                `-- _signup_stage.tpl            // feedback message


The central form template ``_signup_box.tpl`` is further populated by sub templates:

.. code-block:: none

    _signup_box.tpl
    |-- _signup_title.tpl                        // header "Sign up"
    |-- _signup_extra.tpl                        // sign up with other auth modules (if activated)
    |-- _signup_form_form.tpl                    // HTML form
    |   `-- _signup_form_fields.tpl              // 3 form parts plus submit button
    |       |-- _signup_form_fields_email.tpl    // name and email
    |       |-- _signup_form_fields_username.tpl // username and password
    |       `-- _signup_form_fields_tos.tpl      // terms of service
    |-- _signup_support.tpl                      // left empty
    |-- _signup_outside.tpl                      // link (back) to sign in
    `-- _logon_link.tpl                          // shown below sign in form


Reference: Template structure Sign Up form
------------------------------------------

Template tree:

.. code-block:: none

    logon.tpl                                    // sign in page
    |-- _logon_config.tpl                        // template and field configuration
    |   `-- _logon.tpl or _logon_modal.tpl       // logon module
    |       `-- _logon_box.tpl                   // form box components
    |           `-- _logon_stage.tpl             // feedback messages
    `-- logoff.tpl                               // log off page, redirects to q.p or homepage


Two mechanisms handle the state to determine which sub templates should be read:

* For display on the page: the dispatch rule
* For display inside a modal: the ``logon_state`` value


Depending on the the state value, ``_logon_box.tpl`` is populated by different sub templates:

.. code-block:: none

    when logon_state is::
    |== logon_reminder:                          // request a pw reset
    |   `-- _logon_box.tpl
    |       |-- _logon_reminder_title.tpl
    |       |-- _logon_reminder_form.tpl
    |       |   `-- _logon_reminder_form_fields.tpl or _logon_reminder_admin_form_fields.tpl
    |       `-- _logon_reminder_support.tpl      // backlink to logon form
    |== logon_reset:                             // reset pw
    |   `-- _logon_box.tpl
    |       |-- _logon_reset_title.tpl
    |       |-- _logon_reset_form.tpl
    |       |   `-- _logon_reset_form_fields.tpl
    |       `-- _logon_reset_support.tpl         // backlink to logon form
    |== admin_logon:
    |   `-- _logon_box.tpl
    |       |-- _logon_error.tpl
    |       |-- _logon_login_form.tpl
    |       |   `-- logon_login_admin_form_fields.tpl
    |       |-- _logon_login_support.tpl         // link forgot password
    |== signup (logon_state/modal only)
    |   `-- _signup_config.tpl (see Sign Up form)
    |       |-- _signup_support.tpl              // backlink to logon form
    `== else:
        `-- _logon_box.tpl
            |== awaiting verification:
            |   `-- _logon_stage.tpl             // alternative content for logon box
            `== else:
                |-- _logon_login_title.tpl       // title "Sign in to ..."
                |-- _logon_login_extra.tpl       // all-include by other modules
                |-- _logon_error.tpl
                |-- _logon_login_form.tpl
                |   `-- _logon_login_form_fields.tpl
                |-- _logon_login_support.tpl     // link forgot password
                |-- _logon_login_outside.tpl     // all-include _logon_link.tpl
                   `-- _logon_link.tpl           // all-include by other modules

Here the sub templates ensure a consistent markup inside the box when going from state to state.
