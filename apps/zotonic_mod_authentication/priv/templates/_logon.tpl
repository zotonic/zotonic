{#
Loads the template according to the dispatch:
- logon
- logon/reminder
- logon/reset

Params:
style_boxed: creates a background around the form

#}

<div id="signup_logon_box" class="z-logon-box{% if style_boxed %} z-logon-box-boxed{% endif %}">
{#
    Here the zotonic.auth-ui.worker.js loads the "_logon_box.tpl" with the correct
    logon_view argument (one of: "reminder", "reset", "logon")
#}
</div>

{% worker name="auth-ui" src="js/zotonic.auth-ui.worker.js" %}

