{#
Loads the template according to the dispatch:
- logon
- logon/reminder
- logon/reset

Params:
style_boxed: creates a background around the form

#}

{% javascript %}
    {% if page == '#reload' %}
        sessionStorage.setItem('logonFormPage', window.location.href);
    {% else %}
        sessionStorage.setItem('logonFormPage', '{{ page|escapejs }}');
    {% endif %}
{% endjavascript %}

<div id="signup_logon_box" class="z-logon-box{% if style_boxed %} z-logon-box-boxed{% endif %}">
{#
    Here the zotonic.auth-ui.worker.js loads the "_logon_box.tpl" with the correct
    logon_view argument (one of: "reminder", "reset", "logon", "confirm")
#}
</div>

{% worker name="auth-ui" src="js/zotonic.auth-ui.worker.js" %}

