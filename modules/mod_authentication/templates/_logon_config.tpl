{#
Params:
logon_modal: load modal version of form
logon_context: used to distinguish user login from admin login; values: 'admin_logon' or empty
style_boxed: 0 or 1; creates a background around the form
style_width: CSS style value, f.i. "300px"
#}
{% with
    style_boxed|if_undefined:0,
    style_width|if_undefined:"300px"
as
    style_boxed,
    style_width
%}
    {% if logon_modal %}
        {% include "_logon_modal.tpl" %}
    {% else %}
        {% include "_logon.tpl" %}
    {% endif %}
{% endwith %}
