{#
Params:
logon_modal: load modal version of form
style_boxed: 0 or 1; creates a background around the form
#}
{% with
    style_boxed|if_undefined:0
as
    style_boxed
%}
    {% if logon_modal %}
        {% include "_logon_modal.tpl" %}
    {% else %}
        {% include "_logon.tpl" %}
    {% endif %}
{% endwith %}
