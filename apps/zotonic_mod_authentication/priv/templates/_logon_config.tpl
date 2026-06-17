{#
Params:
style_boxed: 0 or 1; creates a background around the form
#}
{% with
    style_boxed|if_undefined:0
as
    style_boxed
%}
    {% include "_logon.tpl" %}
{% endwith %}
