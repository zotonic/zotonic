{#
    Override this template to provide extra Google Analytics parameters.
    See https://developers.google.com/analytics/devguides/collection/analyticsjs/field-reference
#}
{
    {% if m.acl.user %}
        userId: "{{ m.acl.user|escapejs }}" {# add comma if extra params are added #}
    {% endif %}
}
