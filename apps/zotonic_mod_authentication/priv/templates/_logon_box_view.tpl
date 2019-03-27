{#
Params:
- page
- error_reason
- style_boxed -- creates a colored background box
- form_title_tpl
- form_extra_tpl
- form_form_tpl
- form_support_tpl
- form_outside_tpl
#}
{% if form_title_tpl %}
    {% include form_title_tpl %}
{% endif %}

<div class="z-logon-form">
    {% if m.rsc.page_logon.body %}
        <div class="alert">{{ m.rsc.page_logon.body }}</div>
    {% endif %}

    {% if form_extra_tpl %}
        {% include form_extra_tpl %}
    {% endif %}

    {% if q.error and q.error != "unknown_code" %}
        <div id="logon_error" class="alert alert-danger">
            {% include "_logon_error.tpl" %}
        </div>
    {% endif %}

    {% if form_form_tpl %}
        {% include form_form_tpl
            page=page
            form_fields_tpl=form_fields_tpl
            style_boxed=style_boxed
        %}
    {% endif %}

    {% if form_support_tpl %}
        {% include form_support_tpl
            update_target=update_target
            update_template=update_template
            logon_state=logon_state
            style_boxed=style_boxed
        %}
    {% endif %}
</div>

{% if form_outside_tpl %}
    {% include form_outside_tpl
        update_target=update_target
        update_template=update_template
        logon_state=logon_state
        style_boxed=style_boxed
    %}
{% endif %}
