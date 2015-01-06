{#
Params:
page
error_reason

logon_form_title_tpl
    default: _logon_login_title.tpl

logon_form_extra_tpl
    default: _logon_login_extra.tpl
    
logon_form_entry_tpl
    default: _logon_login_form.tpl
    
logon_form_support_tpl
    default: _logon_login_support.tpl

logon_form_outside_tpl
    default: _logon_login_outside.tpl
    
#}
<div id="logon_box" class="z-logon-box">

    {% if logon_form_title_tpl %}
        {% include logon_form_title_tpl %}
    {% endif %}
    
    <div id="logon_error" class="z-logon-error-message">
        {% include "_logon_error.tpl" reason=error_reason %}
    </div>
    
    <div class="z-logon-form">
        {% if logon_form_extra_tpl %}
            {% include logon_form_extra_tpl %}
        {% endif %}
    
        {% if logon_form_entry_tpl %}    
            {% include logon_form_entry_tpl %}
        {% endif %}
    
        {% if logon_form_support_tpl %}
            <div class="z-logon-support">{% include logon_form_support_tpl %}</div>
        {% endif %}
    </div>

    {% if logon_form_outside_tpl %}
        {% include logon_form_outside_tpl %}
    {% endif %}
    
</div>
{# Use a real post for all forms on this page, and not AJAX or Websockets. This will enforce all cookies to be set correctly. #}
{% javascript %}
z_only_post_forms = true;
{% endjavascript %}

{# Set a listener on the session changes - needed for logon via external auth methods or via other pages #}
{% javascript %}
    z_transport_delegate_register('reload', function(_status) {
        $('body').mask();
        z_transport("controller_logon", "ubf", { msg: "logon_redirect", page: '{{ q.p|escapejs }}' });
    });
{% endjavascript %}
