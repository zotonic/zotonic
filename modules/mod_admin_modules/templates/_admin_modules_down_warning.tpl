{% if m.acl.is_allowed.use.mod_admin_modules %}
    {% if m.admin_status.modules.down as down %}
        <div class="alert alert-warning">
            <p>
                <b><span class="glyphicon glyphicon-warning-sign"></span> {_ The following modules are not running: _}<br></b>
                {% for mod in down %}
                    {{ mod|escape }}{% if not forloop.last %},{% endif %}
                {% endfor %}
            </p>
            <p>
                <a class="btn btn-primary" href="{% url admin_modules %}">{_ Modules _}</a>
            </p>
        </div>
    {% endif %}
{% endif %}
