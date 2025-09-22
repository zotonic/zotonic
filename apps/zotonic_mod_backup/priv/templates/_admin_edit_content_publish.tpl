{% overrules %}

{% block more_actions %}
    {% if id.is_editable %}
        <li>
            <a href="{% url admin_backup_revision id=id %}" id="revert-rsc-action" title="{_ Revert to an earlier revision of this page. _}">
                <span class="glyphicon glyphicon-step-backward"></span> {_ Revert... _}
            </a>
        </li>
    {% endif %}
    {% inherit %}
{% endblock %}
