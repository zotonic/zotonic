{% overrules %}

{% block access_control %}
<fieldset>
    <div class="form-group">
        <p>
        	{_ This page is a _}
        	<strong>{{ id.category_id.title }}</strong>
        	{_ in the group _}
        	<strong><a href="{% url admin_edit_rsc id=id.content_group_id %}">{{ id.content_group_id.title }}</a></strong>
        </p>

        {% if m.acl_rule.can_insert[id.content_group_id][id.category_id] %}
    	    <a href="#" id="{{ #changecg }}" class="btn btn-outline-secondary">{_ Change category and/or content group... _}</a>
    		{% wire id=#changecg
    				action={submit closest}
    				action={dialog_open title=_"Category &amp; Content group" template="_action_dialog_change_category.tpl" id=id}
    		%}
    	{% endif %}
    </div>

    <div class="form-group">
        <label class="control-label">{_ Show private properties to _}</label>
        <div>
            {% with id.privacy as privacy %}
            <select class="form-control" id="{{ #privacy }}" name="privacy">
                <option value="0">{_ People who can view this page _}</option>
                <option value="10" {% if privacy == 10 %}selected{% endif %}>{_ Members _}</option>
                {% if id.is_a.person or privacy == 20 %}
                    <option value="20" {% if privacy == 20 %}selected{% endif %}>{_ Members of same user group _}</option>
                {% endif %}
                <option value="30" {% if privacy == 30 %}selected{% endif %}>{_ Collaboration group members _}</option>
                <option value="40" {% if privacy == 40 %}selected{% endif %}>{_ Collaboration group managers _}</option>
                <option value="50" {% if privacy == 50 %}selected{% endif %}>{_ Private _}</option>
            </select>
            {% endwith %}
            <p class="help-block">
                {_ Private properties are email, phone, visiting address, and date range. _}
            </p>
        </div>
    </div>
</fieldset>
{% endblock %}
