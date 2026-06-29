{% if m.acl.user as user_id %}
    <p>
        {% trans 'You are already logged in as <a href="{url}">{name}</a>.'
                 url=user_id.page_url
                 name="_name.tpl"|render:%{ id: user_id }
        %}
        <a href="{% url logoff p=page %}">{_ Log off _}</a>
    </p>
{% else %}
    <div id="signup_logon_box" class="signup z-logon-box{% if style_boxed %} z-logon-box-boxed{% endif %}" {% if style_width %}style="width: {{ style_width }};"{% endif %}>

        {% block signup_title %}
            <h1 class="z-logon-title">{_ Sign Up _}</h1>
        {% endblock %}

        {% block signup_logon %}
            <div id="signup-logon">
                {% trans 'If you already have an account, <a href="{url}" id="back_to_logon">log in now</a>.'
                         url={logon p=page}|url
                %}
            </div>
        {% endblock %}

        <!-- Show signup with email -->
        {% include "_signup_with_email.tpl" %}

        <!-- Show all signup SSO buttons -->
        <div id="signup-services">
            <ul class="z-logon-external">
                <li class="text-muted z-logon-extra-separator -first"><span>{_ or _}</span></li>
                {% all include "_logon_extra.tpl" p=page is_signup %}
            </ul>
        </div>
    </div>
{% endif %}
