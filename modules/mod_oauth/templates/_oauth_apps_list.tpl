        {% for app in m.oauth_app %}
        <li class="clearfix">
            <h2>{{ app.application_title }}</h2>
            <p>{{ app.application_descr }}</p>

            {% with m.oauth_perms.humanreadable[app.id] as readable %}
            {% if readable %}
            <p>This application wants access to the following services:</p>
            <ul>
                {% for perm in readable %}
                <li>{{ perm.desc }}</li>
                {% endfor %}
            </ul>
            {% endif %}
            {% endwith %}

            <dl class="clearfix">
                <dt>Consumer key</dt><dd>{{ app.consumer_key }}</dd>
                <dt>Consumer secret</dt><dd>{{ app.consumer_secret}}</dd>
            </dl>

            <p>
                {% button text="Edit" postback={start_edit_app id=app.id} %}
                {% button text="Show users" postback={start_tokens id=app.id} %}
                {% button text="Delete" postback={start_del_app id=app.id} %}
            </p>

        </li>
        {% endfor %}
