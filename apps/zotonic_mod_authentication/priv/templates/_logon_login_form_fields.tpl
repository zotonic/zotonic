{# Form fields for normal logon action #}

{% if m.authentication.is_one_step_logon %}
    {# One step form. The field blocks are for backward compatibily with 0.x sites. #}
    {% block field_username %}
        <div class="form-group">
            <label for="username" class="control-label">{_ Username _}</label>
            <input class="form-control" type="text" id="username" name="username"
                   value="{{ q.options.username|default:q.username|escape }}"
                   {% if not q.is_show_passcode %}autofocus{% endif %} required autocomplete="username"
                   placeholder="{_ Username _}" />
        </div>
    {% endblock %}

    {% block field_password %}
        <div class="form-group">
            <label for="password" class="control-label">{_ Password _}</label>
            <input class="form-control" type="password" id="password" name="password" value=""
                   required autocomplete="current-password"
                   placeholder="{_ Password _}" />
        </div>
    {% endblock %}

    {% if q.is_show_passcode %}
        {% block field_passcode %}
            <div class="form-group passcode">
                <label for="password" class="control-label">{_ Passcode _}</label>
                <input class="form-control" type="text" id="passcode" name="passcode" value=""
                       autofocus required autocomplete="one-time-code" inputmode="numeric" pattern="[0-9]+"
                       placeholder="{_ Two-factor passcode _}" />
            </div>
        {% endblock %}
    {% endif %}

    {% if m.authentication.is_supported.rememberme %}
        <div class="form-group">
            <div class="checkbox">
                <label>
                    <input type="checkbox" name="rememberme" value="1" />
                    {_ Keep me signed in _}
                </label>
            </div>
        </div>
    {% endif %}

    <div class="form-group">
        <button class="btn btn-primary" type="submit">{_ Sign in _}</button>
    </div>
{% else %}
    {% if q.options.is_username_checked %}
        <div class="form-group hidden">
            <label for="username" class="control-label">{_ Username or email _}</label>
            <input type="text"
                   id="username"
                   name="username"
                   value="{{ q.options.username|default:q.username|escape }}"
                   class="form-control"
                   autocapitalize="off"
                   required
                   autocomplete="off"
                   tabindex="-1"
            >
        </div>
        <p class="clearfix hidden">
            <b>{{ q.options.username|escape }}</b>
            <a class="pull-right" href="{% url logon %}" data-onclick-topic="model/auth-ui/post/view/logon">{_ Other username or email _}</a>
        </p>
    {% else %}
        <div class="form-group">
            <label for="username" class="control-label">{_ Username or email _}</label>
            <input type="text"
                   id="username"
                   name="username"
                   value="{{ q.options.username|default:q.username|escape }}"
                   class="form-control"
                   autofocus
                   autocapitalize="off"
                   required autocomplete="username"
            >
        </div>
    {% endif %}

    {#
        IF external user OR local user: show button with link and password field
        IF unknown: pretend local and show password field
    #}

    {% if q.options.is_user_external %}
        <p class="help-block">
            {_ You can log in using the following external service _}
        </p>
        {% for ext in q.options.user_external %}
            {% if ext.url %}
                <p class="clearfix">
                    <a href="{{ ext.url|escape }}" class="btn btn-default" style="display: block">
                        <span class="fal fa-globe"></span>
                        {_ Log on with _} {{ ext.title|escape|default:_"external service" }}
                    </a>
                </p>
            {% endif %}
        {% endfor %}

        {% if q.options.is_user_local %}
            <p class="help-block">
                {_ or you can enter the password that you have for _} {{ m.site.title }}.
            </p>
        {% endif %}
    {% endif %}

    {% if q.options.is_user_local %}
        <div class="form-group">
            <label for="password" class="control-label">{_ Password _}</label>
            <input class="form-control" type="password" id="password" name="password" value=""
                   required autocomplete="current-password"
                   {% if not q.is_show_passcode %}autofocus{% endif %}
                   placeholder="{_ Password _}"
            >
        </div>

        {% if q.is_show_passcode %}
            <div class="form-group passcode">
                <label for="password" class="control-label">{_ Passcode _}</label>
                <input class="form-control" type="text" id="passcode" name="passcode" value=""
                       autofocus required autocomplete="one-time-code" inputmode="numeric" pattern="[0-9]+"
                       placeholder="{_ Two-factor passcode _}" />
            </div>
        {% endif %}
    {% endif %}

    {% if q.options.is_username_checked %}
        {% if is_user_local or is_user_local|is_undefined %}
            {% if m.authentication.is_supported.rememberme %}
                <div class="form-group">
                    <div class="checkbox">
                        <label title="{_ Stay logged on unless I log off. _}">
                            <input type="checkbox" name="rememberme" value="1" {% if q.rememberme %}checked{% endif %}>
                            {_ Keep me signed in _}
                        </label>
                    </div>
                </div>
                {#
                    {% javascript %}
                        if (isStandalone) {
                            $('#logon_form_form input[name=rememberme]').prop('checked', true);
                        }
                    {% endjavascript %}
                #}
                </div>
            {% endif %}
            <div class="form-group">
                <button class="btn btn-success" style="margin-right: 0px" type="submit">
                    {_ Sign in _}
                </button>
            </div>
        {% endif %}
    {% else %}
        <div class="form-group">
            <button class="btn btn-primary" type="submit">{_ Next _}</button>
        </div>
    {% endif %}

{% endif %}
