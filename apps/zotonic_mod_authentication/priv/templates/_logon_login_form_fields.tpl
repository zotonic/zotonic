{# Form fields for normal logon action #}

{% if m.authentication.is_one_step_logon %}
    {# One step form. The field blocks are for backward compatibily with 0.x sites. #}
    {% block field_username %}
        <div class="form-group">
            <label for="username" class="control-label">{_ Email or username _}</label>
            <input class="form-control" type="text" id="username" name="username"
                   value="{{ q.options.username|default:q.username|escape }}"
                   {% if not is_show_passcode %}autofocus{% endif %}
                   required
                   inputmode="email"
                   placeholder="{_ Email or username _}"
                   autocomplete="username"
                   autocapitalize="off"
                   autocorrect="off">
        </div>
    {% endblock %}

    {% block field_password %}
        <div class="form-group">
            <label for="password" class="control-label">{_ Password _}</label>
            <input class="form-control" type="password" id="password" name="password" value=""
                   required
                   placeholder="{_ Password _}"
                   autocomplete="current-password"
                   autocapitalize="off"
                   autocorrect="off">
        </div>
    {% endblock %}

    {% if is_show_passcode %}
        {% block field_passcode %}
            <div class="form-group passcode">
                <label for="password" class="control-label">{_ Passcode _}</label>
                <input class="form-control" type="text" id="passcode" name="passcode" value=""
                       autofocus required inputmode="numeric" pattern="[0-9]+"
                       placeholder="{_ Two-factor passcode _}"
                       autocomplete="one-time-code"
                       autocapitalize="off"
                       autocorrect="off">
            </div>
        {% endblock %}
    {% endif %}

    {% if m.authentication.is_supported.rememberme %}
        <div class="form-group">
            <div class="checkbox">
                <label>
                    <input type="checkbox" name="rememberme" value="1" {% if q.rememberme or m.authentication.is_rememberme %}checked{% endif %}>
                    {_ Keep me logged in _}
                </label>
            </div>
        </div>
    {% endif %}

    <div class="form-group">
        <button class="btn btn-primary" type="submit">{_ Log in _}</button>
    </div>
{% else %}
    {% if q.options.is_username_checked %}
        <div class="form-group hidden">
            <label for="username" class="control-label">{_ Email or username _}</label>
            <input type="text"
                   id="username"
                   name="username"
                   value="{{ q.options.username|default:q.username|escape }}"
                   class="form-control"
                   required
                   inputmode="email"
                   autocapitalize="off"
                   autocorrect="off"
                   autocomplete="username"
                   tabindex="-1">
        </div>
        <p class="clearfix">
            <b>{{ q.options.username|default:q.username|escape }}</b>
            <a class="pull-right" href="{% url logon %}" data-onclick-topic="model/auth-ui/post/view/logon">{_ Change _}</a>
        </p>
    {% else %}
        <div class="form-group">
            <label for="username" class="control-label">{_ Email or username _}</label>
            <input type="text"
                   id="username"
                   name="username"
                   value="{{ q.options.username|default:q.username|escape }}"
                   class="form-control"
                   required
                   inputmode="email"
                   autofocus
                   autocapitalize="off"
                   autocorrect="off"
                   autocomplete="username">
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
            {% if ext.template %}
                <p class="clearfix">
                    {% include ext.template ext=ext %}
                </p>
            {% elseif ext.url %}
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
        <div class="form-group {% if is_show_passcode or is_set_passcode %}hidden{% endif %}">
            <label for="password" class="control-label">{_ Password _}</label>
            <input class="form-control" type="password" id="password" name="password" value="{{ q.password|escape }}"
                   required
                   {% if not is_show_passcode %}autofocus{% endif %}
                   autocomplete="current-password"
                   autocapitalize="off"
                   autocorrect="off"
                   placeholder="{_ Password _}">
        </div>

        {% if is_show_passcode %}
            <div class="form-group passcode">
                <label for="passcode" class="control-label">{_ Passcode _}</label>
                <input class="form-control" type="text" id="passcode" name="passcode" value=""
                       autofocus required inputmode="numeric" pattern="[0-9]+"
                       placeholder="{_ Two-factor passcode _}"
                       autocomplete="one-time-code"
                       autocapitalize="off"
                       autocorrect="off">
            </div>
        {% elseif is_set_passcode %}
            <div class="form-group set-passcode">
                {% include "_logon_login_set_passcode.tpl" %}
            </div>
        {% endif %}
    {% elseif not q.options.is_username_checked %}
        <div class="form-group hidden">
            <label for="password" class="control-label">{_ Password _}</label>
            <input class="form-control" type="password" id="password" name="password" value=""
                   placeholder="{_ Password _}"
                   autocomplete="current-password"
                   autocapitalize="off"
                   autocorrect="off">
        </div>
    {% endif %}

    {% if q.options.is_username_checked %}
        {% if is_user_local or is_user_local|is_undefined %}
            {% if m.authentication.is_supported.rememberme %}
                <div class="form-group">
                    <div class="checkbox">
                        <label title="{_ Stay logged on unless I log off. _}">
                            <input type="checkbox" name="rememberme" value="1"
                                {% if q.rememberme or m.authentication.is_rememberme %}checked{% endif %}>
                            {_ Keep me logged in _}
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
                    {_ Log in _}
                </button>
            </div>
        {% endif %}
    {% else %}
        <div class="form-group">
            <button class="btn btn-primary" type="submit">{_ Next _}</button>
        </div>
    {% endif %}

{% endif %}
