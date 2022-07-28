{% with error|default:q.error as qerror %}

    {% if qerror == "confirm" %}
        <div class="container">
            <h1>{_ You are about to create a new account. _}</h1>

            {% if q.url %}
                <p>
                    {% trans 'If you are already a user of {site} then <a href="{logon_url}">log on</a>.'
                            site=m.site.title
                            logon_url=`logon`|url
                    %}
                </p>
            {% else %}
                <p>
                    {% trans 'Close this window and log on if you are already a user of {site}.'
                             site=m.site.title
                    %}
                </p>
            {% endif %}

            <form id="signup_confirm"
                  data-onsubmit-topic="bridge/origin/model/authentication/post/service-confirm"
                  data-onsubmit-response-topic="model/auth/event/service-confirm"
            >
                <input type="hidden" name="auth" value="{{ q.auth|escape }}">
                <input type="hidden" name="url" value="{{ q.url|escape }}">
                <button class="btn btn-primary" type="submit">{_ I want to create a new account _}</button>
                {% if q.url %}
                    <a class="btn btn-default" href="/">{_ Cancel _}</a>
                {% else %}
                    <a class="btn btn-default" href="#" data-onclick-topic="model/window/post/close">{_ Close window _}</a>
                {% endif %}
            </form>
        </div>
    {% elseif qerror == "need_passcode" or qerror == "passcode" %}
        <div class="container">
            <h1>{_ You are about to log on. _}</h1>

            {% if qerror == 'passcode' %}
                <div class="text-danger">
                    {% catinclude "logon_error/message.tpl" [qerror] hide_links %}
                </div>
            {% else %}
                <div>
                    {% catinclude "logon_error/message.tpl" [qerror] hide_links %}
                </div>
            {% endif %}

            <form id="signup_confirm"
                  data-onsubmit-topic="bridge/origin/model/authentication/post/service-confirm-passcode"
                  data-onsubmit-response-topic="model/auth/event/service-confirm"
            >
                <input type="hidden" name="authuser" value="{{ q.authuser|escape }}">
                <input type="hidden" name="url" value="{{ q.url|escape }}">

                {% block field_passcode %}
                    <div class="form-group passcode">
                        <label for="password" class="control-label">{_ Passcode _}</label>
                        <input class="form-control" type="text" id="passcode" name="passcode" value=""
                               style="max-width: 30ch"
                               autofocus required inputmode="numeric" pattern="[0-9]+"
                               placeholder="{_ Two-factor passcode _}"
                               autocomplete="one-time-code"
                               autocapitalize="off"
                               autocorrect="off">
                    </div>
                {% endblock %}

                <button class="btn btn-primary" type="submit">{_ Continue _}</button>
                {% if q.url %}
                    <a class="btn btn-default" href="/">{_ Cancel _}</a>
                {% else %}
                    <a class="btn btn-default" href="#" data-onclick-topic="model/window/post/close">{_ Close window _}</a>
                {% endif %}
            </form>
        </div>
    {% else %}
        {% if qerror == "email_required" %}
            <div class="container">
                <h1>{_ Sorry _}</h1>

                <p>{_ You have to share your email address to be able to log in. _}</p>

                {% if auth_link %}
                    <p><a href="{{ auth_link }}">{_ Change your permissions _}</a></p>
                {% endif %}
            </div>
        {% elseif qerror == "duplicate" %}
            <div class="container">
                <h1>{_ Already connected _}</h1>

                <p class="alert alert-warning">{_ Somebody else is already connected with this account on _} {{ service|escape|default:_"the service" }}</p>
            </div>
        {% elseif qerror == "duplicate_email" %}
            <div class="container">
                <h1>{_ Duplicate email _}</h1>

                <p class="alert alert-warning">{_ Somebody with your email address has already an account on _} {{ m.site.title }}</p>
            </div>
        {% else %}
            <div class="container">
                <h1>{_ Sorry _}</h1>

                <p class="alert alert-danger">{_ There was a problem authenticating with _} {{ service|escape|default:_"the service" }}</p>

                <p>{_ Please try again later. _}</p>
            </div>
        {% endif %}

        <p style="text-align: center">
            <a class="btn btn-primary" href="#" data-onclick-topic="model/window/post/close">{_ Close window _}</a>
        </p>
    {% endif %}

{% endwith %}