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
                <a class="btn btn-default" href="/" data-onclick-topic="model/window/post/close">{_ Cancel _}</a>
                <button class="btn btn-primary" type="submit">{_ I want to create a new account _}</button>
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

                <a class="btn btn-default" href="/" data-onclick-topic="model/window/post/close">{_ Cancel _}</a>
                <button class="btn btn-primary" type="submit">{_ Continue _}</button>
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

                <p class="alert alert-warning">{% trans "Somebody else is already connected with this account on {service}." service=service|escape|default:_"the service" %}</p>
            </div>
        {% elseif qerror == "duplicate_email" %}
            <div class="container">
                <h1>{_ Duplicate email _}</h1>

                <p class="alert alert-warning">{% trans "There is already an account with your email address on {site}." site=m.site.title %}</p>
                <p>
                    {% trans "You can connect the account on {site} with your account on {service} if both accounts have a verified email address but this is not the case. Check both sites and then try again."
                                site=m.site.title
                                service=service|escape|default:_"the service"
                    %}
                </p>
            </div>
        {% elseif qerror == "multiple_email" %}
            <div class="container">
                <h1>{_ Multiple accounts _}</h1>

                <p class="alert alert-warning">{% trans "There is more than one account with your email address on {site}." site=m.site.title %}</p>
                <p>{% trans "We cannot connect you to the right account. Contact support of {site} to help you merge the accounts and then try again." site=m.site.title %}</p>
            </div>
        {% elseif qerror == "unexpected_user" %}
            <div class="container">
                <h1>{_ Expected another account _}</h1>

                <p class="alert alert-warning">
                    {% trans "You used another account on {service} than was expected."
                             service=service|escape|default:_"the service"
                    %}
                </p>
            </div>
        {% elseif qerror == "disabled_user" %}
            <div class="container">
                <h1>{_ Disabled account _}</h1>

                <p class="alert alert-warning">
                    {% trans "Your account on {site} is disabled."
                             site=m.site.title
                    %}
                </p>
            </div>
        {% else %}
            <div class="container">
                <h1>{_ Sorry _}</h1>

                <p class="alert alert-danger">{% trans "There was a problem authenticating with {service}." service=service|escape|default:_"the service" %}</p>

                <p>{_ Please try again later. _}</p>
            </div>
        {% endif %}

        <p style="text-align: center">
            <a class="btn btn-default" href="/" data-onclick-topic="model/window/post/close">{_ Cancel _}</a>
        </p>
    {% endif %}

{% endwith %}