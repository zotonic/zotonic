{% with error|default:q.error as qerror %}

    {% if qerror == "confirm" %}
        <div class="container">
            <h1>{_ You are about to create a new account. _}</h1>

            <p>{_ Close this window and log in if you are already a user of _} {{ m.site.title }}</p>

            {% wire id="signup_confirm"
                    type="submit"
                    postback={signup_confirm auth=what}
                    delegate=`mod_authentication`
            %}
            <form id="signup_confirm"
                  class="z_cookie_form"
                  data-onsubmit-topic="bridge/origin/model/authentication/post/service-confirm"
                  data-onsubmit-response-topic="model/auth/event/service-confirm"
                  action="postback"
            >
                <input type="hidden" name="auth" value="{{ q.auth|escape }}">
                <button class="btn btn-primary" type="submit">{_ I want to create a new account _}</button>
                <a class="btn btn-default" href="#" data-onclick-topic="model/window/post/close">{_ Close window _}</a>
            </form>

            <div class="padding alert alert-danger" style="display:none" id="signup_error">
                <p>
                    <b>{_ Something went wrong _}</b>
                    {_ Please try again later. _}
                </p>
            </div>
        </div>
    {% else %}
        {% if qerror == "email_required" %}
            <div class="container">
                <h1>{_ Sorry _}</h1>

                <p>{_ You have to share your email address to be able to sign in. _}</p>

                {% if auth_link %}
                    <p><a href="{{ auth_link }}">{_ Change your permissions _}</a></p>
                {% endif %}
            </div>
        {% elseif qerror == "duplicate" %}
            <div class="container">
                <h1>{_ Already connected _}</h1>

                <p class="alert alert-warning">{_ Somebody else is already connected with this account on _} {{ service|default:_"the service" }}</p>
            </div>
        {% elseif qerror == "duplicate_email" %}
            <div class="container">
                <h1>{_ Duplicate email _}</h1>

                <p class="alert alert-warning">{_ Somebody with your email address has already an account on _} {{ m.site.title }}</p>
            </div>
        {% else %}
            <div class="container">
                <h1>{_ Sorry _}</h1>

                <p class="alert alert-danger">{_ There was a problem authenticating with _} {{ service|default:_"the service" }}</p>

                <p>{_ Please try again later. _}</p>
            </div>
        {% endif %}

        <p style="text-align: center">
            <a class="btn btn-primary" href="#" data-onclick-topic="model/window/post/close">{_ Close window _}</a>
        </p>
    {% endif %}

{% endwith %}