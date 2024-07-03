{#
Params:
show_signup_username_title
show_signup_username
show_signup_password
show_signup_password2
#}
{% if not xs_props %}
    {% if show_signup_username_title %}
        <h3>{_ Choose a username and password _}</h3>
    {% endif %}

    {% if show_signup_username %}
        <div class="form-group" id="signup_username">
            <label for="username" class="control-label">{_ Username _}</label>
            <input class="form-control" id="username" name="username" type="text" value=""
                   required
                   autocapitalize="off"
                   autocorrect="off">
            {% validate id="username"
                wait=400
                type={presence failure_message=_"Enter a username"}
                type={username_unique}
            %}
        </div>
    {% endif %}

    {% if show_signup_password %}
        <div class="form-group" id="signup_password1">
            <label for="password1" class="control-label">{_ Password _}</label>
            <input class="form-control" id="password1" name="password1" type="password" value=""
                   required
                   minlength="{{ m.authentication.password_min_length }}"
                   autocomplete="new-password"
                   autocapitalize="off"
                   autocorrect="off">
            {% validate id="password1"
                type={presence failure_message=_"Enter a password"}
                type={acceptable_password
                    failure_message=_"Your new password is too short or not strong enough. Use a: uppercase letter, lowercase letter, number, and symbol."
                }
                only_on_blur
            %}
        </div>
    {% endif %}

    {% if show_signup_password2 %}
        <div class="form-group" id="signup_password2">
            <label for="password2" class="control-label">{_ Verify password _}</label>
            <input class="form-control" id="password2" name="password2" type="password" value=""
                   required
                   autocomplete="new-password"
                   autocapitalize="off"
                   autocorrect="off">
            {% validate id="password2"
                type={presence failure_message=_"Repeat your password"}
                type={confirmation match="password1" failure_message=_"This does not match the first password"}
                only_on_blur
            %}
        </div>
    {% endif %}

{% endif %}
