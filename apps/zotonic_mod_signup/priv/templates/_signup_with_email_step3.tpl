{# Step 3 of the signup.
 # Request the user's name, email address, username and password.
 #}
<p class="clearfix">
    <b>{{ email|escape }}</b>
    <a id="signup-go-step1-3" class="pull-right" href="{% url signup p=page %}" role="button">{_ Change _}</a>
    {% wire id="signup-go-step1-3"
            type="click"
            postback={signup_go_step1 page=page props=props signup_props=signup_props email=email}
            delegate=`controller_signup`
    %}
</p>

{% block signup_form_step3 %}
{% wire id="signup_form_step3"
        type="submit"
        postback={signup_email_step3 email=email page=page props=props signup_props=signup_props form_fields=signup_form_fields}
        delegate=`controller_signup`
%}
<form id="signup_form_step3" action="postback">

    <input type="hidden" name="email" value="{{ email|escape }}">

    {% block signup_fields %}

        {% block signup_fields_props %}
            <div class="signup-name">
                {% if show_signup_name_first|default_if_none:true %}
                    <div class="form-group" id="signup_name_first">
                        <label for="name_first" class="control-label">{_ First name _}</label>
                        <input class="form-control" id="name_first" name="name_first" type="text" value="{{ props.name_first|escape }}"
                               placeholder="{_ First name _}" autocomplete="given-name" required>
                        {% validate id="name_first"
                            type={presence failure_message=_"Enter first name"}
                            only_on_blur
                        %}
                        {% wire action={focus target="name_first"} %}
                    </div>
                {% endif %}

                {% if show_signup_name_prefix|default_if_none:true %}
                    <div class="form-group" id="signup_surname_prefix">
                        <label for="surprefix" class="control-label">{_ Prefix _}</label>
                        <input class="form-control" id="surprefix" name="surprefix" type="text" value="{{ props.name_surname_prefix|escape }}" autocomplete="additional-name">
                    </div>
                {% endif %}

                {% if show_signup_name_last|default_if_none:true %}
                    <div class="form-group" id="signup_name_surname">
                        <label for="name_surname" class="control-label">{_ Last name _}</label>
                        <input class="form-control" id="name_surname" name="name_surname" type="text" value="{{ props.name_surname|escape }}"
                               placeholder="{_ Last name _}" autocomplete="family-name" required>
                        {% validate id="name_surname"
                            type={presence failure_message=_"Enter last name"}
                            only_on_blur
                        %}
                    </div>
                {% endif %}
            </div>
        {% endblock %}

        {% if m.signup.config.username_equals_email %}
            <input type="hidden" name="username" value="{{ email|escape }}">
        {% else %}
            {% block signup_fields_username %}
                <div class="form-group" id="signup_username">
                    <label for="username" class="control-label">{_ Username _}</label>
                    <input class="form-control" id="username" name="username" type="text" value="{{ email|escape }}"
                           required placeholder="{_ Username _}" autocapitalize="off" autocorrect="off">
                    {% validate id="username"
                        wait=400
                        type={presence failure_message=_"Enter a username"}
                        type={username_unique}
                    %}
                </div>
                <div style="display: none" id="signup_error_username" class="signup-error">
                    <p class="text-danger">
                        {_ There is already an account with this username. _}
                    </p>
                </div>
            {% endblock %}
        {% endif %}

        {% block signup_fields_password %}
            <div class="form-group" id="signup_password">
                <label for="password" class="control-label">{_ Password _}</label>
                <input class="form-control" id="password" name="password" type="password" value=""
                       required autocomplete="new-password" autocapitalize="off" autocorrect="off"
                       minlength="{{ m.authentication.password_min_length }}">
                {% validate id="password" name="password"
                    type={presence failure_message=_"Enter a password"}
                    type={acceptable_password
                        failure_message=_"Your new password is too short or not strong enough. Use a: uppercase letter, lowercase letter, number, and symbol."
                    }
                    only_on_blur
                %}
            </div>
            <p class="help-block">
                {_ Need a new password? _}
                <a href="#new-password" id="new-password" role="button" title="{_ Generate a new and secure password. _}">
                    {_ Generate a secure password _}
                </a>
                <a href="#copy-password" id="copy-password" class="pull-right" style="display: none" role="button" title="{_ Copy the password to the clipboard. _}">
                    <span class="glyphicon glyphicon-copy"></span> {_ Copy _}
                </a>

                {% javascript %}
                    $('#new-password').click(function (e) {
                        e.preventDefault();
                        cotonic.broker.call("bridge/origin/model/identity/get/generate_password")
                            .then((msg) => {
                                const password = msg.payload.result;
                                $('#password').val(password).trigger('blur').effect('highlight');
                                $('#copy-password').fadeIn();
                            });
                    });

                    $('#copy-password').click(function (e) {
                        e.preventDefault();
                        const password = $('#password').val();
                        navigator.clipboard.writeText(password).then(() => {
                            $('#password').effect('highlight');
                            $('#copy-password').effect('highlight');
                        }).catch((err) => {
                        });
                    });
                {% endjavascript %}
            </p>
        {% endblock %}

        {% block signup_fields_tos %}
            <p class="help-block">
                {_ We will be very careful with all the information given to us and will never give your name or address away without your permission. We do have some rules that we need you to agree with. _}
            </p>

            <div class="form-group" id="signup_tos">
                <div id="signup_tos_agree_group">
                    <label class="checkbox" for="signup_tos_agree">
                        <input type="checkbox" name="signup_tos_agree" id="signup_tos_agree" value="1">
                        {_ I agree to the Terms of Service and the Privacy policies. _}
                    </label>
                    {% with m.rsc.signup_tos.page_url as tos_url %}
                    {% with m.rsc.signup_privacy.page_url as privacy_url %}
                        {% if tos_url or privacy_url %}
                            <p class="help-block">
                                {_ You can read them here: _}
                                {% if tos_url %}
                                    <a target="_blank" href="{{ tos_url }}">{_ Terms of Service _} <span class="fa fa-external-link"></span></a>
                                {% endif %}
                                {% if privacy_url %}
                                    <a target="_blank" href="{{ privacy_url }}">{_ Privacy policies _} <span class="fa fa-external-link"></span></a>
                                {% endif %}
                            </p>
                        {% endif %}
                    {% endwith %}
                    {% endwith %}
                    {% validate id="signup_tos_agree"
                        type={acceptance failure_message=_"You must agree to the Terms in order to sign up."}
                        message_after="signup_tos_agree_group"
                    %}
                </div>
            </div>

            <div style="display: none" id="signup_error_tos_agree" class="signup-error">
                <p class="text-danger">
                    {_ To sign up you must agree with the Terms of Service and Privacy policies. _}
                </p>
            </div>
        {% endblock %}

    {% endblock %}

    <div class="form-group">
        <button class="btn btn-primary" type="submit">{_ Sign Up _}</button>
    </div>

    {% block signup_errors %}
        {% if m.signup.config.username_equals_email %}
            <div style="display: none" id="signup_error_username" class="signup-error">
                <p class="text-danger">
                    {_ There is already an account connected to your email address. _}
                </p>
                <a href="{% url logon p=page u=email %}" class="btn btn-primary">{_ Log in _}</a>
            </div>
        {% endif %}

        <div style="display: none" id="signup_error_email" class="signup-error">
            <p class="text-danger">
                {_ There is already an account connected to your email address. _}
            </p>
            <a href="{% url logon p=page u=email %}" class="btn btn-primary">{_ Log in _}</a>
        </div>

        <div style="display: none" id="signup_error_generic" class="signup-error">
            <p class="text-danger">
                {_ Something went wrong. Please try again later. _}
            </p>
        </div>

        <div style="display: none" id="signup_error_duplicate_identity" class="signup-error">
            <p class="text-danger">
                {_ Sorry, you already have an account. Maybe your account here was suspended. _}
            </p>
        </div>
    {% endblock %}

</form>
{% endblock %}

