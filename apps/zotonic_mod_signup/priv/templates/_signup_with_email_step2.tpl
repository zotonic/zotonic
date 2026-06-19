{# Step 2 of the signup.
 # Request the code from email.
 # Show status of email sending.
 # Option to resend a code.
 #}
<p class="clearfix">
    <b>{{ email|escape }}</b>
    <a id="signup-go-step1-2" class="pull-right" href="{% url signup p=page %}" role="button">{_ Change _}</a>
    {% wire id="signup-go-step1-2"
            type="click"
            postback={signup_go_step1 page=page props=props signup_props=signup_props email=email}
            delegate=`controller_signup`
    %}
</p>

{% if is_logon_link %}
    <div class="signup-logon-link">
        <p>
            {_ You already have an account. _}
        </p>
        <a href="{% url logon p=page u=email %}" class="btn btn-primary">{_ Log in _}</a>
    </div>
{% endif %}

{% if is_code_sent %}
    <div class="signup-code-sent">
        <h2>{_ Check your email _}</h2>
        <p>
            {% trans "We have sent a code to <b>{email}</b> to confirm your email address." email=email|escape %}
            {_ Please check your email and enter the code in the form below. _}
        </p>

        {% wire id="signup_form_step2"
                type="submit"
                postback={signup_email_step2 email=email page=page props=props signup_props=signup_props}
                delegate=`controller_signup`
        %}
        <form id="signup_form_step2" action="postback">
            <div class="form-group">
                <input id="signup-code-input" class="form-control" autofocus type="text" name="code" autocomplete="one-time-code" placeholder="{_ Enter code _}" required>
            </div>
            <p class="text-danger" style="display: none" id="signup-code-error">
                {_ The code you entered is incorrect. Please try again. _}
            </p>
            <p class="text-danger" style="display: none" id="signup-code-ratelimit">
                {_ Too many tries, you can try to signup again in an hour. _}
            </p>
            <div class="form-group">
                <button class="btn btn-primary" type="submit">{_ Next _}</button>
            </div>
        </form>
        {% wire action={focus target="signup-code-input"} %}

        <div class="help-block" id="signup-code-email-status">
            {% optional include "_email_send_status.tpl" message_nr=message_nr %}
        </div>

        <p class="help-block">
            {_ Didn’t receive the email? _}
            <a id="signup-resend-code" href="#" role="button">{_ Resend code _}</a>
            {% wire id="signup-resend-code"
                    type="click"
                    postback={signup_resend_code email=email}
                    delegate=`controller_signup`
            %}
        </p>
        <p class="help-block" id="signup-code-resent" style="display: none">
            <span class="glyphicon glyphicon-info-sign"></span>
            {_ A new verification code has been sent to your email address. Please check your inbox and spam folder. _}
        </p>
    </div>
{% endif %}

{% if user_external %}
    {% if is_code_sent %}
        <div class="text-muted z-logon-extra-separator"><span>{_ or _}</span></div>
    {% endif %}
    <div class="signup-external">
        <p class="help-block">
            {_ You can sign up using the following external service. _}
        </p>
        {% for ext in user_external %}
            {% if ext.template %}
                <div>
                    {% include ext.template ext=ext %}
                </div>
            {% elseif ext.url %}
                <p>
                    <a href="{{ ext.url|escape }}" class="btn btn-default">
                        <span class="fal fa-globe"></span>
                        {_ Log on with _} {{ ext.title|escape|default:_"external service" }}
                    </a>
                </p>
            {% endif %}
        {% endfor %}
    </div>
{% endif %}

{% if error and error != `exists` %}
    <div class="signup-error">
        {% if error == `email_send_failed` %}
            <p class="text-danger">
                {% trans "Could not send email to <b>{email}</b>. Please check the email address and try again."
                         email=email|escape
                %}
            </p>
        {% else %}
            <p class="text-danger">
                {_ Something went wrong, please retry again later. _}
            </p>
        {% endif %}
    </div>
{% endif %}
