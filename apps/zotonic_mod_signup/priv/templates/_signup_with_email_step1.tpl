{% wire id="signup_form"
        type="submit"
        postback={signup_email_step1 page=page props=props signup_props=signup_props}
        delegate=`controller_signup`
%}
<form id="signup_form" method="post" action="postback">
    <div class="form-group">
        {% if props.email %}
            <p>
                <b>{{ props.email|escape }}</b>
                <a id="signup-go-step1-1" class="pull-right" href="{% url signup p=page %}" role="button">{_ Change _}</a>
                {% wire id="signup-go-step1-1"
                        type="click"
                        postback={signup_go_step1 page=page props=props signup_props=signup_props email=email}
                        delegate=`controller_signup`
                %}
            </p>
        {% else %}
            <input id="signup-email-input" class="form-control" autofocus type="email" name="email" autocomplete="email" placeholder="you@example.com" value="{{ email|escape }}" required>
            {% validate id="signup-email-input" name="email" type={email} %}
        {% endif %}
    </div>
    <div class="form-group">
        <button class="btn btn-primary" type="submit">{_ Next _}</button>
    </div>
    {% if not is_email_verified %}
        <p class="help-block">
            {_ We will mail you a code to confirm your email address. _}
        </p>
    {% else %}
        <p><br></p>
    {% endif %}
</form>
