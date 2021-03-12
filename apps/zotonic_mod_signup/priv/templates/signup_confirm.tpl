{% extends "base.tpl" %}

{% block title %}{_ Confirm my account _}{% endblock %}

{% block content %}

	<h1>{_ Confirm my account _}</h1>

	<p>{_ In your e-mail you received a confirmation key. Please copy it in the input field below. _}</p>

	<p id="confirm_error" class="error" {% if not error %}style="display: none"{% endif %}>
		{_ Sorry, I don't know that confirmation code. Did you copy it correctly? _}
	</p>

	{% wire id="signup_confirm_form"
			type="submit"
			postback={confirm}
			delegate=`controller_signup_confirm`
	%}
	<form class="form-inline" id="signup_confirm_form" method="post" action="postback">
		<input class="form-control" type="text" id="key" name="key" placeholder="{_ Confirm key _}" value="{{ q.key|escape }}" />
		<button class="btn btn-primary">{_ Confirm my account _}</button>
	</form>

    {% if q.key %}
        {% wire
                action={mask target="signup_confirm_form"}
                action={postback
                    postback={confirm key=q.key}
                    delegate=`controller_signup_confirm`
                }
        %}
    {% endif %}

{% endblock %}
