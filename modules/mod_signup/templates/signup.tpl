{% extends "page.tpl" %}

{% block title %}Sign up{% endblock %}

{% block content %}

<style type="text/css">
div#signup_verify {
	display: none;
}

ul#signup_services li {
	list-style-type: none;
}

input#surprefix {
	width: 50px;
}

div#signup_tos_text {
	height: 100px;
	overflow-y: scroll;
	margin-bottom: 10px;
	padding: 4px;
	border: 1px solid #ccc;
}

div#signup_tos label {
	float: none;
	display: inline;
}

#signup_error_tos_agree,
#signup_error_duplicate_username,
#signup_error_duplicate_identity {
	display: none;
}

.error_tos_agree #signup_error_tos_agree,
.error_duplicate_username #signup_error_duplicate_username,
.error_duplicate_identity #signup_error_duplicate_identity {
	display: block;
}

.z_validation_message {        
	color: red;
    padding: 0 0 0 9px;
}
</style>

{% if m.acl.user %}
<h1>You are already a member</h1>

<p>When you want to sign up a new member, then first <a href="{% url logoff %}">log off from your current account</a>.</p>

{% else %}

<div id="signup_area">

	<h1>Sign up and become a member</h1>

	<p>If you have already an account, <a href="{% url logon p=q.p %}">log on now</a>.</p>

	{% if xs_props %}
	<p>You can also <a href="{% url signup %}">sign up for a username and password</a>.</p>
	{% else %}
	<ul id="signup_services" class="clearfix">
		{% all include "_signup_services.tpl" mask_target="signup_form" %}
	</ul>
	{% endif %}

	{% wire id="signup_form" type="submit" postback={signup xs_props=xs_props} %}
	<form id="signup_form" method="post" action="postback">

		<h2>Create your account</h2>

		<p id="signup_error_duplicate_identity" class="error">Sorry, there is already an account coupled to your account at your service provider. Maybe your account here was suspended.</p>

		<div id="signup_name_first">
			<label for="name_first">First name</label>
			{% if name_first %}
				<span>{{ name_first|escape }}</span>
			{% else %}
				<input id="name_first" name="name_first" type="text" value="{{ name_first|escape }}" />
				{% validate id="name_first" type={presence} %}
			{% endif %}
		</div>

	{# Enable this for nl, de and be sites
		<div id="signup_surname_prefix">
			<label for="surprefix">Prefix</label>
			<input id="surprefix" name="surprefix" type="text" value="" />
		</div>
	#}

		<div id="signup_name_surname">
			<label for="name_surname">Last name</label>
			{% if email %}
				<span>{{ name_surname|escape }}</span>
			{% else %}
				<input id="name_surname" name="name_surname" type="text" value="{{ name_surname|escape }}" />
				{% validate id="name_surname" type={presence} %}
			{% endif %}
		</div>

		<div id="signup_email">
			<label for="email">E-mail</label>
			{% if email %}
				<span>{{ email|escape }}</span>
			{% else %}
				<input id="email" name="email" type="text" value="{{ email|escape }}" />
				{% validate id="email" type={email} type={presence} %}
			{% endif %}
		</div>

		<p class="clear"></p>

		{% if not xs_props %}
			<h3>Choose a username and password</h3>

			<p id="signup_error_duplicate_username" class="error">Sorry, this username is already in use. Please try another one.</p>

			<div id="signup_username">
				<label for="username">Username</label>
				<input id="username" name="username" type="text" value="" />
				{% validate id="username" type={presence} %}
			</div>

			<div id="signup_password1">
				<label for="password1">Password</label>
				<input id="password1" name="password1" type="password" value="" />
				{% validate id="password1" 
					type={presence} 
					type={length minimum=6 too_short_message="Too short, use 6 or more."} 
					type={confirmation match="password2"} %}
			</div>

			<div id="signup_password2">
				<label for="password2">Verify password</label>
				<input id="password2" name="password2" type="password" value="" />
				{% validate id="password2" type={presence} %}
			</div>

			<p class="clear"></p>
		{% endif %}
	
		<h3>Check our Terms of Service and Privacy policies</h3>

		<div id="signup_tos">
			<p>We will be very careful with all the information given to us and will never give your name or address away without your permission.
			We do have some rules that we need you to agree with.</p>
	
			<div id="signup_tos_text">
				<h3>{{ m.rsc.signup_tos.title }}</h3>
				<p class="summary">{{ m.rsc.signup_tos.summary }}</p>
				{{ m.rsc.signup_tos.body }}
			</div>

			<p id="signup_error_tos_agree" class="error">To sign up you must agree with the Terms of Service and Privacy policies.</p>
	
			<input type="checkbox" name="signup_tos_agree" id="signup_tos_agree" value="1" />
			<label for="signup_tos_agree">I agree to the <a target="_blank" href="{{ m.rsc.signup_tos.page_url }}">{{ m.rsc.signup_tos.title }}</a>
				 and the <a target="_blank" href="{{ m.rsc.signup_privacy.page_url }}">Privacy policies</a>.</label>
			{% validate id="signup_tos_agree" type={acceptance} %}
		</div>

		<p class="clear"></p>

		<h2>And sign up</h2>

		<div id="signup_button">
			<button>Sign Up</button>
		</div>
	</form>

</div>

<div id="signup_verify">
	<h1><a name="signup_verify"></a>You are almost there!</h1>
	
	<p>We have sent you an e-mail. In the e-mail you will find instructions on how to confirm your account.</p>
	
	<p>When you don’t receive the e-mail within a few minutes then be sure to check your spam filter and spam folders.</p>
</div>

{% endif %}

{% endblock %}
