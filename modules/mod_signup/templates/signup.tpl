{% extends "page.tpl" %}

{% block title %}{_ Sign Up _}{% endblock %}

{% block html_head_extra %}
	{% inherit %}
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
		.error_duplicate_identity #signup_error_duplicate_identity,
		.error_custom_1 #signup_error_custom_1,
		.error_custom_2 #signup_error_custom_2 {
			display: block;
		}

		.z_validation_message {        
			color: red;
		    padding: 0 0 0 9px;
		}
	</style>
{% endblock %}

{% block content %}
<section class="article-wrapper">

{% if m.acl.user %}
	{% wire action={redirect location=m.rsc[m.acl.user].page_url} %}
{% else %}

<div id="signup_area">

	<h1>{_ Sign up and become a member _}</h1>

	<p>{_ If you have already an account, _} <a href="{% url logon p=q.p %}">{_ log on now _}</a>.</p>

	{% if xs_props %}
	<p>{_ You can also _} <a href="{% url signup %}">{_ sign up for a username and password _}</a>.</p>
	{% else %}
	<ul id="signup_services" class="clearfix">
		{% all include "_signup_services.tpl" mask_target="signup_form" %}
	</ul>
	{% endif %}

	{% wire id="signup_form" type="submit" postback={signup xs_props=xs_props} %}
	<form id="signup_form" class="setcookie" method="post" action="postback">

		<h2>{_ Create your account _}</h2>

		<p id="signup_error_duplicate_identity" class="error">{_ Sorry, there is already an account coupled to your account at your service provider. Maybe your account here was suspended. _}</p>

		<div id="signup_name_first">
			<label for="name_first">{_ First name _}</label>
			{% if name_first %}
				<span>{{ name_first|escape }}</span>
			{% else %}
				<input id="name_first" name="name_first" type="text" value="{{ name_first|escape }}" />
				{% validate id="name_first" type={presence} %}
			{% endif %}
		</div>

		{# Enable this for nl, de and be sites #}
		{% if z_language == 'nl' or z_language == 'de' %}
		<div id="signup_surname_prefix">
			<label for="surprefix">{_ Prefix _}</label>
			<input id="surprefix" name="surprefix" type="text" value="" />
		</div>
		{% endif %}

		<div id="signup_name_surname">
			<label for="name_surname">{_ Last name _}</label>
			{% if name_surname %}
				<span>{{ name_surname|escape }}</span>
			{% else %}
				<input id="name_surname" name="name_surname" type="text" value="{{ name_surname|escape }}" />
				{% validate id="name_surname" type={presence} %}
			{% endif %}
		</div>

		<div id="signup_email">
			<label for="email">{_ E-mail _}</label>
			{% if email %}
				<span>{{ email|escape }}</span>
			{% else %}
				<input id="email" name="email" type="text" value="{{ email|escape }}" />
				{% validate id="email" type={email} type={presence} %}
			{% endif %}
		</div>

		<p class="clear"></p>

		{% if not xs_props %}
			<h3>{_ Choose a username and password _}</h3>

			<p id="signup_error_duplicate_username" class="error">{_ Sorry, this username is already in use. Please try another one. _}</p>

			<div id="signup_username">
				<label for="username">{_ Username _}</label>
				<input id="username" name="username" type="text" value="" />
				{% validate id="username" wait=400 type={presence} type={username_unique} %}
			</div>

			<div id="signup_password1">
				<label for="password1">{_ Password _}</label>
				<input id="password1" name="password1" type="password" value="" autocomplete="off" />
				{% validate id="password1" 
					type={presence} 
					type={length minimum=6 too_short_message="Too short, use 6 or more."} %}
			</div>

			<div id="signup_password2">
				<label for="password2">{_ Verify password _}</label>
				<input id="password2" name="password2" type="password" value="" autocomplete="off" />
				{% validate id="password2" 
				    type={presence} 
				    type={confirmation match="password1"} %}
			</div>

			<p class="clear"></p>
		{% endif %}

		{% all include "_signup_extra.tpl" %}
	
		<h3>{_ Check our Terms of Service and Privacy policies _}</h3>

		<div id="signup_tos">
			<p>{_ We will be very careful with all the information given to us and will never give your name or address away without your permission.
			We do have some rules that we need you to agree with. _}</p>
	
			<p id="signup_error_tos_agree" class="error">{_ To sign up you must agree with the Terms of Service and Privacy policies. _}</p>
	
			<input type="checkbox" name="signup_tos_agree" id="signup_tos_agree" value="1" />
			<label for="signup_tos_agree">{_ I agree to the _} <a target="_blank" href="{{ m.rsc.signup_tos.page_url }}">{{ m.rsc.signup_tos.title }}</a>
				{_ and the _} <a target="_blank" href="{{ m.rsc.signup_privacy.page_url }}">{_ Privacy policies _}</a>.</label>
			{% validate id="signup_tos_agree" type={acceptance} %}
		</div>

		<p class="clear"></p>

		<h2>{_ And sign up _}</h2>

		<div id="signup_button">
			<button>{_ Sign Up _}</button>
		</div>
	</form>

</div>

<div id="signup_verify">
	<h1><a name="signup_verify"></a>{_ You are almost there! _}</h1>
	
	<p>{_ We have sent you an e-mail. In the e-mail you will find instructions on how to confirm your account. _}</p>
	
	<p>{_ When you don’t receive the e-mail within a few minutes then be sure to check your spam filter and spam folders. _}</p>
</div>

{% endif %}

</section>

{% endblock %}
