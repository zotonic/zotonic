{% with m.acl.user as user_id %}
{% if user_id or m.config.mod_comment.anonymous.value|default_if_undefined:1 %}
	{% wire id="comments-form" type="submit" postback={newcomment id=id} delegate="mod_comment" %}
	<form id="comments-form" method="post" action="postback">
		<fieldset class="zp-100">

			{% if not user_id %}
			<div class="zp-30">
				<div class="form-item">
					<label for="name">{_ Name _}</label>
					<input type="text" name="name" id="name" />
					{% validate id="name" type={presence} %}
				</div>
				<div class="form-item">
					<label for="mail">{_ E-mail _}</label>
					<input type="text" name="mail" id="mail" />
					{% validate id="mail" type={presence} type={email} %}
				</div>
			</div>
			{% endif %}

			<div class="zp-70 last">
				<div class="form-item">
					<label for="message">{_ Message _}</label>
					<textarea name="message" id="message" cols="60" rows="8"></textarea>
					{% validate id="message" type={presence} %}
				</div>
				<div class="form-item button-wrapper">
					<button type="submit">{_ Send _}</button>
				</div>
			</div>
		</fieldset>
	</form>
{% else %}
	<p id="comments-logon"><a href="{% url logon back %}">{_ Log on or sign up to comment _}</a>.</p>
{% endif %}
{% endwith %}
