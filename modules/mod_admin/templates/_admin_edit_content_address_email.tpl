<label class="control-label" for="email">{_ E-mail address _}</label>
<div class="controls">
	<input id="email" type="text" name="email" value="{{ id.email }}" class="span4" />
	{% validate id="email" type={email} %}
</div>
