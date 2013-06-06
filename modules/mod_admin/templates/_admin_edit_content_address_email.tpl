<div class="control-group">
	<label class="control-label" for="email">{_ E-mail address _}</label>
	<div class="controls">
		<input id="email" type="text" name="email" value="{{ id.email }}" class="input-block-level" />
		{% validate id="email" type={email} %}
	</div>
</div>
