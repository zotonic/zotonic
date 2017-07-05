<div class="form-group">
	<label class="control-label" for="email">{_ E-mail address _}</label>
	<div>
		<input class="form-control" id="email" type="text" name="email" value="{{ id.email }}" />
		{% validate id="email" type={email} %}
	</div>
</div>
