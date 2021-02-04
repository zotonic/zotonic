<div class="form-group label-floating">
	<input class="form-control" id="email" type="text" name="email" value="{{ id.email }}" placeholder="{_ E-mail address _}">
	{% validate id="email" type={email} %}
    <label class="control-label" for="email">{_ E-mail address _}</label>
</div>
