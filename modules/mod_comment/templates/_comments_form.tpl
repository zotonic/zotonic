{% with m.acl.user as user_id %}
{% if user_id or m.config.mod_comment.anonymous.value|default_if_undefined:1 %}
<h2>{_ Leave a comment _}</h2>
{% wire id="comments-form" type="submit" postback={newcomment id=id} delegate="mod_comment" %}
<form id="comments-form" method="post" action="postback" class="form-horizontal">
    <fieldset>

	{% if not user_id %}
        <div class="control-group">
	    <label class="control-label" for="name">{_ Name _}</label>
            <div class="controls">
		<input type="text" name="name" id="name" class="span4" />
		{% validate id="name" type={presence} %}
	    </div>
        </div>
        
        <div class="control-group">
	    <label class="control-label" for="mail">{_ E-mail _}</label>
            <div class="controls">
		<input type="text" name="mail" id="mail" class="span4" />
		{% validate id="mail" type={presence} type={email} %}
	    </div>
	</div>
	{% endif %}

        <div class="control-group">
	    <label class="control-label" for="message">{_ Message _}</label>
	    <div class="controls">
		<textarea name="message" id="message" cols="60" rows="8" class="span4"></textarea>
		{% validate id="message" type={presence} %}
	    </div>
        </div>
        
        <div class="control-group">
	    <div class="controls">
		<button class="btn btn-primary" type="submit">{_ Send _}</button>
	    </div>
	</div>
        
    </fieldset>
</form>
{% else %}
<p id="comments-logon"><a href="{% url logon back %}">{_ Log on or sign up to comment _}</a>.</p>
{% endif %}
{% endwith %}
