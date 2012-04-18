<form id="logon_form" method="post" action="postback">
    {% if not hide_title %}
    <h1 class="logon_header">{_ Log on to _} <span>{{ m.config.site.title.value|default:"Zotonic" }}</span>.</h1>
    {% endif %}
    
    <input type="hidden" name="page" value="{{ page|escape }}" />
    <input type="hidden" name="handler" value="username" />

    <div class="control-group">
        <label for="username" class="control-label">{_ Username _}</label>
        <div class="controls">
	    <input type="text" id="username" name="username" value="" class="span4" placeholder="{_ user@example.com _}" autofocus="autofocus" autocapitalize="off" autocomplete="on" />
            {% validate id="username" type={presence} %}
        </div>
    </div>

    <div class="control-group">
        <label for="password" class="control-label">{_ Password _}</label>
        <div class="controls">
	    <input type="password" id="password" class="span4" name="password" value="" autocomplete="on" />
        </div>
    </div>

    <div class="control-group">
        <div class="controls">
	    <input type="checkbox" id="{{ #rememberme }}" name="rememberme" value="1" />
	    <label class="checkbox inline" for="{{ #rememberme }}">{_ Stay logged on unless I log off. _}</label>
        </div>
    </div>

    <div class="control-group buttons">
        <div class="controls">
	    <button class="btn btn-primary btn-large" type="submit">{_ Log on _}</button>
            <p class="pull-right"><a href="{% url logon f="reminder" %}">{_ I forgot my password _}.</a></p>
        </div>
    </div>
</form>
