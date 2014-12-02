<iframe src="/lib/images/spinner.gif" id="logonTarget" name="logonTarget" style="display:none"></iframe>
<form id="logon_form" method="post" action="postback" class="z_logon_form" target="logonTarget">
    
    <input type="hidden" name="page" value="{{ page|escape }}" />
    <input type="hidden" name="handler" value="username" />

    <div class="form-group">
        <label for="username" class="control-label">{_ Username _}</label>
        <input class="form-control" type="text" id="username" name="username" value="" autofocus="autofocus" autocomplete="on" />
        {% validate id="username" type={presence} only_on_submit %}
    </div>

    <div class="form-group">
        <label for="password" class="control-label">{_ Password _}</label>
	    <input class="form-control" type="password" id="password" name="password" value="" autocomplete="on" />
	    {% validate id="password" type={presence} only_on_submit %}
    </div>

    <div class="form-group">
        <div class="checkbox">
            <label title="{_ Stay logged on unless I log off. _}">
                <input type="checkbox" name="rememberme" value="1" />
                {_ Remember me _}
            </label>
        </div>
    </div>
    
    <div class="form-group">
        <button class="btn btn-primary" type="submit">{_ Log on _}</button>
    </div>
</form>