{% extends "admin_edit_widget_std.tpl" %}

{# Widget with main rsc controls: publish, delete, duplicate, etc  #}

{% block widget_title %}{_ Publish this page _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}sidebar-publish{% endblock %}
{% block widget_class %}dark{% endblock %}


{% block widget_content %}
<div class="admin-form ">
    <div class="control-group">
        <div class="pull-right">
	    {% button class="btn" text=_"Cancel" action={redirect back} title=_"Go back." %}
        </div>        
	{% button type="submit" id="save_stay" class="btn btn-primary" text=_"Save" title=_"Save this page." disabled=not is_editable %}
	{% if is_editable %}
	    {% button type="submit" id="save_view" class="btn" text=_"Save &amp; view" title=_"Save and view the page." %}
	{% else %}
	    {% button id="save_view" class="btn-primary" text=_"view" title=_"View this page." action={redirect id=id} %}
	{% endif %}

    </div>

    <div class="control-group">
	<label for="is_published" class="checkbox inline">
	    <input type="checkbox" id="is_published" name="is_published" value="1" {% if r.is_published %}checked="checked"{% endif %}/>
            {_ Published _}
        </label>
        
	<label for="is_featured" class="checkbox inline">
            <input type="checkbox" id="is_featured" name="is_featured" value="1" {% if r.is_featured %}checked="checked"{% endif %}/>
	    {_ Featured _}
        </label>

	<label for="is_protected" class="checkbox inline" title="{_ Protect from deletion _}">
            <input type="checkbox" id="is_protected" name="is_protected" value="1" {% if r.is_protected %}checked="checked"{% endif %} {% ifequal id 1 %}disabled="disabled"{% endifequal %} />
	    {_ Protect _}
        </label>
    </div>

    <div class="control-group">
        <div class="pull-right">
	    {% for id in m.search[{previous id=id cat=m.rsc[id].category.name pagelen=1}] %}
	    {% button class="btn btn-small right" text="&laquo;" action={redirect dispatch="admin_edit_rsc" id=id} title=_"Previous in category: "|append:m.rsc[id].title %}
	    {% endfor %}

	    {% for id in m.search[{next id=id cat=m.rsc[id].category.name pagelen=1}] %}
	    {% button class="btn btn-small" text="&raquo;" action={redirect dispatch="admin_edit_rsc" id=id} title=_"Next in category: "|append:m.rsc[id].title %}
	    {% endfor %}
        </div>

	{% ifnotequal id 1 %}
	    {% button class="btn btn-small" disabled=(r.is_protected or not m.rsc[id].is_deletable) id="delete-button" text=_"Delete" action={dialog_delete_rsc id=r.id on_success={redirect back}} title=_"Delete this page." %}
	{% endifnotequal %}
	
	{% if is_editable %}
	    {% button type="submit" id="save_duplicate" class="btn btn-small" text=_"Duplicate" title=_"Duplicate this page." %}
	{% else %}
	    {% button	class="btn btn-small" 
			text=_"Duplicate" 
			action={dialog_duplicate_rsc id=id} 
			title=_"Duplicate this page."
			disabled=(not m.acl.insert[r.category.name]) %}
	{% endif %}
    </div>
</div>
{% endblock %}
