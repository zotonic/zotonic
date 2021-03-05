{% with id.medium as medium %}
<div class="tab-pane" id="{{ tab }}-url">
	{% wire id=#urlform type="submit"
		postback={media_url_embed
						subject_id=subject_id
						object_id=object_id
						predicate=predicate
						id=id
						is_zlink=is_zlink
						is_zmedia=is_zmedia
						is_replace_medium=is_replace_medium
						redirect=redirect|if_undefined:(not stay)
						content_group_id=content_group_id
						actions=actions callback=callback
						discover_elt=#discover}
		delegate=`z_admin_media_discover`
	%}
	<form id="{{ #urlform }}" method="POST" action="postback" class="form">
		<p>
		    {_ Enter a URL or embed code. It will be analyzed and you can choose how to import the data. _}
		</p>

		<div class="form-group label-floating">
	        <textarea type="text" class="form-control do_autofocus" id="url" name="url" rows="3" placeholder="{_ Media URL or HTML _}">{{ medium.media_import|escape }}</textarea>
	        {% validate id="url" type={presence} %}
		    <label class="control-label" for="url">{_ Media URL or HTML _}</label>
		</div>
		<p class="help-block">
			<span class="glyphicon glyphicon-info-sign"></span>
			{_ Embed code will be sanitized for allowed sites and html. _}
		</p>

		<div class="modal-footer">
		    {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
		    <button class="btn btn-primary" type="submit">{_ Discover media _}</button>
	    </div>
	</form>

	<div id="{{ #discover }}" style="display:none">
	</div>
</div>

{% if medium.media_import %}
    {% javascript %}
        $('#{{ tab }} a[href="#{{ tab }}-url"]').tab('show');
    {% endjavascript %}
{% endif %}
{% endwith %}
