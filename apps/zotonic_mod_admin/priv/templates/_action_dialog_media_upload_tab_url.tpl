{% with id.medium as medium %}
<div class="tab-pane" id="{{ tab }}-url">
	{# todo: check the content_group_id #}

	{% wire id=#urlform type="submit" 
		postback={media_url_embed 
						subject_id=subject_id
						object_id=object_id
						predicate=predicate  
						id=id  
						redirect=redirect|if_undefined:(not stay)
						content_group_id=content_group_id
						actions=actions callback=callback
						discover_elt=#discover} 
		delegate=`z_admin_media_discover` 
	%}
	<form id="{{ #urlform }}" method="POST" action="postback" class="form form-horizontal">
		<p>
		    {_ Enter a URL or embed code. It will be analyzed and you can choose how to import the data. _}
		</p>

		<div class="form-group row">
		    <label class="control-label col-md-3" for="upload_file">{_ URL or Embed Code_}</label>
            <div class="col-md-9">
		        <textarea type="text" class="col-lg-4 col-md-4 form-control do_autofocus" id="url" name="url" rows="3">{{ medium.media_import|escape }}</textarea>
		        {% validate id="url" type={presence} %}
		        <p class="help-block">{_ Embed code will be sanitized. Only whitelisted sites are allowed in embed codes. _}</p>
            </div>
		</div>

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
