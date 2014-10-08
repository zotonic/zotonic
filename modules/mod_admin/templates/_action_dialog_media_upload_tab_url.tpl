<div class="tab-pane" id="{{ tab }}-url">
	<p>
	    {_ Upload a file which is already on the internet. _}
	    {% if not id %}
	        {_ You have to specify a description of the file to make it easier to find and share. _}
	    {% endif %}
	</p>

	{% wire id=#urlform type="submit" 
		postback={media_url predicate=predicate actions=actions id=id subject_id=subject_id stay=stay callback=callback} 
		delegate=`action_admin_dialog_media_upload` 
	%}
	<form id="{{ #urlform }}" method="POST" action="postback" class="form">
		{% if not id %}
		    <div class="form-group row">
		        <label class="control-label col-md-3" for="new_media_title">{_ Media title _}</label>
                <div class="col-md-9">
		            <input type="text" class="col-lg-4 col-md-4 form-control" id="new_media_title_url" name="new_media_title_url" value="{{ title|escape }}" />
		            {% validate id="new_media_title_url" type={presence} %}
                </div>
		    </div>
		{% endif %}

		<div class="form-group row">
		    <label class="control-label col-md-3" for="upload_file">{_ Media URL _}</label>
            <div class="col-md-9">
		        <input type="text" class="col-lg-4 col-md-4 form-control" id="url" name="url" />
		        {% validate id="url" type={presence} type={format pattern="^https?://.+"} %}
            </div>
		</div>

		<div class="modal-footer">
		    {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
		    <button class="btn btn-primary" type="submit">{% if not id %}{_ Make media item _}{% else %}{_ Replace media item _}{% endif %}</button>
		    </div>
        </fieldset>
	</form>
</div>
