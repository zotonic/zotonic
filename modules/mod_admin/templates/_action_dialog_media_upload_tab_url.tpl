
<div class="tab-pane" id="{{ tab }}-url">
	<p>
	    {_ Upload a file which is already on the internet. _}
	    {% if not id %}
	    {_ You have to specify a description of the file to make it easier to find and share. _}
	    {% endif %}
	</p>

	{% wire id=#urlform type="submit" 
		postback={media_url predicate=predicate actions=actions id=id subject_id=subject_id stay=stay callback=callback} 
		delegate=delegate 
	%}
	<form id="{{ #urlform }}" method="POST" action="postback" class="form-horizontal">
    <fieldset>
		{% if not id %}
		<div class="control-group">
		    <label class="control-label" for="new_media_title">{_ Media title _}</label>
                    <div class="controls">
		        <input type="text" class="span4" id="new_media_title_url" name="new_media_title_url" value="{{ title|escape }}" />
		        {% validate id="new_media_title_url" type={presence} %}
                    </div>
		</div>
		{% endif %}

		<div class="control-group">
		    <label class="control-label" for="upload_file">{_ Media URL _}</label>
            <div class="controls">
		        <input type="text" class="span4" id="url" name="url" />
		        {% validate id="url" type={presence} type={format pattern="^https?://.+"} %}
            </div>
		</div>

		<div class="modal-footer">
		    {% button class="btn" action={dialog_close} text=_"Cancel" %}
		    <button class="btn btn-primary" type="submit">{% if not id %}{_ Make media item _}{% else %}{_ Replace media item _}{% endif %}</button>
		</div>
    </fieldset>
	</form>
</div>
