{# Panel for defining the embed code #}
{% with id.medium as medium %}
{% with medium.mime == "text/html-oembed" as is_oembed %}
<div class="tab-pane" id="{{ tab }}-oembed">
    <p>{_ Embed a video or other media URL. Here you can paste any URL from YouTube, Vimeo or other services. _}</p>

    {% wire id=#form type="submit" postback={add_video_embed predicate=predicate actions=actions id=id subject_id=subject_id stay=stay} delegate="mod_oembed" %}

    <form id="{{ #form }}" method="POST" action="postback" class="form-horizontal">
	<fieldset>

	    <div class="control-group">
		<label class="control-label" for="{{ #embed_code }}">{_ Embed URL _}</label>
                <div class="controls">
		    <input id="{{ #embed_code }}" class="span4" name="oembed_url" value="{{ medium.oembed_url }}" />
		    {% validate id=#embed_code name="oembed_url" type={presence} %}
                    {% if not id %}
                    {% wire id=#embed_code type="change" postback={do_oembed} delegate="mod_oembed" %}
                    {% endif %}
                </div>
	    </div>

	    {% if not id %}
            <p>
                {_ The media title will be automatically detected from its URL. _}
            </p>
	    <div class="control-group" style="display:none">
		<label class="control-label">&nbsp;</label>
                <div class="controls">
                    <img id="oembed-image" src="" width="180" />
                </div>
	    </div>
            
	    <div class="control-group">
		<label class="control-label" for="oembed-title">{_ Media title _}</label>
                <div class="controls">
		    <input type="text" class="span4" id="oembed-title" name="title" value="{{ title|escape }}" />
		    {% validate id="oembed-title" name="title" type={presence} %}
                </div>
	    </div>
            
	    <div class="control-group">
		<label class="control-label" for="oembed-summary">{_ Summary _}</label>
                <div class="controls">
		    <textarea class="span4" id="oembed-summary" name="summary">{{ summary|escape }}</textarea>
                </div>
	    </div>
	    {% endif %}

	    <div class="modal-footer">
		{% button class="btn" action={dialog_close} text=_"Cancel" %}
		<button class="btn btn-primary" type="submit">{% if id %}{_ Replace media item _}{% else %}{_ Make media item _}{% endif %}</button>
	    </div>
	</div>
    </form>
</div>

{% if is_oembed %}
{#{% wire action={script script=["$('#",tabs,"').tabs().tabs('select', '#",tab,"-oembed')"]} %}#}
{% endif %}

{% endwith %}
{% endwith %}
