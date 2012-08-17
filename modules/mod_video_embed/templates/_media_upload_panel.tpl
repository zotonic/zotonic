{# Panel for defining the embed code #}
{% with id.medium as medium %}
{% with medium.mime == "text/html-video-embed" as is_video_embed %}
<div class="tab-pane" id="{{ tab }}-embed">
    <p>Embed a video or other media. Here you can paste embed code from YouTube, Vimeo or other services.</p>

    {% wire id=#form type="submit" 
    		postback={add_video_embed predicate=predicate actions=actions id=id subject_id=subject_id callback=callback stay=stay} 
    		delegate="mod_video_embed" %}

    <form id="{{ #form }}" method="POST" action="postback" class="form-horizontal">
	{% if not id %}
	<div class="control-group">
	    <label class="control-label" for="{{ #title }}">Media title</label>
            <div class="controls">
	        <input type="text" id="{{ #title }}" name="title" value="{{ title|escape }}" class="span4" />
	        {% validate id=#title name="title" type={presence} %}
            </div>
	</div>
	{% endif %}

	<div class="control-group">
	    <label class="control-label" for="{{ #service }}">From site</label>
            <div class="controls">
	        <select id="{{ #service }}" name="video_embed_service" class="span4">
		    {% include "_video_embed_service_options.tpl" service=medium.video_embed_service %}
                </select>
            </div>
	</div>
	
	<div class="control-group">
	    <label class="control-label" for="{{ #embed_code }}">Embed code</label>
            <div class="controls">
	        <textarea id="{{ #embed_code }}" class="span4" name="video_embed_code" rows="10">{% if is_video_embed %}{{ medium.video_embed_code|escape }}{% endif %}</textarea>
	        {% validate id=#embed_code name="video_embed_code" type={presence} %}
            </div>
	</div>
	
	<div class="modal-footer">
	    {% button class="btn" action={dialog_close} text="Cancel" tag="a" %}
	    <button class="btn btn-primary" type="submit">{% if id %}Replace{% else %}Make{% endif %} media item</button>
	</div>
    </form>
</div>

{% endwith %}
{% endwith %}
