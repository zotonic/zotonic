{# Panel for defining the embed code #}
{% if not tabs_enabled or "embed"|member:tabs_enabled %}

{% with id.medium as medium %}
{% with medium.mime == "text/html-oembed" as is_video_embed %}
<div class="tab-pane" id="{{ tab }}-embed">
    <p>{_ Embed a video or other media. Here you can paste embed code from YouTube, Vimeo or other services. _}</p>

    {% wire id=#form type="submit" 
    		postback={add_video_embed predicate=predicate actions=actions id=id subject_id=subject_id callback=callback stay=stay} 
    		delegate="mod_video_embed" %}

    <form id="{{ #form }}" method="POST" action="postback" class="form">
	{% if not id %}
	    <div class="form-group row">
	        <label class="control-label col-md-3" for="{{ #title }}">{_ Media title _}</label>
            <div class="col-md-9">
	        <input type="text" id="{{ #title }}" name="title" value="{{ title|escape }}" class="col-lg-4 col-md-4 form-control" />
	        {% validate id=#title name="title" type={presence} %}
            </div>
	</div>
	{% endif %}

	<div class="form-group row">
	    <label class="control-label col-md-3" for="{{ #service }}">{_ From site _}</label>
        <div class="col-md-9">
	        <select id="{{ #service }}" name="video_embed_service" class="col-lg-4 col-md-4 form-control">
		    {% include "_video_embed_service_options.tpl" service=medium.video_embed_service %}
                </select>
            </div>
	</div>
	
	<div class="form-group row">
	    <label class="control-label col-md-3" for="{{ #embed_code }}">{_ Embed code _}</label>
        <div class="col-md-9">
	        <textarea id="{{ #embed_code }}" class="form-control" name="video_embed_code" rows="10">{% if is_video_embed %}{{ medium.video_embed_code|escape }}{% endif %}</textarea>
	        {% validate id=#embed_code name="video_embed_code" type={presence} %}
            </div>
	</div>
	
	<div class="modal-footer">
	    {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
	    <button class="btn btn-primary" type="submit">{% if id %}{_ Replace media item _}{% else %}{_ Make media item _}{% endif %}</button>
	</div>
    </form>
</div>

{% endwith %}
{% endwith %}

{% endif %}
