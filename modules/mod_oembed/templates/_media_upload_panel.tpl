{# Panel for defining the embed code #}
{% with id.medium as medium %}
{% with medium.mime == "text/html-oembed" as is_oembed %}
<div id="{{ tab }}-oembed">
	<p>{_ Embed a video or other media URL. Here you can paste any URL from YouTube, Vimeo or other services. _}</p>

	{% wire id=#form type="submit" postback={add_video_embed predicate=predicate actions=actions id=id subject_id=subject_id stay=stay} delegate="mod_oembed" %}

	<form id="{{ #form }}" method="POST" action="postback">
		<div class="new-media-wrapper">

			<div class="form-item clearfix">
				<label for="{{ #embed_code }}">{_ Embed URL _}</label>
				<input id="{{ #embed_code }}" name="oembed_url" value="{{ medium.oembed_url }}" />
				{% validate id=#embed_code name="oembed_url" type={presence} %}
                {% if not id %}
                {% wire id=#embed_code type="change" postback={do_oembed} delegate="mod_oembed" %}
                {% endif %}
			</div>

			{% if not id %}
				<div class="form-item clearfix">
                    {_ The media title will be automatically detected from its URL. _}
                </div>
				<div class="form-item clearfix" style="display:none">
				    <label for="oembed-title" style="color:white">&nbsp;</label>
                                    <img id="oembed-image" src="" width="180" />
				</div>
				<div class="form-item clearfix">
					<label for="oembed-title" style="color:white">{_ Media title _}</label>
					<input type="text" id="oembed-title" name="title" value="{{ title|escape }}" />
					{% validate id="oembed-title" name="title" type={presence} %}
				</div>
				<div class="form-item clearfix">
					<label for="oembed-summary" style="color:white">{_ Summary _}</label>
					<textarea id="oembed-summary" name="summary">{{ summary|escape }}</textarea>
				</div>
			{% endif %}

			<div class="form-item clearfix">
				<button type="submit">{% if id %}{_ Replace media item _}{% else %}{_ Make media item _}{% endif %}</button>
				{% button action={dialog_close} text=_"Cancel" %}
			</div>
		</div>
	</form>
</div>

{% if is_oembed %}
	{% wire action={script script=["$('#",tabs,"').tabs().tabs('select', '#",tab,"-oembed')"]} %}
{% endif %}

{% endwith %}
{% endwith %}
