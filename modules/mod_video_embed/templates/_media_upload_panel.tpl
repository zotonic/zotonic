{# Panel for defining the embed code #}
{% with id.medium as medium %}
{% with medium.mime == "text/html-video-embed" as is_video_embed %}
<div id="{{ tab }}-embed">
	<p>{_ Embed a video or other media. Here you can paste embed code from YouTube, Vimeo or other services. _}</p>

	{% wire id=#form type="submit" postback={add_video_embed predicate=predicate actions=actions id=id subject_id=subject_id stay=stay} delegate="mod_video_embed" %}

	<form id="{{ #form }}" method="POST" action="postback">
		<div class="new-media-wrapper">
			{% if not id %}
				<div class="form-item clearfix">
					<label for="{{ #title }}" style="color:white">{_ Media title _}</label>
					<input type="text" id="{{ #title }}" name="title" value="{{ title|escape }}" />
					{% validate id=#title name="title" type={presence} %}
				</div>
			{% endif %}

			<div class="form-item clearfix">
				<label for="{{ #service }}">{_ From site _}</label>
				<select id="{{ #service }}" name="video_embed_service">
					{% include "_video_embed_service_options.tpl" service=medium.video_embed_service %}
				</select>
			</div>

			<div class="form-item clearfix">
				<label for="{{ #embed_code }}">{_ Embed code _}</label>
				<textarea id="{{ #embed_code }}" name="video_embed_code" rows="10">{% if is_video_embed %}{{ medium.video_embed_code|escape }}{% endif %}</textarea>
				{% validate id=#embed_code name="video_embed_code" type={presence} %}
			</div>

			<div class="form-item clearfix">
				<button type="submit">{% if id %}{_ Replace media item _}{% else %}{_ Make media item _}{% endif %}</button>
				{% button action={dialog_close} text=_"Cancel" %}
			</div>
		</div>
	</form>
</div>

{% if is_video_embed %}
	{% wire action={script script=["$('#",tabs,"').tabs().tabs('select', '#",#tab,"-embed')"]} %}
{% endif %}

{% endwith %}
{% endwith %}
