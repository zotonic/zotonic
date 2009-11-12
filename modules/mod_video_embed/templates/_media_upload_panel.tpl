{# Panel for defining the embed code #}
<div id="{{ tab }}-embed">
	<p>Embed a video or other media. Here you can paste embed code from YouTube, Vimeo or other services.</p>

	{% wire id=#form type="submit" postback={add_video_embed predicate=predicate actions=actions id=id subject_id=subject_id stay=stay} delegate="mod_video_embed" %}

	<form id="{{ #form }}" method="POST" action="postback">
		<div class="new-media-wrapper">
			{% if not id %}
				<div class="form-item clearfix">
					<label for="{{ #title }}" style="color:white">Media title</label>
					<input type="text" id="{{ #title }}" name="title" value="{{ title|escape }}" />
					{% validate id=#title name="title" type={presence} %}
				</div>
	
				<div class="form-item clearfix">
					<label for="{{ #group_id }}">Group</label>
					<select id="{{ #group_id }}" name="group_id">
					{% for id in m.acl.member %}
						<option value="{{ id }}" {% ifequal group_id id %}selected="selected"{% endifequal %}>{{ m.rsc[id].title }}</option>
					{% endfor %}
					</select>
				</div>
			{% endif %}

			<div class="form-item clearfix">
				<label for="{{ #service }}">From site</label>
				<select id="{{ #service }}" name="video_embed_service">
					{% include "_video_embed_service_options.tpl" %}
				</select>
			</div>
	
			<div class="form-item clearfix">
				<label for="{{ #embed_code }}">Embed code</label>
				<textarea id="{{ #embed_code }}" name="video_embed_code"></textarea>
				{% validate id=#embed_code name="video_embed_code" type={presence} %}
			</div>
	
			<div class="form-item clearfix">
				<button type="submit">{% if id %}Replace{% else %}Make{% endif %} media item</button>
				{% button action={dialog_close} text="Cancel" %}
			</div>
		</div>
	</form>
</div>
