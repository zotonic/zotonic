{% tabs id=#tabs %}
<div id="{{ #tabs }}">
	<ul class="clearfix">
		<li><a href="#{{ #tab }}-upload">Upload</a></li>
		<li><a href="#{{ #tab }}-url">URL</a></li>
		{% all include "_media_upload_tab.tpl" tab=#tab %}
	</ul>
	
	<div id="{{ #tab }}-upload">
		<p>
			Upload a file from your computer.
			{% if not id %}
				You have to specify a description of the file to make it easier to find and share.
			{% endif %}
		</p>

		{% wire id=#form type="submit" postback={media_upload predicate=predicate actions=actions id=id subject_id=subject_id stay=stay} delegate=delegate %}
		<form id="{{ #form }}" method="POST" action="postback">
			<div class="new-media-wrapper">
				{% if not id %}
					<div class="form-item clearfix">
						<label for="new_media_title" style="color:white">Media title</label>
						<input type="text" id="new_media_title" name="new_media_title" value="{{ title|escape }}" />
						{% validate id="new_media_title" type={presence} %}
					</div>
				{% endif %}
				
				<div class="form-item clearfix">
					<label for="upload_file">Media file</label>
					<input type="file" id="upload_file" name="upload_file" />
					{% validate id="upload_file" type={presence} %}
				</div>
		
				<div class="form-item clearfix">
					<button type="submit">Upload file</button>
					{% button action={dialog_close} text="Cancel" %}
				</div>
			</div>
		</form>
	</div>

	<div id="{{ #tab }}-url">
		<p>
			Upload a file which is already on the internet.
			{% if not id %}
				You have to specify a description of the file to make it easier to find and share.
			{% endif %}
		</p>

		{% wire id=#urlform type="submit" postback={media_url predicate=predicate actions=actions id=id subject_id=subject_id stay=stay} delegate=delegate %}
		<form id="{{ #urlform }}" method="POST" action="postback">
			<div class="new-media-wrapper">
				{% if not id %}
					<div class="form-item clearfix">
						<label for="new_media_title" style="color:white">Media title</label>
						<input type="text" id="new_media_title_url" name="new_media_title_url" value="{{ title|escape }}" />
						{% validate id="new_media_title_url" type={presence} %}
					</div>
				{% endif %}

				<div class="form-item clearfix">
					<label for="upload_file">Media URL</label>
					<input type="text" id="url" name="url" />
					{% validate id="url" type={presence} type={format pattern="^https?://.+"} %}
				</div>

				<div class="form-item clearfix">
					<button type="submit">{% if not id %}Make media item{% else %}Replace media item{% endif %}</button>
					{% button action={dialog_close} text="Cancel" %}
				</div>
			</div>
		</form>
	</div>
	
	{% all include "_media_upload_panel.tpl" tab=#tab %}

</div>
