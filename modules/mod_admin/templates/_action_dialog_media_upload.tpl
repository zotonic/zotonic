{% tabs id=#tabs %}
<div id="{{ #tabs }}">
	<ul class="clearfix">
		<li><a href="#{{ #tab }}-upload">{_ Upload _}</a></li>
		<li><a href="#{{ #tab }}-url">{_ URL _}</a></li>
		{% all include "_media_upload_tab.tpl" tab=#tab %}
	</ul>
	
	<div id="{{ #tab }}-upload">
		<p>
			{_ Upload a file from your computer. _}
			{% if not id %}
				{_ You have to specify a description of the file to make it easier to find and share. _}
			{% endif %}
		</p>

		{% wire id=#form type="submit" postback={media_upload predicate=predicate actions=actions id=id subject_id=subject_id stay=stay} delegate=delegate %}
		<form id="{{ #form }}" method="POST" action="postback">
			<div class="new-media-wrapper">
				{% if not id %}
					<div class="form-item clearfix">
						<label for="new_media_title" style="color:white">{_ Media title _}</label>
						<input type="text" id="new_media_title" name="new_media_title" value="{{ title|escape }}" />
					</div>
				{% endif %}
				
				<div class="form-item clearfix">
					<label for="upload_file">{_ Media file _}</label>
					<input type="file" id="upload_file" name="upload_file" />
					{% validate id="upload_file" type={presence} %}
				</div>
		
				<div class="form-item clearfix">
					<button type="submit">{_ Upload file _}</button>
					{% button action={dialog_close} text="Cancel" %}
				</div>
			</div>
		</form>
	</div>

	<div id="{{ #tab }}-url">
		<p>
			{_ Upload a file which is already on the internet. _}
			{% if not id %}
				{_ You have to specify a description of the file to make it easier to find and share. _}
			{% endif %}
		</p>

		{% wire id=#urlform type="submit" postback={media_url predicate=predicate actions=actions id=id subject_id=subject_id stay=stay} delegate=delegate %}
		<form id="{{ #urlform }}" method="POST" action="postback">
			<div class="new-media-wrapper">
				{% if not id %}
					<div class="form-item clearfix">
						<label for="new_media_title" style="color:white">{_ Media title _}</label>
						<input type="text" id="new_media_title_url" name="new_media_title_url" value="{{ title|escape }}" />
						{% validate id="new_media_title_url" type={presence} %}
					</div>
				{% endif %}

				<div class="form-item clearfix">
					<label for="upload_file">{_ Media URL _}</label>
					<input type="text" id="url" name="url" />
					{% validate id="url" type={presence} type={format pattern="^https?://.+"} %}
				</div>

				<div class="form-item clearfix">
					<button type="submit">{% if not id %}{_ Make media item _}{% else %}{_ Replace media item _}{% endif %}</button>
					{% button action={dialog_close} text=_"Cancel" %}
				</div>
			</div>
		</form>
	</div>
	
	{% all include "_media_upload_panel.tpl" tab=#tab tabs=#tabs %}

</div>
