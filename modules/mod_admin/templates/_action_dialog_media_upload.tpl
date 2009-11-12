{% tabs id=#tabs %}
<div id="{{ #tabs }}">
	<ul class="clearfix">
		<li><a href="#{{ #tab }}-upload">Upload</a></li>
		{% all include "_media_upload_tab.tpl" tab=#tab %}
	</ul>
	
	<div id="{{ #tab }}-upload">
		<p>
			Upload a file from your computer.
			{% if not id %}
				You have to specify a description of the file to make it easier to find and share.  You also have to specify with which group you want to share the uploaded file.
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
	
	{% all include "_media_upload_panel.tpl" tab=#tab %}

</div>
