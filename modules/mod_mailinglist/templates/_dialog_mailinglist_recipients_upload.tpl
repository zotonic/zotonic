<p>Upload a file with recipients. The file must contain a single e-mail address per line. The file’s character set must be utf-8.</p>

{% wire id=#form type="submit" delegate="mod_mailinglist" postback={mailinglist_upload id=id} %}
<form id="{{ #form }}" method="post" action="postback" enctype="multipart/form-data">
	
	<div class="form-item">
		<label for="{{ #file }}">Select file</label>
		<input type="file" id="{{ #file }}" name="file" />
	</div>
	{% validate id=#file name="file" type={presence} %}
	
	{% button text="Upload file" %}
	{% button text="Cancel" action={dialog_close} %}

</form>
