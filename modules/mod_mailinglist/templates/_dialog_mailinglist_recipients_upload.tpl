
{% wire id=#form type="submit" delegate="mod_mailinglist" postback={mailinglist_upload id=id} %}
<form id="{{ #form }}" method="post" action="postback" enctype="multipart/form-data" class="form-horizontal">
<p>{_ Upload a file with recipients. The file must contain a single e-mail address per line. The file’s character set must be utf-8. _}</p>
	
    <div class="control-group">
	<label class="control-label" for="{{ #file }}">{_ Select file _}</label>
        <div class="controls">
	    <input type="file" id="{{ #file }}" name="file" />
	    {% validate id=#file name="file" type={presence} %}
	</div>
    </div>

    <div class="modal-footer">
	{% button class="btn" text=_"Cancel" action={dialog_close} %}
	{% button class="btn btn-primary" text=_"Upload file" %}
    </div>
</form>
