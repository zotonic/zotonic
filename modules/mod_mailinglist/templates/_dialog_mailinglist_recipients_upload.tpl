
{% wire id=#form type="submit" delegate="mod_mailinglist" postback={mailinglist_upload id=id} %}
<form id="{{ #form }}" method="post" action="postback" enctype="multipart/form-data" class="form">
    <p>{_ Upload a file with recipients. The file must contain a single e-mail address per line. The file’s character set must be utf-8. _}</p>
	
	<div class="form-group row">
		<label class="control-label col-md-3" for="{{ #file }}">{_ Select file _}</label>
		<div class="col-md-9">
			<input class="form-control" type="file" id="{{ #file }}" name="file" />
			{% validate id=#file name="file" type={presence} %}
		</div>
	</div>
	
	<div class="form-group row">
        <div class="col-md-9 col-md-offset-3">
		    <div class="checkbox">
			    <label>
				    <input type="checkbox" name="truncate" id="{{ #truncate }}" />
				    {_ Delete all current recipients before adding the file. _}
			    </label>
		    </div>
        </div>
	</div>

	<div class="modal-footer">
	    {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
	    {% button class="btn btn-primary" text=_"Upload file" %}
	</div>
</form>
