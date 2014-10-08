<p>{_ Please fill in the title of the new predicate. _}</p>

{% wire id=#form type="submit" postback="predicate_new" delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback" class="form">

    <input type="hidden" name="redirect" value="{{ redirect }}" />

    <div class="form-group row">
	    <label class="control-label col-md-3" for="new_predicate_title">{_ Title _}</label>
        <div class="col-md-9">
	        <input type="text" id="new_predicate_title" name="new_predicate_title" value="{{ title|escape }}" class="input-xlarge do_autofocus form-control" />
	        {% validate id="new_predicate_title" type={presence} %}
        </div>
    </div>

    <div class="modal-footer">
	    {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
	    <button class="btn btn-primary" type="submit">{_ Make predicate _}</button>
    </div>
</form>

