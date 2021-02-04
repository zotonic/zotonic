<p>{_ Enter the title of the new predicate: _}</p>

{% wire id=#form type="submit" postback="predicate_new" delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback" class="form">

    <input type="hidden" name="redirect" value="{{ redirect }}" />

    <div class="form-group label-floating">
        <input type="text" id="new_predicate_title" name="new_predicate_title" value="{{ title|escape }}" class="input-xlarge do_autofocus form-control" placeholder="{_ Title _}">
        {% validate id="new_predicate_title" type={presence} %}
        <label class="control-label" for="new_predicate_title">{_ Title _}</label>
    </div>

    <div class="modal-footer">
	    {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
	    <button class="btn btn-primary" type="submit">{_ Make predicate _}</button>
    </div>
</form>

