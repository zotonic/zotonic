<p>
    {_ You are going to duplicate the page _} “{{ m.rsc[id].title }}”<br/>
    {_ Please fill in the title of the new page. _}
</p>

{% wire id=#form type="submit" postback={duplicate_page id=id} delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback" class="form-horizontal">

    <div class="control-group">
	<label class="control-label" for="new_rsc_title">Page title</label>
        <div class="controls">
	    <input type="text" id="new_rsc_title" name="new_rsc_title" value="{{ m.rsc[id].title }}" />
	    {% validate id="new_rsc_title" type={presence} %}
        </div>
    </div>

    <div class="control-group">
        <label for="{{ #published }}" class="control-label">{_ Published _}</label>
        <div class="controls">
	    <input type="checkbox" id="{{ #published }}" name="is_published" value="1" />
	</div>
    </div>

    <div class="modal-footer">
	{% button class="btn" action={dialog_close} text=_"Cancel" tag="a" %}
	<button class="btn btn-primary" type="submit">{_ Duplicate page _}</button>
    </div>

</form>

