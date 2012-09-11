{% wire id=#form type="submit" postback={restore id=id} delegate=`resource_admin_backup` %}
<form id="{{ #form }}" method="POST" action="postback" class="form-horizontal">
	<p>{_ Select the backup file you want to upload. The file must be a .bert file. _}</p>

    <div class="control-group">
		<input id="{{ #upload }}" name="file" type="file" accept=".bert" />
		{% validate id=#upload name="file" type={presence} %}
	</div>

	<p class="alert"><strong>{_ Warning! _}</strong> {_ This will overwrite your page with the contents of the backup file. _}</p>

    <div class="modal-footer">
		{% button class="btn" action={dialog_close} text=_"Cancel" tag="a" %}
		<button class="btn btn-primary" type="submit">{_ Restore backup _}</button>
    </div>
</form>
