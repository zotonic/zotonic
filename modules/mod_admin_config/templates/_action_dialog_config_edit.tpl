{% wire id=#form type="submit" postback={config_edit module=module key=key on_success=on_success} delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback" class="form-horizontal">

    <div class="control-group">
	<label class="control-label" for="{{ #value }}">{_ Value _}</label>
        <div class="controls">
	    <input type="text" id="{{ #value }}" name="val" value="{{ m.config[module][key].value|escape }}" class="input-xlarge do_autofocus" />
	</div>
    </div>
    
    <div class="modal-footer">
	{% button class="btn" action={dialog_close} text=_"Cancel" %}
        <button class="btn btn-primary" type="submit">{_ Save _}</button>
    </div>
</form>

