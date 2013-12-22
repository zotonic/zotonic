{% wire id=#form type="submit" postback={config_edit module=module key=key on_success=on_success} delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback" class="form-horizontal">
    <div class="control-group">
        <label class="control-label" for="{{ #module }}">{_ Module _}</label>
        <div class="controls">
            <input type="text" id="{{ #module }}" name="module" value="{{ module|escape }}" class="input-xlarge" />
        </div>
    </div>
    <div class="control-group">
        <label class="control-label" for="{{ #key }}">{_ Key _}</label>
        <div class="controls">
            <input type="text" id="{{ #key }}" name="key" value="{{ key|escape }}" class="input-xlarge" />
        </div>
    </div>
    <div class="control-group">
        <label class="control-label" for="{{ #value }}">{_ Value _}</label>
        <div class="controls">
            <input type="text" id="{{ #value }}" name="val" value="{{ value|escape }}" class="input-xlarge do_autofocus" />
        </div>
    </div>
    
    <div class="modal-footer">
	    {% button class="btn" action={dialog_close} text=_"Cancel" tag="a" %}
        <button class="btn btn-primary" type="submit">{_ Save _}</button>
    </div>
</form>