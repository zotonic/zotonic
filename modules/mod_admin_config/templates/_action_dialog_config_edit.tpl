{% wire id=#form type="submit" postback={config_edit module=module key=key on_success=on_success} delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback" class="form">
    <div class="form-group row">
        <label class="control-label col-md-3" for="{{ #module }}">{_ Module _}</label>
        <div class="col-md-6">
            <input type="text" id="{{ #module }}" name="module" value="{{ module|escape }}" class="input-xlarge form-control" />
        </div>
    </div>
    <div class="form-group row">
        <label class="control-label col-md-3" for="{{ #key }}">{_ Key _}</label>
        <div class="col-md-6">
            <input type="text" id="{{ #key }}" name="key" value="{{ key|escape }}" class="input-xlarge form-control" />
        </div>
    </div>
    <div class="form-group row">
        <label class="control-label col-md-3" for="{{ #value }}">{_ Value _}</label>
        <div class="col-md-9">
            <input type="text" id="{{ #value }}" name="val" value="{{ value|escape }}" class="input-xlarge do_autofocus form-control" />
        </div>
    </div>
    
    <div class="modal-footer">
	    {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
        <button class="btn btn-primary" type="submit">{_ Save _}</button>
    </div>
</form>
