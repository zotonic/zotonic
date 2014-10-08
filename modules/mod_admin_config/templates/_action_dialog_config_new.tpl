<p>{_ Please fill in the module, key and value for the new configuration key. _}</p>

{% wire id=#form type="submit" postback={config_new on_success=on_success} delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback" class="form">

    <div class="form-group row">
	    <label class="control-label col-md-3" for="{{ #module }}">{_ Module _}</label>
        <div class="col-md-6">
	        <input type="text" id="{{ #module }}" name="module" value="" class="do_autofocus form-control" />
	        {% validate id=#module name="module" type={presence} %}
        </div>
    </div>

    <div class="form-group row">
	    <label class="control-label col-md-3" for="{{ #key }}">{_ Key _}</label>
        <div class="col-md-6">
	        <input class="form-control" type="text" id="{{ #key }}" name="key" value="" />
	        {% validate id=#key name="key" type={presence} %}
        </div>
    </div>

    <div class="form-group row">
	    <label class="control-label col-md-3" for="{{ #value }}">{_ Value _}</label>
        <div class="col-md-9">
	        <input class="form-control" type="text" id="{{ #value }}" name="val" value="" />
        </div>
    </div>

    <div class="modal-footer">
        {% button class="btn btn-default" action={dialog_close} text="Cancel" tag="a" %}
        <button class="btn btn-primary" type="submit">{_ Add key _}</button>
    </div>
</form>

