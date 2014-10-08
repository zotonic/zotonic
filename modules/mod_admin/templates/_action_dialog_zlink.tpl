<div class="form-group">
    <label class="control-label" for="{{#input}}">{_ Type the title of the page you want to connect to. _}</label>
    <div>
        <input id="{{#input}}" class="autocompleter do_autofocus col-lg-6 col-md-6 form-control" type="text" value="" />
        <ul id="{{#suggestions}}" class="suggestions-list"></ul>
    </div>
</div>

{% wire id=#input
type="keyup" 
action={typeselect
target=#suggestions 
action_with_id={with_args action={zlink} arg={id select_id}}
action={dialog_close}
}
%}

<div class="modal-footer">
    {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
</div>
