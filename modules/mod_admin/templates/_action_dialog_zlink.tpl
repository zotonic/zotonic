<div class="control-group">
    <label class="control-label" for="{{#input}}">{_ Type the title of the page you want to connect to. _}</label>
    <div class="controls">
        <input id="{{#input}}" class="autocompleter span6 do_autofocus" type="text" value="" />
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
    {% button class="btn" text=_"Cancel" action={dialog_close} tag="a" %}
</div>
