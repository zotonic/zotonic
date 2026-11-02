<div class="row form-group">
    <div class="col-md-8">
        <p class="help-block">{_ Rescanning will rebuild the index of all modules, actions, templates etc. It will also reload all dispatch rules. _}</p>
    </div>
    <div class="col-md-4 z-button-row">
        {% button class="btn btn-default" text=_"Rescan modules" action={module_rescan} %}
    </div>
</div>

