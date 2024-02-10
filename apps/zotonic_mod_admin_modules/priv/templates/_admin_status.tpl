<div class="form-group">
    <div>
        {% button class="btn btn-outline-secondary" text=_"Rescan modules" action={module_rescan} %}
        <p class="help-block">{_ Rescanning will rebuild the index of all modules, actions, templates etc. It will also reload all dispatch rules. _}</p>
    </div>
</div>

