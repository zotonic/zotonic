<h3>{_ There are modules depending on _}: {{ module|escape }}</h3>

<p>{_ Deactivating this module will stop the following modules: _}</p>

<ul>
    {% for mod, deps in missing %}
        <li>{{ mod|escape }}</li>
    {% endfor %}
</ul>

<p>{% trans "Do you still want to deactivate the module <b>{module}</b>?" module=module|escape %}</p>

<div class="modal-footer">
    {% button tag="a" class="btn btn-primary" text=_"Cancel" action={dialog_close} %}
    {% button tag="a"
              class="btn btn-danger"
              text=_"Deactivate"
              postback={module_deactivate_confirm module=module}
              delegate=`action_admin_modules_module_toggle`
    %}
</div>
