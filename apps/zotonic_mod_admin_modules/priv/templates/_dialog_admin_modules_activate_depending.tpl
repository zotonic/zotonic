<h3>{_ Modules are missing dependencies _}</h3>

<p>{_ The following modules will not start because they miss dependencies: _}</p>

<table class="table">
    <thead>
      <tr>
          <th>{_ Module _}</th>
          <th>{_ Dependencies _}</th>
      </tr>
    </thead>
    <tbody>
    {% for mod, deps in missing %}
        <tr>
            <th>{{ mod|escape }}</th>
            <td>
                <ul>
                    {% for mod in deps %}
                        <li>{{ mod|escape }}</li>
                    {% endfor %}
                </ul>
            </td>
        </tr>
    {% endfor %}
    </tbody>
</table>

<p>{_ Do you still want to activate the module _} <b>{{ module|escape }}</b>?</p>

<div class="modal-footer">
    {% button tag="a" class="btn btn-primary" text=_"Cancel" action={dialog_close} %}
    {% button tag="a"
              class="btn btn-danger"
              text=_"Activate"
              postback={module_activate_confirm module=module}
              delegate=`action_admin_modules_module_toggle`
    %}
</div>
