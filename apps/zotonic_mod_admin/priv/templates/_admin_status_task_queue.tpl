<table class="table">
    <thead>
        <tr>
            <th>{_ Module _}</th>
            <th>{_ Function _}</th>
            <th>{_ Count _}</th>
            <th>{_ Next due _}</th>
            <th></th>
        </tr>
    </thead>
    <tbody>
        {% for t in m.admin_status.task_queue %}
            {% with forloop.counter as nr %}
            <tr>
                <td>{{ t.module|escape }}</td>
                <td>{{ t.function|escape }}</td>
                <td>{{ t.count }}</td>
                <td>
                    {{ t.due|date:"Y-m-d H:i:s" }}
                    {% if t.error_count_total %}
                        <br>
                        <span class="text-danger">
                            <span class="glyphicon glyphicon-exclamation-sign"></span>
                            {% trans "Some tasks are retrying.<br>Total errors: {total}<br>Highest for single task: {max}"
                                    total=t.error_count_total
                                    max=t.error_count_max
                            %}
                        </span>
                    {% endif %}
                </td>
                <td>
                    <button id="{{ #task.nr }}" class="btn btn-danger btn-xs">{_ Delete _}</button>
                    {% wire id=#task.nr
                            action={confirm
                                text = [
                                    _"Are you sure you want to delete all tasks for this module and function?",
                                    "<br><br>",
                                    _"Deletion cannot be undone."
                                ]
                                is_danger
                                ok = _"Delete"
                                postback={delete_tasks module=t.module function=t.function}
                                delegate=`mod_admin`
                            }
                    %}
                </td>
            </tr>
            {% endwith %}
        {% empty %}
            <tr>
                <td colspan="3">
                    <p>
                        {_ There are no tasks in the task queue. _}
                    </p>
                </td>
            </tr>
        {% endfor %}
    </tbody>
</table>