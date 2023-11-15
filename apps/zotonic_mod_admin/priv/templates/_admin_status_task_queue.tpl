<table class="table">
    <thead>
        <tr>
            <th>{_ Module _}</th>
            <th>{_ Function _}</th>
            <th>{_ Count _}</th>
            <th>{_ Next due _}</th>
        </tr>
    </thead>
    <tbody>
        {% for t in m.admin_status.task_queue %}
            <tr>
                <td>{{ t.module|escape }}</td>
                <td>{{ t.function|escape }}</td>
                <td>{{ t.count }}</td>
                <td>
                    {{ t.due|date:"Y-m-d H:i:s" }}
                    {% if t.error_count_total %}
                        <br>
                        <span class="text-danger">
                            <span class="fa fa-warning"></span>
                            {% trans "Some tasks are retrying.<br>Total errors: {total}<br>Highest for single task: {max}"
                                    total=t.error_count_total
                                    max=t.error_count_max
                            %}
                        </span>
                    {% endif %}
                </td>
            </tr>
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