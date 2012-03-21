<table class="table table-striped do_adminLinkedTable">
    <thead>
        <tr>
            <th width="15%">{_ Title _}</th>
            <th width="30%">{_ Access to _}</th>
            <th width="20%">{_ Consumer key _}</th>
            <th width="30%">{_ Consumer secret _}</th>
        </tr>
    </thead>

    <tbody>
        {% for app in m.oauth_app %}
        {% with app.id as id %}
        <tr id="{{ #tr.id }}" title="{{ app.application_descr }}" data-href="#">
            <td>{{ app.application_title }}</td>
            <td>
                {% for perm in m.oauth_perms.humanreadable[app.id] %}
                {{ perm.desc }}{% if not forloop.last %},{% endif %}
                {% empty %}
                &nbsp;
                {% endfor %}
            </td>
            <td>
                <tt style="font-size: 70%">{{ app.consumer_key }}</tt>
            </td>
            <td>
                <div class="pull-right">
                    {% button class="btn btn-mini" text=_"Edit" postback={start_edit_app id=app.id} title=_"Change the title, description and access permissions of this application." %}
                    {% button class="btn btn-mini" text=_"Users" postback={start_tokens id=app.id} title=_"Show applications/users that are using this key" %}
                    {% button class="btn btn-mini" text=_"Delete" postback={start_del_app id=app.id} %}
                </div>

                <tt style="font-size: 70%">{{ app.consumer_secret }}</tt>
            </td>
        </tr>
        {% wire id=#tr.id postback={start_edit_app id=id} %}
        
        {% endwith %}
        {% empty %}
        <tr>
            <td colspan="4">
                {_ No applications registered yet. _}
            </td>
        </tr>
        {% endfor %}
    </tbody>
</table>
