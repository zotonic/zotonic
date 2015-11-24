<dl class="dl-horizontal">
    <dt>{_ Session Id _}</dt>
    <dd>{{ session_id }}</dd>

    <dt>{_ # Pages _}<dt>
    <dd>{{ pages | length }}</dd>
</dl>


<table class="table table-bordered">
        <thead>
            <tr><th >{_ Page Id _}</th><th >{_ Status _}</th></tr>
        </thead>
        <tbody>
        {% for page_id, state in pages %}
            <tr><td>{{ page_id }}</td><td>{{ state | pprint }}</td></tr>
        {% endfor %}
        </tbody>
</table>
