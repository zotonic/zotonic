<div class="system-content-warning">
    <b>{_ Proceed with care: _}</b>
    {% if id == 1 %}
        {_ You are editing the <b>Site Administrator Account</b>. _}
    {% else %}
        {_ You are editing a: _} <b>{{ category_id.title }}</b>.
    {% endif %}
    {_ This is system content. _}

    <a href="https://zotonic.com/en/latest/user-guide/datamodel.html" target="_blank">
        [{_ Click here for more information _}]
    </a>
</div>