<tr>
    <th></th>
    {% if kind == `rsc` %}
        <th>{_ ACL user group _}</th>
        <th>{_ Content group _}</th>
        <th>{_ Category _}</th>
    {% elseif kind == `collab` %}
        <th>{_ Category _}</th>
    {% elseif kind == `module` %}
        <th>{_ ACL user group _}</th>
        <th>{_ Module _}</th>
    {% endif %}
    <th>{_ Permissions _}</th>
    <th></th>
</tr>
