{% with m.backup_revision.restore_options[rev_id] as restore %}
{% if restore %}
{% wire id="revertconfirm"
        type="submit"
        postback={revert rsc_id=rsc_id rev_id=rev_id}
        delegate=`controller_admin_backup_revision`
%}
<form class="form" id="revertconfirm" action="postback">
    <p>{_ Are you sure you want to revert to this version? _}</p>
    <p class="help-block">{_ Recovering a deleted page requires permission to create pages in the selected category and content group. _}</p>

    {% if restore.category_id.is_missing or restore.content_group_id.is_missing %}
        <div class="alert alert-warning">
            {_ The original category or content group no longer exists. Confirm the suggested replacement or select another one. _}
        </div>
    {% endif %}
    <div class="form-group">
        <label for="restore-category">{_ Category _}</label>
        <p class="help-block">{_ Original _}: {{ restore.category_id.title|default:restore.category_id.name|escape }} ({{ restore.category_id.id|escape }})</p>
        <select id="restore-category" name="category_id" class="form-control" required>
            <option value="">{_ Select a category _}</option>
            {% for cat in m.category.tree_flat %}
                <option value="{{ cat.id }}" {% if cat.id == restore.category_id.suggested_id %}selected{% endif %}>{{ cat.indent }}{{ cat.id.title }}</option>
            {% endfor %}
        </select>
    </div>
    <div class="form-group">
        <label for="restore-content-group">{_ Content group _}</label>
        <p class="help-block">{_ Original _}: {{ restore.content_group_id.title|default:restore.content_group_id.name|escape }} ({{ restore.content_group_id.id|escape }})</p>
        <select id="restore-content-group" name="content_group_id" class="form-control" required>
            <option value="">{_ Select a content group _}</option>
            {% for cg in m.search.query::%{ cat: ["content_group", "acl_collaboration_group"], pagelen: 1000, sort: "pivot_title" } %}
                <option value="{{ cg }}" {% if cg == restore.content_group_id.suggested_id %}selected{% endif %}>{{ cg.title }}</option>
            {% endfor %}
        </select>
    </div>

    <p>
        {_ You can recover incoming and outgoing connections to other pages. _}
        {_ It could be that a connected dependent page was deleted because of the deletion of this page, such a deleted dependent page can also be recovered. _}
    </p>

    <div class="form-group">
        <label class="checkbox">
            <input type="checkbox" checked name="outgoing_edges">
            {_ Recover outgoing connections to other pages _}
        </label>
        <label class="checkbox">
            <input type="checkbox" checked name="incoming_edges">
            {_ Recover incoming connections from other pages _}
        </label>
        <label class="checkbox">
            <input type="checkbox" checked name="dependent">
            {_ Restore deleted <em>dependent</em> pages that were referred by this page _}
        </label>
    </div>

    <p class="help-block">
        <span class="glyphicon glyphicon-info-sign"></span> {_ Media files can only be recovered if they are not yet deleted from the file storage. Zotonic will keep media for at least 5 weeks. _}
        {_ After the page has been restored, you will be redirected to the edit page. _}
    </p>

    <div class="modal-footer">
        {% button class="btn btn-default" action={dialog_close} text=_"Cancel" type="button" %}
        <button class="btn btn-primary" type="submit">{_ Revert _}</button>
    </div>
</form>

{% endif %}
{% endwith %}
