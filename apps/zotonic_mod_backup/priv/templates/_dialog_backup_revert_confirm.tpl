{% wire id="dialog_backup_revert_confirm"
        type="submit"
        postback={revert rsc_id=rsc_id rev_id=rev_id}
        delegate=`controller_admin_backup_revision`
        action={mask message=_"Reverting page..." body}
%}
<form class="dialog" id="dialog_backup_revert_confirm">
    <p>{_ Are you sure you want to revert to this version? _}</p>

    <p>
        {_ You can recover incoming and outgoing connections to other pages. _}
        {_ It could be that a connected dependent page was deleted because of the deletion of this page, such a deleted dependent page can also be recovered. _}
    </p>
    <div class="form-group">
        <label class="checkbox">
            <input type="checkbox" checked name="outgoing_edges">
            {_ Recover outgoing edges to other pages _}
        </label>
        <label class="checkbox">
            <input type="checkbox" checked name="incoming_edges">
            {_ Recover incoming edges from other pages _}
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
        {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
        <button class="btn btn-primary" type="submit">{_ Revert _}</button>
    </div>
</form>
