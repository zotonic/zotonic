{% if m.acl.use.mod_import_feed %}
    <p>
        {_ Delete this feed and its import history? _}
        <strong>{{ feed.title|escape }}</strong>
    </p>
    {% wire id=#delete type="submit" postback={delete_feed id=feed.id} delegate=`mod_import_feed` %}
    <form id="{{ #delete }}" method="post" action="postback">
        <div class="radio">
            <label>
                <input type="radio" name="delete_content" value="0" checked>
                {_ Delete only the feed; keep all imported content _}
            </label>
        </div>
        <div class="radio">
            <label>
                <input type="radio" name="delete_content" value="1">
                {_ Also delete imported articles and media _}
            </label>
        </div>
        <p class="help-block">{_ Protected resources and resources you cannot delete will be kept. Deletable content shared with other feeds will also be deleted. _}</p>
        <div class="modal-footer">
            {% button class="btn btn-default" text=_"Cancel" action={dialog_close} %}
            <button type="submit" class="btn btn-danger">{_ Delete feed _}</button>
        </div>
    </form>

{% endif %}
