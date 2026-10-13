{% if m.import_feed.can_select_user %}
    <p class="alert alert-warning">{_ Administrator accounts cannot run feed imports. Use a non-administrator account with limited permissions. _}</p>
    <p>{_ Select the account whose permissions will be used to import articles and media. Save the feed to apply your selection. _}</p>

    {% wire id="feed-user-search" type="submit" postback=`search_users` delegate=`mod_import_feed` %}
    <form id="feed-user-search" method="post" action="postback" class="form-inline">
        <label for="feed-user-search-text">{_ Search users _}</label>
        <input id="feed-user-search-text" name="search_text" class="form-control" type="search">
        <button class="btn btn-default" type="submit">{_ Search _}</button>
    </form>

    <div id="feed-user-results">
        {% include "_admin_import_feed_user_results.tpl" %}
    </div>

    <div class="modal-footer">
        {% button class="btn btn-default" text=_"Cancel" action={dialog_close} %}
    </div>
{% endif %}
