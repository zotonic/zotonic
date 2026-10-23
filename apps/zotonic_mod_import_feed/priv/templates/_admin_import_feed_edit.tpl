{% if m.acl.use.mod_import_feed %}
    {% lib "css/admin_import_feed.css" %}
    <div class="row">
        <div class="col-md-6">
            <section class="widget" aria-labelledby="feed-settings-heading">
                <div class="widget-header" id="feed-settings-heading" role="heading" aria-level="3">
                    {_ Import settings _}
                </div>
                <div class="widget-content">
                    {% wire id="feed-settings" type="submit" postback={save id=feed.id is_zotonic_feed=feed.is_zotonic_feed} delegate=`mod_import_feed` %}
                    <form id="feed-settings" method="post" action="postback">
                        <input type="hidden" name="url" value="{{ feed.url|escape }}">
                        <p>{{ feed.url|escape }}</p>
                        <div class="form-group">
                            <label>{_ Import user _}</label>
                            <div id="feed-import-user">
                                {% if feed.id %}
                                    {% include "_admin_import_feed_user.tpl" user_id=feed.user_id %}
                                {% else %}
                                    {% include "_admin_import_feed_user.tpl" user_id=m.acl.user %}
                                {% endif %}
                            </div>
                            <p class="help-block">{_ Imports run with this account's permissions. Only administrators and users with full access to accounts can change it. _}</p>
                        </div>

                        <div class="form-group">
                            <label for="feed-auth">{_ Fetch using _}</label>
                            <select id="feed-auth" name="use_credentials" class="form-control">
                                <option value="0" {% if not feed.use_credentials %}selected{% endif %}>{_ Anonymous requests _}</option>
                                <option value="1" {% if feed.use_credentials %}selected{% endif %}>{_ Registered OAuth2 credentials _}</option>
                            </select>
                            <p class="help-block">{_ Uses credentials registered for the import user and destination host. Credentials are only sent over HTTPS. _}</p>
                            {% if m.site.environment == `development` %}
                                <p class="help-block">{_ Development environment: local destinations and self-signed certificates are allowed. _}</p>
                            {% endif %}
                        </div>

                        <div class="form-group">
                            <label for="feed-title">{_ Title _}</label>
                            <input id="feed-title" class="form-control" name="title" value="{{ feed.title|escape }}" required>
                        </div>

                        <div class="form-group">
                            <label for="feed-category">{_ Default category _}</label>
                            <select id="feed-category" class="form-control" name="category_id" required>
                                {% for cat in m.category.tree_flat %}
                                    <option value="{{ cat.id }}" {% if cat.id == feed.category_id %}selected{% endif %}>{{ cat.indent }}{{ cat.id.title }}</option>
                                {% endfor %}
                            </select>
                        </div>

                        <div class="checkbox">
                            <label>
                                <input type="checkbox" name="is_authoritative" value="1" {% if feed.is_authoritative %}checked{% endif %}>
                                {_ Import as authoritative local content _}
                            </label>
                        </div>
                        <p class="help-block">{_ By default imported content is non-authoritative. Both modes keep source identities and allow updates from this feed. _}</p>

                        {% if m.modules.active.mod_content_groups %}
                            <div class="form-group">
                                <label for="feed-group">{_ Content group _}</label>
                                <select id="feed-group" class="form-control" name="content_group_id">
                                    <option value="">{_ Default content group _}</option>
                                    {% for group in m.hierarchy.content_group.tree_flat %}
                                        <option value="{{ group.id }}" {% if group.id == feed.content_group_id %}selected{% endif %}>{{ group.indent }}{{ group.id.title }}</option>
                                    {% endfor %}
                                </select>
                            </div>
                        {% endif %}
                        <div class="checkbox">
                            <label>
                                <input type="checkbox" name="guess_category" value="1" {% if feed.guess_category %}checked{% endif %}>
                                {_ Guess category from tags _}
                            </label>
                        </div>
                        <p class="help-block">{_ Match existing category names or titles, otherwise use the default category. _}</p>
                        <div class="form-group">
                            <label>{_ Default connections _}</label>
                            <ul id="feed-connections" class="list-unstyled">
                                {% for predicate, object in feed.connections %}
                                    {% include "_admin_import_feed_connection.tpl" predicate=predicate object=object %}
                                {% endfor %}
                            </ul>
                            {% button class="btn btn-default" text=_"Add connection"
                                postback=`add_connection` delegate=`mod_import_feed` %}
                            <p class="help-block">{_ These connections are added to every imported article. Save the feed to apply changes. _}</p>
                        </div>

                        <div class="form-group">
                            <label for="feed-media">{_ Media _}</label>
                            <select id="feed-media" class="form-control" name="media_mode">
                                <option value="dependent" {% if feed.media_mode == `dependent` %}selected{% endif %}>{_ Import as dependent resources with depiction connections _}</option>
                                <option value="attached" {% if feed.media_mode == `attached` %}selected{% endif %}>{_ Attach the most important medium directly _}</option>
                                <option value="none" {% if feed.media_mode == `none` %}selected{% endif %}>{_ Do not import media _}</option>
                            </select>
                        </div>
                        {% if m.modules.active.mod_copyright %}
                            {% include "_admin_import_feed_copyright.tpl" %}
                        {% endif %}
                        <fieldset id="feed-zotonic-options"{% if feed.is_zotonic_feed == false %} class="hidden"{% endif %}>
                            <legend>{_ Zotonic resource feeds _}</legend>
                            <p class="help-block">{_ Entries advertising a Zotonic resource export use the native importer, preserving structured content and resource connections. _}</p>
                            <div class="form-group">
                                <label for="feed-import-depth">{_ Connection import depth _}</label>
                                <input id="feed-import-depth" name="import_depth" type="number" class="form-control"
                                    min="0" max="10" value="{{ feed.import_depth|default_if_undefined:1 }}" required>
                                <p class="help-block">{_ Default is 1: import directly connected resources. Use 0 to skip connections. _}</p>
                            </div>
                            {% if m.modules.active.mod_websub %}
                                <div class="checkbox">
                                    <label>
                                        <input type="checkbox" name="is_subscribe" value="1" {% if feed.is_subscribe %}checked{% endif %}>
                                        {_ Subscribe imported articles for updates _}
                                    </label>
                                </div>
                                <div class="checkbox">
                                    <label>
                                        <input type="checkbox" name="is_subscribe_connections" value="1" {% if feed.is_subscribe_connections %}checked{% endif %}>
                                        {_ Also subscribe imported connections for updates _}
                                    </label>
                                </div>
                                <p class="help-block">{_ Subscriptions require a source that supports updates and non-authoritative imports. _}</p>
                            {% else %}
                                <input type="hidden" name="is_subscribe" value="{% if feed.is_subscribe %}1{% else %}0{% endif %}">
                                <input type="hidden" name="is_subscribe_connections" value="{% if feed.is_subscribe_connections %}1{% else %}0{% endif %}">
                                <p class="help-block">{_ Enable mod_websub to subscribe imported resources for updates. _}</p>
                            {% endif %}
                        </fieldset>

                        <div class="form-group">
                            <label for="feed-remove-patterns">{_ Text to remove (regular expressions) _}</label>
                            <textarea id="feed-remove-patterns" name="remove_patterns" class="form-control" rows="4" maxlength="16384" spellcheck="false">{{ feed.remove_patterns|escape }}</textarea>
                            <p class="help-block">{_ One regexp per line. Matching text is removed from summaries and body text, including text across links. HTML formatting is preserved. Patterns are case-sensitive. _}</p>
                            <p class="help-block">
                                {_ Example: _}
                                <code>Het bericht .*? verscheen eerst op De Westkrant\.</code>
                            </p>
                        </div>

                        <div class="form-group">
                            <label for="feed-frequency">{_ Minimum polling interval (minutes) _}</label>
                            <input id="feed-frequency" class="form-control" name="poll_minutes" type="number" min="5" max="10080" value="{{ feed.poll_minutes }}" required>
                            <p class="help-block">{_ Publisher refresh hints and retries after failures can extend this interval. Successful fetches reset failure backoff. _}</p>
                        </div>

                        <div class="checkbox">
                            <label><input type="checkbox" name="import_all_pages" value="1" {% if feed.import_all_pages %}checked{% endif %}> {_ Import all pages on the initial import _}</label>
                            <p class="help-block">{_ Warning: this can import large amounts of articles and media. By default, only the first page is imported initially. _}</p>
                        </div>
                        <div class="checkbox">
                            <label>
                                <input id="feed-use-wordpress" type="checkbox" name="use_wordpress" value="1" {% if feed.use_wordpress %}checked{% endif %} {% if feed.is_zotonic_feed %}disabled{% endif %}>
                                {_ Enrich WordPress entries using the public REST API _}
                            </label>
                        </div>

                        <div class="checkbox">
                            <label>
                                <input type="checkbox" name="is_paused" value="1" {% if feed.is_paused %}checked{% endif %}>
                                {_ Pause automatic imports _}
                            </label>
                        </div>

                        <div class="modal-footer">
                            <button class="btn btn-primary" type="submit">{_ Save feed _}</button>
                            <button class="btn btn-default" type="submit" name="refetch_deleted" value="1">{_ Save & refetch deleted _}</button>
                        </div>
                    </form>

                </div>
            </section>

            {% if feed.id %}
                <section class="widget" aria-labelledby="feed-delete-heading">
                    <div class="widget-header" id="feed-delete-heading" role="heading" aria-level="3">
                        {_ Delete imported content _}
                    </div>
                    <div class="widget-content">
                        <p>{_ Delete imported articles and media that you are allowed to delete. Protected resources are kept. The feed will be paused; its settings and import history are retained. _}</p>
                        <div class="modal-footer">
                            {% button class="btn btn-danger" text=_"Delete imported content"
                                action={confirm is_danger
                                    title=_"Delete imported content?"
                                    text=_"This deletes all deletable articles and media imported by this feed, including resources shared with other feeds. Protected resources and resources you cannot delete are kept. The feed will be paused."
                                    ok=_"Delete imported content"
                                    postback={delete_content id=feed.id} delegate=`mod_import_feed`}
                            %}
                            {% button class="btn btn-danger" text=_"Delete feed"
                                postback={delete_feed_dialog id=feed.id} delegate=`mod_import_feed` %}
                        </div>
                    </div>
                </section>

            {% endif %}
        </div>
        <div class="col-md-6" id="feed-preview">
            {% include "_admin_import_feed_preview.tpl" %}
        </div>
    </div>
{% endif %}
