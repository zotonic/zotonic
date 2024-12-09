<form id="{{ #update_all }}" action="{% url (dispatch) %}" method="GET" class="form-horizontal">

<div class="row">

    <div class="col-sm-4">
        {% with m.rsc[qargs.qquery_id].id as query_id %}
        {% with m.search.query::%{
                            cat: 'admin_content_query'
                            sort: [
                                '-is_featured',
                                'pivot_title'
                            ],
                            pagelen: 100
                        }
            as admin_queries
        %}
        {% if admin_queries or query_id %}
            <div class="form-group">
                <label class="col-sm-3 control-label">{_ Query _}</label>
                <div class="col-sm-9">
                    <select name="qquery_id" class="form-control">
                        <option value=""></option>
                        {% if query_id and not query_id|member:admin_queries %}
                            <option value="{{ query_id }}" selected>
                                {{ query_id.title }}
                            </option>
                        {% endif %}
                        {% for id in admin_queries %}
                            <option value="{{ id }}" {% if id == query_id %}selected{% endif %}>
                                {{ id.title }}
                            </option>
                        {% endfor %}
                    </select>
                </div>
            </div>
        {% endif %}
        {% endwith %}
        {% endwith %}

        <div class="form-group">
            <label class="col-sm-3 control-label">{_ Text _}</label>
            <div class="col-sm-9">
                <input type="text" name="qs" class="form-control" value="{{ qargs.qs|escape }}">
            </div>
        </div>

        <div class="form-group">
            <label class="col-sm-3 control-label">{_ Published _}</label>
            <div class="col-sm-4">
                <select name="qis_published" class="form-control col-md-8">
                    <option value=""></option>
                    <option value="1" {% if q.qis_published %}selected{% endif %}>{_ Published _}</option>
                    <option value="0" {% if q.qis_published == "0" %}selected{% endif %}>{_ Unpublished _}</option>
                </select>
            </div>
        </div>

        {% block meta %}
        {% endblock %}

        {% block category %}
            {% with m.rsc[qargs.qcat].id as cat_id %}
                <div class="form-group">
                    <label class="col-sm-3 control-label">{_ Category _}</label>
                    <div class="col-sm-9">
                        <select name="qcat" class="form-control">
                            <option value=""></option>
                            {% for c in m.category.tree_flat %}
                                {% if m.acl.insert[c.id.name|as_atom] %}
                                    <option value="{{ c.id }}" {% if c.id == cat_id %}selected{% endif %}>
                                        {{ c.indent }}{{ c.id.title|default:c.id.name }}
                                    </option>
                                {% endif %}
                            {% endfor %}
                        </select>
                    </div>
                </div>
            {% endwith %}

            {% with m.rsc[qargs.qcat_exact].id as cat_id %}
                <div class="form-group">
                    <label class="col-sm-3 control-label">{_ Exact category _}</label>
                    <div class="col-sm-9">
                        <select name="qcat_exact" class="form-control">
                            <option value=""></option>
                            {% for c in m.category.tree_flat %}
                                {% if m.acl.insert[c.id.name|as_atom] %}
                                    <option value="{{ c.id }}" {% if c.id == cat_id %}selected{% endif %}>
                                        {{ c.indent }}{{ c.id.title|default:c.id.name }}
                                    </option>
                                {% endif %}
                            {% endfor %}
                        </select>
                    </div>
                </div>
            {% endwith %}

            {% with m.rsc[qargs.qcat_exclude].id as cat_id %}
                <div class="form-group">
                    <label class="col-sm-3 control-label">{_ Exclude category _}</label>
                    <div class="col-sm-9">
                        <select name="qcat_exclude" class="form-control">
                            <option value=""></option>
                            {% for c in m.category.tree_flat %}
                                {% if m.acl.insert[c.id.name|as_atom] %}
                                    <option value="{{ c.id }}" {% if c.id == cat_id %}selected{% endif %}>
                                        {{ c.indent }}{{ c.id.title|default:c.id.name }}
                                    </option>
                                {% endif %}
                            {% endfor %}
                        </select>
                    </div>
                </div>
            {% endwith %}
        {% endblock %}

        {% block content_group %}
        {% endblock %}

        {% block meta_after %}
        {% endblock %}
    </div>

    <div class="col-sm-4">
        {% block properties %}
        {% endblock %}

        {% with m.search.all_bytitle::%{ cat: 'predicate' } as predicates %}
            {% with qargs.qhasobjectpredicate as qpred %}
            <div class="form-group">
                <label class="col-sm-3 control-label">{_ Has connections with predicate _}</label>
                <div class="col-sm-9">
                    <select name="qhasobjectpredicate" class="form-control col-md-8">
                        <option value=""></option>
                        {% for title, id in predicates %}
                            <option value="{{ id }}" {% if id == qpred %}selected{% endif %}>
                                {{ title }}
                            </option>
                        {% endfor %}
                    </select>
                </div>
            </div>
            {% endwith %}

            {% with qargs.qhassubjectpredicate as qpred %}
            <div class="form-group">
                <label class="col-sm-3 control-label">{_ Is connected with predicate _}</label>
                <div class="col-sm-9">
                    <select name="qhassubjectpredicate" class="form-control">
                        <option value=""></option>
                        {% for title, id in predicates %}
                            <option value="{{ id }}" {% if id == qpred %}selected{% endif %}>
                                {{ title }}
                            </option>
                        {% endfor %}
                    </select>
                </div>
            </div>
            {% endwith %}
        {% endwith %}

        <div class="form-group">
            <label class="col-sm-3 control-label">{_ Date start/end _}</label>
            <div class="col-sm-9">
                <label class="checkbox">
                    <input type="checkbox" name="qupcoming" value="1" {% if q.qupcoming %}checked{% endif %}>
                    {_ Upcoming _}
                </label>
                <label class="checkbox">
                    <input type="checkbox" name="qongoing" value="1" {% if q.qongoing %}checked{% endif %}>
                    {_ Ongoing _}
                </label>
                <label class="checkbox">
                    <input type="checkbox" name="qfinished" value="1" {% if q.qfinished %}checked{% endif %}>
                    {_ Finished _}
                </label>
            </div>
        </div>

        <div class="form-group">
            <label class="col-sm-3 control-label">{_ Has medium attached _}</label>
            <div class="col-sm-4">
                <select name="qhasmedium" class="form-control">
                    <option value=""></option>
                    <option value="1"{% if qargs.qhasmedium %}selected{% endif %}>{_ Has medium _}</option>
                    <option value="0" {% if qargs.qhasmedium == '0' %}selected{% endif %}>{_ No medium _}</option>
                </select>
            </div>
        </div>

        {% block properties_after %}
        {% endblock %}
    </div>

    <div class="col-sm-4">
        {% block flags %}
        {% endblock %}

        <div class="form-group">
            <label class="col-sm-3 control-label">{_ Featured _}</label>
            <div class="col-sm-4">
                <select name="qis_featured" class="form-control">
                    <option value=""></option>
                    <option value="1"{% if qargs.qis_featured %}selected{% endif %}>{_ Featured _}</option>
                    <option value="0" {% if qargs.qis_featured == '0' %}selected{% endif %}>{_ Not featured _}</option>
                </select>
            </div>
        </div>

        <div class="form-group">
            <label class="col-sm-3 control-label">{_ Findable _}</label>
            <div class="col-sm-4">
                <select name="qis_findable" class="form-control">
                    <option value=""></option>
                    <option value="1"{% if qargs.qis_findable %}selected{% endif %}>{_ Findable _}</option>
                    <option value="0" {% if qargs.qis_findable == '0' %}selected{% endif %}>{_ Not findable _}</option>
                </select>
            </div>
        </div>

        <div class="form-group">
            <label class="col-sm-3 control-label">{_ Protected _}</label>
            <div class="col-sm-4">
                <select name="qis_protected" class="form-control">
                    <option value=""></option>
                    <option value="1"{% if qargs.qis_protected %}selected{% endif %}>{_ Protected _}</option>
                    <option value="0" {% if qargs.qis_protected == '0' %}selected{% endif %}>{_ Not protected _}</option>
                </select>
            </div>
        </div>

        <div class="form-group">
            <label class="col-sm-3 control-label">{_ Dependent _}</label>
            <div class="col-sm-4">
                <select name="qis_dependent" class="form-control">
                    <option value=""></option>
                    <option value="1" {% if qargs.qis_dependent %}selected{% endif %}>{_ Dependent _}</option>
                    <option value="0" {% if qargs.qis_dependent == '0' %}selected{% endif %}>{_ Not dependent _}</option>
                </select>
            </div>
        </div>

        <div class="form-group">
            <label class="col-sm-3 control-label">{_ Authoritative _}</label>
            <div class="col-sm-4">
                <select name="qis_authoritative" class="form-control">
                    <option value=""></option>
                    <option value="1" {% if qargs.qis_authoritative %}selected{% endif %}>{_ Local content _}</option>
                    <option value="0" {% if qargs.qis_authoritative == '0' %}selected{% endif %}>{_ Imported content _}</option>
                </select>
            </div>
        </div>

        {% block flags_after %}
        {% endblock %}

        <div class="form-group">
            <label class="col-sm-3 control-label">{_ Unique name _}</label>
            <div class="col-sm-9">
                <input type="text" name="qname" class="form-control" value="{{ qargs.qname|escape }}">
            </div>
        </div>

        {% block sort %}
            {% with qargs.qsort as qsort %}
                <div class="form-group">
                    <label class="col-sm-3 control-label">{_ Sort by _}</label>
                    <div class="col-sm-9">
                        <select name="qsort" class="form-control">
                            <option value=""></option>
                            {% for name, title in [
                                    [ "-created", _"Creation date, newest first" ],
                                    [ "created", _"Creation date, oldest first" ],
                                    [ "-modified", _"Modification date, newest first" ],
                                    [ "modified", _"Modification date, oldest first" ],
                                    [ "-publication_start", _"Published on, newest first" ],
                                    [ "publication_start", _"Published on, oldest first" ],
                                    [ "-publication_end", _"Published ends, newest first" ],
                                    [ "publication_end", _"Published ends, oldest first" ]
                                ]
                            %}
                                <option value="{{ name }}" {% if qsort == name %}selected{% endif %}>
                                    {{ title }}
                                </option>
                            {% endfor %}
                            {% block sort_options %}
                            {% endblock %}
                        </select>
                    </div>
                </div>
            {% endwith %}
        {% endblock %}

        {% block sort_after %}
        {% endblock %}
    </div>
</div>

<div class="modal-footer">
    {% if on_cancel %}
        {% button class="btn btn-default" action=on_cancel text=_"Cancel" %}
    {% endif %}
    {% button class="btn btn-primary" type="submit" text=_"Search" %}
</div>

</form>
