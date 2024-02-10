<form id="{{ #update_all }}" action="{% url (dispatch) %}" method="GET" class="form-horizontal">

<div class="row">

    <div class="col-sm-4">
        {% with m.rsc[qargs.qquery_id].id as query_id %}
        {% with m.search.query::%{
                            cat: 'admin_content_query'
                            sort: 'pivot_title'
                            pagelen: 100
                        }
            as admin_queries
        %}
        {% if admin_queries or query_id %}
            <div class="form-group row">
                <label class="col-sm-3 control-label">{_ Query _}</label>
                <div class="col-sm-9">
                    <select name="qquery_id" class="form-select">
                        <option value=""></option>
                        {% if query_id and not query_id|member:admin_queries %}
                            <option value="{{ query_id }}" selected>
                                {{ query_id.title }}
                            </option>
                        {% endif %}
                        {% for id in admin_queries %}
                            <option value="{{ id }}" {% if id == qargs.qquery_id %}selected{% endif %}>
                                {{ id.title }}
                            </option>
                        {% endfor %}
                    </select>
                    <div class="form-check">
                        <input type="checkbox" name="qquery_id" id="qquery_id" class="form-check-input" value="{{ query_id }}" checked>
                        <label for="qquery_id" class="form-check-label">{{ query_id.title }}</label>
                    </div>
                </div>
            </div>
        {% endif %}
        {% endwith %}
        {% endwith %}

        <div class="form-group row">
            <label class="col-sm-3 control-label">{_ Text _}</label>
            <div class="col-sm-9">
                <input type="text" name="qs" class="form-control" value="{{ qargs.qs|escape }}">
            </div>
        </div>

        <div class="form-group row">
            <label class="col-sm-3 control-label">{_ Published _}</label>
            <div class="col-sm-4">
                <select name="qis_published" class="form-select col-md-8">
                    <option value=""></option>
                    <option value="1">{_ Published _}</option>
                    <option value="0">{_ Unpublished _}</option>
                </select>
            </div>
        </div>

        {% block category %}
            {% with m.rsc[qargs.qcat].id as cat_id %}
                <div class="form-group row">
                    <label class="col-sm-3 control-label">{_ Category _}</label>
                    <div class="col-sm-9">
                        <select name="qcat" class="form-select">
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
                <div class="form-group row">
                    <label class="col-sm-3 control-label">{_ Exact category _}</label>
                    <div class="col-sm-9">
                        <select name="qcat_exact" class="form-select">
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
                <div class="form-group row">
                    <label class="col-sm-3 control-label">{_ Exclude category _}</label>
                    <div class="col-sm-9">
                        <select name="qcat_exclude" class="form-select">
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
    </div>

    <div class="col-sm-4">
        {% block properties %}
        {% endblock %}

        {% with m.search.all_bytitle::%{ cat: 'predicate' } as predicates %}
            {% with qargs.qhasobjectpredicate as qpred %}
            <div class="form-group row">
                <label class="col-sm-3 control-label">{_ Has connections with predicate _}</label>
                <div class="col-sm-9">
                    <select name="qhasobjectpredicate" class="form-select col-md-8">
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
            <div class="form-group row">
                <label class="col-sm-3 control-label">{_ Is connected with predicate _}</label>
                <div class="col-sm-9">
                    <select name="qhassubjectpredicate" class="form-select">
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

        <div class="form-group row">
            <label class="col-sm-3 control-label">{_ Date start/end _}</label>
            <div class="col-sm-9">
                <div class="form-check">
                    <input type="checkbox" name="qupcoming" id="qupcoming" class="form-check-input" value="1" {% if q.qupcoming %}checked{% endif %}>
                    <label for="qupcoming" class="form-check-label">{_ Upcoming _}</label>
                </div>
                <div class="form-check">
                    <input type="checkbox" name="qongoing" id="qongoing" class="form-check-input" value="1" {% if q.qongoing %}checked{% endif %}>
                    <label for="qongoing" class="form-check-label">{_ Ongoing _}</label>
                </div>
                <div class="form-check">
                    <input type="checkbox" name="qfinished" id="qfinished" class="form-check-input" value="1" {% if q.qfinished %}checked{% endif %}>
                    <label for="qfinished" class="form-check-label">{_ Finished _}</label>
                </div>
            </div>
        </div>

        <div class="form-group row">
            <label class="col-sm-3 control-label">{_ Has medium attached _}</label>
            <div class="col-sm-4">
                <select name="qhasmedium" class="form-select">
                    <option value=""></option>
                    <option value="1"{% if qargs.qhasmedium %}selected{% endif %}>{_ Has medium _}</option>
                    <option value="0" {% if qargs.qhasmedium == '0' %}selected{% endif %}>{_ No medium _}</option>
                </select>
            </div>
        </div>
    </div>

    <div class="col-sm-4">
        <div class="form-group row">
            <label class="col-sm-3 control-label">{_ Featured _}</label>
            <div class="col-sm-4">
                <select name="qis_featured" class="form-select">
                    <option value=""></option>
                    <option value="1"{% if qargs.qis_featured %}selected{% endif %}>{_ Featured _}</option>
                    <option value="0" {% if qargs.qis_featured == '0' %}selected{% endif %}>{_ Not featured _}</option>
                </select>
            </div>
        </div>

        <div class="form-group row">
            <label class="col-sm-3 control-label">{_ Findable _}</label>
            <div class="col-sm-4">
                <select name="qis_findable" class="form-select">
                    <option value=""></option>
                    <option value="1"{% if qargs.qis_findable %}selected{% endif %}>{_ Findable _}</option>
                    <option value="0" {% if qargs.qis_findable == '0' %}selected{% endif %}>{_ Not findable _}</option>
                </select>
            </div>
        </div>

        <div class="form-group row">
            <label class="col-sm-3 control-label">{_ Protected _}</label>
            <div class="col-sm-4">
                <select name="qis_protected" class="form-select">
                    <option value=""></option>
                    <option value="1"{% if qargs.qis_protected %}selected{% endif %}>{_ Protected _}</option>
                    <option value="0" {% if qargs.qis_protected == '0' %}selected{% endif %}>{_ Not protected _}</option>
                </select>
            </div>
        </div>

        <div class="form-group row">
            <label class="col-sm-3 control-label">{_ Dependent _}</label>
            <div class="col-sm-4">
                <select name="qis_dependent" class="form-select">
                    <option value=""></option>
                    <option value="1" {% if qargs.qis_dependent %}selected{% endif %}>{_ Dependent _}</option>
                    <option value="0" {% if qargs.qis_dependent == '0' %}selected{% endif %}>{_ Not dependent _}</option>
                </select>
            </div>
        </div>

        <div class="form-group row">
            <label class="col-sm-3 control-label">{_ Authoritative _}</label>
            <div class="col-sm-4">
                <select name="qis_authoritative" class="form-select">
                    <option value=""></option>
                    <option value="1" {% if qargs.qis_authoritative %}selected{% endif %}>{_ Local content _}</option>
                    <option value="0" {% if qargs.qis_authoritative == '0' %}selected{% endif %}>{_ Imported content _}</option>
                </select>
            </div>
        </div>

        <div class="form-group row">
            <label class="col-sm-3 control-label">{_ Unique name _}</label>
            <div class="col-sm-9">
                <input type="text" name="qname" class="form-control" value="{{ qargs.qname|escape }}">
            </div>
        </div>

        {% with qargs.qsort as qsort %}
            <div class="form-group row">
                <label class="col-sm-3 control-label">{_ Sort by _}</label>
                <div class="col-sm-9">
                    <select name="qsort" class="form-select">
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
    </div>
</div>

<div class="modal-footer">
    {% if on_cancel %}
        {% button class="btn btn-outline-secondary" action=on_cancel text=_"Cancel" %}
    {% endif %}
    {% button class="btn btn-primary" type="submit" text=_"Search" %}
</div>

</form>
