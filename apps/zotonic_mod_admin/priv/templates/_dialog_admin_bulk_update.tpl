<p>
    {% trans "Update {n} pages with the following properties:" n=q.ids|length %}
</p>

{% wire id=#update_all
        type="submit"
        postback={update_all
                ids=q.ids
                on_success=on_success
                on_success={dialog_close}
        }
        delegate=`mod_admin`
%}
<form id="{{ #update_all }}" action="postback" class="form-horizontal">

    <div class="form-group">
        <label class="col-sm-3 control-label">{_ Published _}</label>
        <div class="col-sm-4">
            <select name="is_published" class="form-control col-md-8">
                <option value=""></option>
                <option value="1">{_ Published _}</option>
                <option value="0">{_ Unpublished _}</option>
            </select>
        </div>
    </div>

    {% block category %}
        <div class="form-group">
            <label class="col-sm-3 control-label">{_ Category _}</label>
            <div class="col-sm-9">
                <select id="category_id" name="category_id" class="col-lg-4 col-md-4 form-control">
                    <option value=""></option>
                    {% for c in m.category.tree_flat %}
                        {% if m.acl.insert[c.id.name|as_atom] %}
                            <option value="{{ c.id }}">
                                {{ c.indent }}{{ c.id.title|default:c.id.name }}
                            </option>
                        {% endif %}
                    {% endfor %}
                </select>
            </div>
        </div>
    {% endblock %}

    {% block content_group %}
    {% endblock %}

    {% block properties %}
    {% endblock %}

    <div class="form-group">
        <label class="col-sm-3 control-label">{_ Protected _}</label>
        <div class="col-sm-4">
            <select name="is_protected" class="form-control col-md-8">
                <option value=""></option>
                <option value="1">{_ Protected _}</option>
                <option value="0">{_ Not protected _}</option>
            </select>
        </div>
    </div>

    <div class="form-group">
        <label class="col-sm-3 control-label">{_ Dependent _}</label>
        <div class="col-sm-4">
            <select name="is_dependent" class="form-control col-md-8">
                <option value=""></option>
                <option value="1">{_ Dependent _}</option>
                <option value="0">{_ Not dependent _}</option>
            </select>
        </div>
    </div>

    <div class="form-group">
        <label class="col-sm-3 control-label">{_ SEO _}</label>
        <div class="col-sm-4">
            <select name="seo_noindex" class="form-control col-md-8">
                <option value=""></option>
                <option value="0">{_ Indexed by search engines _}</option>
                <option value="1">{_ Not indexed by search engines _}</option>
            </select>
        </div>
    </div>

    <div class="modal-footer">
        {% button class="btn btn-default" action={dialog_close} text=_"Cancel" %}
        {% button class="btn btn-primary" type="submit" text=_"Update" %}
    </div>

</form>
