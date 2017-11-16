{#
Params:
id
cat_id
#}
<p class="text-muted">{_ Category name: _} {{ cat_id.name }}</p>

<h4>{_ About categories _}</h4>
<p>{_ Every page is categorized in exactly one category. The category defines what the page represents. For example an event, a product or a person. The categories are hierarchically defined. In that way you can have a vehicles category with subcategories car and bicycle. _}</p>

<div class="form-group">
    <h4>{_ More like this _}</h4>
    {% button class="btn btn-default" action={redirect dispatch="admin_overview_rsc" qcat=cat_id} text=_"View all pages from this category" %}
</div>

{% if id.is_editable %}
    {% wire id=#form type="submit" postback="rscform" action={dialog_close} delegate="controller_admin_edit" %}
    <form id="{{ #form }}" method="POST" action="postback" class="form">
        <fieldset class="form-horizontal">

            <input type="hidden" name="id" value="{{ id }}" />

            <div class="form-group row">
                <div class="col-md-12">
                    <h4>{_ Change the category of this page _}</h4>
                    {% include "_admin_category_dropdown.tpl" id=id cat_id=cat_id %}
                </div>
            </div>

            <div class="modal-footer clearfix">
                {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
                {% button class="btn btn-primary" text=_"Save" type="submit" %}
            </div>
        </fieldset>
    </form>
{% endif %}
