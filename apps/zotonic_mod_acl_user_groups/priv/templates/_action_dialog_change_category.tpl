<p>{_ Every page is categorized in exactly one category.  The category defines what the page represents. For example an event, a product or a person.  The categories are hierarchically defined. In that way you can have a vehicles category with subcategories car and bicycle._}</p>
<hr/>

{% wire id=#form type="submit" postback={change_catcg id=id error=#error} delegate=`admin_acl_rules_rsc` %}
<form id="{{ #form }}" method="POST" action="postback" class="form form-horizontal">

    {% include "_admin_catcg.tpl" cg_id=id.content_group_id cat_id=id.category_id form=#form error=#error %}

    <div class="modal-footer clearfix">
	    {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
	    {% button class="btn btn-primary" text=_"Save" type="submit" %}
    </div>

</form>
