{% wire id=#form type="submit" postback="rscform" action={dialog_close} delegate="resource_admin_edit" %}
<p>{_ Choose the new category for this page. _}</p>
<form id="{{ #form }}" method="POST" action="postback">

    <input type="hidden" name="id" value="{{ id }}" />

    <div class="form-item">
        {% include "_admin_category_dropdown.tpl" id=id %}
    </div>

	<div class="form-item clearfix">
		{% button text=_"Save" %}
		{% button action={dialog_close} text=_"Cancel" %}
	</div>
</form>

<hr />
<p>{_ Every page is categorized in exactly one category.  The category defines what the page represents. For example an event, a product or a person.  The categories are hierarchically defined. In that way you can have a vehicles category with subcategories car and bicycle._}</p>
