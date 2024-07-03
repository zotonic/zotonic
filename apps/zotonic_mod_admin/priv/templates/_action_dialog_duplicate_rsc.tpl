<p>{_ You are going to duplicate the page _}: <b>{{ m.rsc[id].title|default:m.rsc[id].short_title }}</b></p>
<p>{_ After the duplication you can edit the new page. The new page will unpublished. _}</p>

{% wire id=#form type="submit" postback={duplicate_page id=id} delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback" class="form form-horizontal">
    <div class="modal-footer">
	    {% button class="btn btn-default" action={dialog_close} text=_"Cancel" %}
	    {% button class="btn btn-primary" type="submit" text=_"Duplicate page" %}
    </div>
</form>
