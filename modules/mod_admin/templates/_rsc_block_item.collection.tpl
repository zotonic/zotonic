{% live template="_rsc_item.tpl" topic=id id=id catinclude is_page_block %}

<hr/>

<strong>{{ m.rsc.haspart.title }}</strong>

<div id="{{ #undo_message }}"></div>

{% live template="_admin_edit_content_page_connections_list.tpl"
    topic={object id=id predicate=`haspart`}
    id=id
    predicate=`haspart`
    button_label=button_label
    button_class=button_class
    dialog_title_add=dialog_title_add
    callback=callback
    action=action
    unlink_action=unlink_action
    undo_message_id=#undo_message
    list_id=list_id
    is_editable=id.is_editable
%}

<hr />
