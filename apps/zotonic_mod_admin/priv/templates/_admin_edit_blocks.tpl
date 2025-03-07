{% block blocks_before %}{% endblock %}

{% with blocks|if_undefined:(m.admin_blocks.list[id]) as blocks %}
<div id="edit-blocks-wrapper">
    <input type="hidden" name="blocks" value="">
    {% include "_admin_edit_block_addblock.tpl" %}
    <ul class="blocks ui-sortable" id="edit-blocks">
        {% block blocks %}
            {% for blk in id.blocks|default:[] %}
                {% include "_admin_edit_block_li.tpl" %}
            {% endfor %}
        {% endblock %}
    </ul>
</div>
{% endwith %}

{% block blocks_after %}{% endblock %}

{% javascript %}
$('#edit-blocks').sortable({
    helper: 'clone',
    handle: '.drag-handle',
    revert: 'invalid',
    axis: 'y',
    start: function(event, ui) {
        z_editor_save($(this));
        z_editor_remove($(this));
    },
    stop: function(event, ui) {
        z_editor_add($(this));
        $(this).closest('form').trigger('change');
    }
})
.on('click', '.block-remove', function(event) {
    event.stopPropagation();
    var $block = $(this).closest('li');
    z_dialog_confirm({
        title: '{_ Confirm removal _}',
        text: '<p>{_ Do you want to remove this page block? _}</p>',
        cancel: '{_ Cancel _}',
        ok: '{_ Delete _}',
        is_danger: true,
        on_confirm: function() {
            $block
                .fadeTo('fast', 0.0)
                .slideUp('normal', 0.0, function() {
                    $(this).closest('form').trigger('change');
                    z_editor_remove($(this));
                    $(this).remove();
                });
        }
    })
})
.on('click', '.block-down', function(event) {
    event.stopPropagation();
    let $block = $(this).closest('li');
    let top = $block.position().top;
    z_editor_save($block);
    z_editor_remove($block);
    $block.next('li').after($block);
    $block.find(".widget").effect("highlight");
    window.scrollBy(0, $block.position().top - top);
    z_editor_add($block);
    $block.closest('form').trigger('change');
})
.on('click', '.block-up', function(event) {
    event.stopPropagation();
    let $block = $(this).closest('li');
    let top = $block.position().top;
    z_editor_save($block);
    z_editor_remove($block);
    $block.prev('li').before($block);
    $block.find(".widget").effect("highlight");
    window.scrollBy(0, $block.position().top - top);
    z_editor_add($block);
    $block.closest('form').trigger('change');
});


$('#edit-blocks-wrapper').on('click', '.block-add-block .dropdown-menu a', function(event) {
    var $this = $(this),
        block_type = $this.data('block-type'),
        after_block = $(this).closest('li.block').attr('id');
        langs = '';
    $('input[name="language[]"]:checked').each(function() { langs += ',' + $(this).val(); });

    z_notify('admin-insert-block', {
                z_delegate: 'mod_admin',
                type: block_type,
                after: after_block,
                {% if id %}
                    rsc_id: {{ id }},
                {% endif %}
                language: langs,
                edit_language: $('.language-tabs .active').attr('lang')
            });
    event.preventDefault();
});


$('#edit-blocks-wrapper').on('click', '.block-page a.page-connect', function(event) {
    window.zBlockConnectTrigger = this;
    z_event("admin-block-connect", {});
    event.preventDefault();
});

$('#edit-blocks-wrapper').on('click', '.block-page a.page-disconnect', function(event) {
    window.zBlockConnectTrigger = this;
    window.zAdminBlockConnectDone({object_id: '', predicate: '', subject_id: '', title: '', title_language: '', url_language: '' });
    event.preventDefault();
});

window.zAdminBlockConnectDone = function(v) {
    var $block_page = $(window.zBlockConnectTrigger).closest(".block-page");
    var target_id = $(".rsc-item-wrapper", $block_page).attr('id');
    $("input[type=hidden]", $block_page).val(v.object_id);
    z_notify("update", {z_delegate: 'mod_admin', id: v.object_id, z_target_id: target_id});
}

$('#edit-blocks-wrapper').on('click', '.rsc-item h5 a', function(event) {
    var rsc_id = $(this).attr('href').replace('#', '');
    z_event("admin-edit-basics", {
                        id: rsc_id,
                        element_id: $(this).closest(".rsc-item").attr('id'),
                        template: "_rsc_item.tpl",
                        edit_dispatch: "{{ edit_dispatch }}"
                });
    event.preventDefault();
});

{% endjavascript %}

{% wire name="admin-block-connect"
    action={dialog_open
        intent="select"
        subject_id=id
        predicate=""
        template="_action_dialog_connect.tpl"
        title=_"Find page"
        callback="window.zAdminBlockConnectDone"
        center=0
        autoclose
        is_zlink
        width="large"
    }
%}

{% wire name="admin-edit-basics" action={dialog_edit_basics template="_rsc_item.tpl"} %}

