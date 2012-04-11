<div id="edit-blocks-wrapper">
    {% include "_admin_edit_block_addblock.tpl" %}
    <ul class="blocks ui-sortable" id="edit-blocks">
    {% for blk in id.blocks %}
        {% include "_admin_edit_block_li.tpl" %}
    {% endfor %}
    </ul>
</div>

{% wire action={script script=["
        $('#edit-blocks').sortable({ 
            helper: 'clone',
            handle: '.widget-header',
            revert: 'invalid',
            axis: 'y',
            start: function(event, ui) {
                z_tinymce_save($(this));
                z_tinymce_remove($(this));
            },
            stop: function(event, ui) {
                z_tinymce_add($(this));
            }
        })
        .on('click', '.icon-remove', function() { 
            var block = $(this).closest('li');
            z_dialog_confirm({
                title: '",_"Confirm block removal","',
                text: '<p>",_"Do you want to remove this block?","</p>',
                cancel: '",_"Cancel","',
                ok: '",_"Delete","',
                on_confirm: function() { 
                                $(block).fadeTo('fast', 0.0)
                                        .slideUp('normal', 0.0, 
                                         function() { 
                                            z_tinymce_remove($(this)); 
                                            $(this).remove(); 
                                        });
                            }
            })
        });
        
        $('#edit-blocks-wrapper').on('click', '.block-add-block .dropdown-menu a', function(event) {
            var block_type = $(this).data('block-type'); 
            var after_block = $(this).closest('li.block').attr('id');
            var langs = '';
            
            $('input[name=language]:checked').each(function() { langs += ',' + $(this).val(); });
            
            z_notify('admin-insert-block', { 
                        z_delegate: 'mod_admin', 
                        type: block_type, 
                        after: after_block, 
                        rsc_id: ",id|make_list,", 
                        language: langs,
                        edit_language: $('.language-tabs .active').attr('lang')
                    });
            event.preventDefault();
        });
    "]
    }
%}
