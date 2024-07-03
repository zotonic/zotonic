{# Logic for the bulk actions on selected rows.
 # Used for overview list of pages and media.
 #}

{% wire name="csel-delete"
        action={dialog_open
            title=_"Delete selected pages"
            template="_dialog_admin_bulk_delete.tpl"
            on_success=[
                {reload}
            ]
        }
%}

{% wire name="csel-update"
        action={dialog_open
            title=_"Update selected pages"
            template="_dialog_admin_bulk_update.tpl"
            on_success=[
                {reload}
            ]
        }
%}

{% javascript %}
    function collect_ids()
    {
        let selected = [];

        $('tbody input:checked:visible').each(function() {
            selected.push($(this).val());
        });
        return selected;
    }

    $('#csel-delete').on('click', function() {
        const ids = collect_ids();
        if (ids.length == 0) {
            z_dialog_alert({
                title: "{_ No pages selected _}",
                text: "<p>{_ Check the pages you want to delete. _}</p>"
            });
        } else {
            z_event("csel-delete", { ids: ids });
        }
    });

    $('#csel-update').on('click', function() {
        const ids = collect_ids();
        if (ids.length == 0) {
            z_dialog_alert({
                title: "{_ No pages selected _}",
                text: "<p>{_ Check the pages you want to update. _}</p>"
            });
        } else {
            z_event("csel-update", { ids: ids });
        }
    });

    $('#check-all').on('click', function() {
        const tab = $(this).closest('table');
        const csels = $(tab).find('input[name=csel]');
        if ($(this).is(":checked")) {
            csels.prop("checked", true);
            $('#sel-count').text(csels.length);
        } else {
            csels.prop("checked", false);
            $('#sel-count').text('0');
        }
    });

    $('input[name=csel]').on('click', function() {
        const tab = $(this).closest('table');
        const csels = $(tab).find('input[name=csel]');
        if (csels.not(':checked').length > 0) {
            $('#check-all').prop("checked", false);
        } else {
            $('#check-all').prop("checked", true);
        }
        $('#sel-count').text(csels.filter(":checked").length);
    });
{% endjavascript %}

