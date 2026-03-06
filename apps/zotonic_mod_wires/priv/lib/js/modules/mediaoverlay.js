/* Media viewer using an overlay - opened via mod_wires notify observer. */

cotonic.ready.then(function () {
    $('body').on('click', "[data-mediaoverlay-id]", function (_event) {
        const id = $(this).data('mediaoverlay-id');
        let ids = [];
        $('body [data-mediaoverlay-id]').each(function () {
            ids.push($(this).data('mediaoverlay-id'));
        });
        z_notify("mediaoverlay", { id: id, ids: ids });
    });
});
