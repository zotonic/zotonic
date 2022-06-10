jQuery( document ).ready(function() {
    let surl, input, $form, $result;
    let search_view_busy = false;

    jQuery( '[name="qs"]' ).on('input', function() {
        if (!search_view_busy && this.value != last_search) {
            search_view_busy = true;
            $form = jQuery( this ).closest( 'form' );
            input = this;

            cotonic.broker.call("bridge/origin/model/template/get/render/search_view.tpl", { qs: this.value })
                .then(function( resp ) {
                    search_view_busy = false;
                    jQuery( input ).next().remove();
                    jQuery( input ).after( resp.payload.result );

                    $result = jQuery( '.search-view-results li' );

                    $result.on('click', function() {
                        jQuery( input ).next().remove();
                        input.value = jQuery( this ).find( 'span' ).text();
                        $form.submit();
                    });

                    $result.hover(function() {
                        $result.removeClass( 'active' );
                        jQuery( this ).toggleClass( 'active' );
                    });
          	});
        }
    });

    jQuery( this ).keydown(function(e) {
        if ((e.keyCode === 40 || e.keyCode === 38 || e.which == 13) && $result.length) {
            var $active = jQuery( '.search-view-results li.active' );

            if (e.keyCode === 40){ // pgdn
                if (!$active.next().length) {
                    $result.removeClass( 'active' );
                    jQuery( '.search-view-results li:first' ).toggleClass( 'active' );
                } else {
                    $active.toggleClass( 'active' );
                    $active.next().toggleClass( 'active' );
                }
            } else if (e.keyCode === 38) { // pgup
                if (!$active.prev().length) {
                    $result.removeClass( 'active' );
                    jQuery( '.search-view-results li:last' ).toggleClass( 'active' );
                } else {
                    $active.toggleClass( 'active' );
                    $active.prev().toggleClass( 'active' );
                }
            }

            $active = jQuery( '.search-view-results li.active' );

            if ($active && $active.length) input.value = $active.find( 'span' ).text();
        }
    });

    jQuery( 'body' ).on('click', function() {
         if ($form && $form.length) jQuery( input ).next().remove();
    });
});
