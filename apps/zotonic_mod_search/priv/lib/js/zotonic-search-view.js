jQuery( document ).ready(function() {
    var surl, input, $form, $result;

    jQuery( '[name="qs"]' ).on('input', function() {
        surl = window.location.protocol + '//' + window.location.host + '/search-view?qs=';
        $form = jQuery( this ).closest( 'form' );
        input = this;

        jQuery.ajax({
            type: 'GET',
            url: surl + this.value
      	}).done(function( data ) {
            jQuery( input ).next().remove();
            jQuery( input ).after( data );

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
