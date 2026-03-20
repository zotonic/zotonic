/* Support functions for the development tools */

$('body').on('click', '#graphviz_svg .node a', function(e) {
    e.preventDefault();

    const href = $(this).attr('xlink:href');
    if (href && href.startsWith('#')) {
        const template_path = href.substring(1);
        z_event("development_template_click", { template: template_path });
    }
});
