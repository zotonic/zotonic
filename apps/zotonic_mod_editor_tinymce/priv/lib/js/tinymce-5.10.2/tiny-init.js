if (typeof tinyInit !== 'object')
  tinyInit = {
    selector: "textarea",

    // mode: "none",
    // theme: "modern",

    plugins: "advlist code paste table link zlink zmedia autosave directionality autoresize lists fullscreen searchreplace codesample",
    menubar: "edit format table tools insert",
    toolbar: [
        "styleselect | bold italic | alignleft aligncenter alignright | bullist numlist outdent indent | ltr rtl | removeformat",
        "link unlink | zlink zmedia | code | searchreplace | fullscreen"
    ],

    contextmenu: "zlink zmedia link",

    style_formats: [
        { title: 'Headers', items: [
          { title: 'h1', block: 'h1' },
          { title: 'h2', block: 'h2' },
          { title: 'h3', block: 'h3' },
          { title: 'h4', block: 'h4' },
          { title: 'h5', block: 'h5' },
          { title: 'h6', block: 'h6' }
        ] },

        { title: 'Blocks', items: [
          { title: 'p', block: 'p' },
          { title: 'div', block: 'div' },
          { title: 'pre', block: 'pre' },
          // { title: 'DL', cmd: 'InsertDefinitionList' }
        ] },

        { title: 'Containers', items: [
          { title: 'section', block: 'section', wrapper: true, merge_siblings: false },
          // { title: 'article', block: 'article', wrapper: true, merge_siblings: false },
          { title: 'blockquote', block: 'blockquote', wrapper: true },
          // { title: 'hgroup', block: 'hgroup', wrapper: true },
          // { title: 'figure', block: 'figure', wrapper: true },
          { title: 'aside', block: 'aside', wrapper: true }
        ] }
    ],

    codesample_languages: [
        { text: 'HTML/XML', value: 'markup' },
        { text: 'JavaScript', value: 'javascript' },
        { text: 'CSS', value: 'css' },
        { text: 'Erlang', value: 'erlang' },
        { text: 'Django', value: 'django' },
        { text: 'PHP', value: 'php' },
        { text: 'Ruby', value: 'ruby' },
        { text: 'Python', value: 'python' },
        { text: 'Java', value: 'java' },
        { text: 'C', value: 'c' },
        { text: 'C#', value: 'csharp' },
        { text: 'C++', value: 'cpp' }
    ],

    content_css: "/lib/css/tinymce-zotonic~2.css",
    content_style: "",

    // language : "en", // set in _admin_tinymce_overrides_js.tpl

    // /* Adapted valid element list, added some html5 elements, removed controls, object, embed etc */
    // /* See: http://www.tinymce.com/wiki.php/Configuration:valid_elements */
    valid_elements : "@[class|style|title|dir<ltr?rtl|lang|xml::lang],"
    + "a[rel|rev|charset|hreflang|tabindex|accesskey|type|name|href|target|title],"
    + "strong/b,em/i,strike,u,"
    + "#p,-ol[type|compact],-ul[type|compact],-li,br,img[longdesc|usemap|"
    + "src|border|alt=|title|hspace|vspace|width|height|align],-sub,-sup,"
    + "-blockquote,-table[border=0|cellspacing|cellpadding|width|frame|rules|"
    + "height|align|summary|bgcolor|background|bordercolor],-tr[rowspan|width|"
    + "height|align|valign|bgcolor|background|bordercolor],tbody,thead,tfoot,"
    + "#td[colspan|rowspan|width|height|align|valign|bgcolor|background|bordercolor|scope],"
    + "#th[colspan|rowspan|width|height|align|valign|scope],caption,-div,"
    + "-span,-code,-pre,address,-h1,-h2,-h3,-h4,-h5,-h6,hr[size|noshade],"
    + "dd,dl,dt,cite,abbr,del[datetime|cite],ins[datetime|cite],"
    + "map[name],area[shape|coords|href|alt|target],bdo,"
    + "col[align|char|charoff|span|valign|width],colgroup[align|char|charoff|span|valign|width],"
    + "dfn,kbd,legend,"
    + "q[cite],samp,small,"
    + "tt,var,big,"
    + "article,section,aside,audio,video",

    relative_urls: "",
    remove_script_host: "",
    convert_urls: "",
    apply_source_formatting: "",
    remove_linebreaks: "1",

    formats : {
        bdo_rtl : {inline : 'bdo', attributes : {dir: 'rtl'}},
        bdo_ltr : {inline : 'bdo', attributes : {dir: 'ltr'}}
    },

    // setup: function(editor) {
    //     // setup code here; override in _admin_tinymce_overrides_js.tpl
    // },

    // /* Cleanup pasted html code */
    paste_auto_cleanup_on_paste : true,
    paste_convert_middot_lists: true,
    paste_remove_spans: true,
    paste_remove_styles: true,
    paste_remove_styles_if_webkit: true,
    paste_strip_class_attributes: true,

    // /* below is a workaround for problem where tinyMCE setEntities skips the ones below and the doesn't initialize the entityLookup array
    //  * which results in an error in the _encode function.
    //  */
    // entity_encoding: "raw",
    // entities: "38,amp,60,lt,62,gt",

    end_container_on_empty_block: true,
    accessibility_focus: "1",
    wpeditimage_disable_captions: "",
    table_row_limit: 100,
    table_col_limit: 10,
    autoresize_max_height: 400
};
