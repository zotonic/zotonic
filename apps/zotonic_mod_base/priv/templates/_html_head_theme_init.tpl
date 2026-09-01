{# Sets `document[data-zotonic-theme]` to the user-selected theme, and `document[data-bs-theme]`
 # to the resolved theme (which is either the user-selected theme or the system theme if the
 # user-selected theme is "auto")
 #}
<script nonce="{{ m.req.csp_nonce }}">
    (function() {
        {% if m.site.ui_theme as theme %}
            let theme = '{{ theme }}';
            try {
                if (JSON.parse(localStorage.getItem("zotonic-theme")) != theme) {
                    localStorage.setItem("zotonic-theme", JSON.stringify(theme));
                }
            } catch (e) {}
        {% else %}
            let theme = "auto";
            try {
                theme = JSON.parse(localStorage.getItem("zotonic-theme")) || "auto";
            } catch (e) {
                theme = "auto";
            }
        {% endif %}
        const resolved = theme === "auto" && window.matchMedia
            ? (window.matchMedia("(prefers-color-scheme: dark)").matches ? "dark" : "light")
            : (theme === "dark" ? "dark" : "light");
        document.documentElement.setAttribute("data-bs-theme", resolved);
        document.documentElement.setAttribute("data-zotonic-theme", theme);
    })();
</script>
