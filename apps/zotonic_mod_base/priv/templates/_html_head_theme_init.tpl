{# Sets `document[data-zotonic-theme]` to the user-selected theme, and `document[data-bs-theme]`
 # to the resolved theme (which is either the user-selected theme or the system theme if the
 # user-selected theme is "auto")
 #}
<script nonce="{{ m.req.csp_nonce }}">
    (function() {
        let theme = "auto";
        try {
            theme = JSON.parse(localStorage.getItem("zotonic-theme")) || "auto";
        } catch (e) {
            theme = "auto";
        }
        const resolved = theme === "auto" && window.matchMedia
            ? (window.matchMedia("(prefers-color-scheme: dark)").matches ? "dark" : "light")
            : (theme === "dark" ? "dark" : "light");
        document.documentElement.setAttribute("data-bs-theme", resolved);
        document.documentElement.setAttribute("data-zotonic-theme", theme);
    })();
</script>
