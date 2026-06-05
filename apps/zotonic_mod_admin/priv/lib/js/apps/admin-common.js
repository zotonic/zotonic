/* Admin Common js
----------------------------------------------------------

@package:   Zotonic 2009
@Author:    Tim Benniks <tim@timbenniks.nl>

Copyright 2009 Tim Benniks
Copyright 2012 Arjan Scherpenisse

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
 
http://www.apache.org/licenses/LICENSE-2.0
 
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

---------------------------------------------------------- */

(function($)
{
    // Theme selection. Set the selected light/dark theme on the document[data-bs-theme] attribute.
    // The selected theme is stored in the localStorage
    const themeStorageKey = "zotonic-theme";
    const themeValues = [ "light", "dark", "auto" ];
    const themeMedia = window.matchMedia
        ? window.matchMedia("(prefers-color-scheme: dark)")
        : undefined;

    const themePreference = () => {
        let value = "auto";
        try {
            value = JSON.parse(localStorage.getItem(themeStorageKey)) || "auto";
        } catch (e) {
            value = "auto";
        }
        return themeValues.indexOf(value) === -1 ? "auto" : value;
    };

    const themeResolved = (value) =>
        value === "auto" && themeMedia && themeMedia.matches ? "dark" : value === "dark" ? "dark" : "light";

    const themeApply = (value) => {
        const theme = themeValues.indexOf(value) === -1 ? "auto" : value;
        document.documentElement.setAttribute("data-bs-theme", themeResolved(theme));
        document.documentElement.setAttribute("data-zotonic-theme", theme);
        $(".admin-theme-menu-item").each(function() {
            const isActive = $(this).attr("data-admin-theme-value") === theme;
            $(this).toggleClass("active", isActive);
        });
    };

    const themeSet = (value) => {
        const theme = themeValues.indexOf(value) === -1 ? "auto" : value;
        try {
            localStorage.setItem(themeStorageKey, JSON.stringify(theme));
        } catch (e) {
            // Ignore storage failures; the selected theme still applies to the current page.
        }
        themeApply(theme);
    };

    if (themeMedia && themeMedia.addEventListener) {
        themeMedia.addEventListener("change", function() {
            if (themePreference() === "auto") {
                themeApply("auto");
            }
        });
    } else if (themeMedia && themeMedia.addListener) {
        themeMedia.addListener(function() {
            if (themePreference() === "auto") {
                themeApply("auto");
            }
        });
    }

    // Menu item to change the theme
    $(document).on("click", ".admin-theme-menu-item", function(e) {
        e.preventDefault();
        themeSet($(this).attr("data-admin-theme-value"));
    });

    $.widget("ui.adminLinkedTable",
    {
        // make row cells clickable
        // by adding class 'clickable'
        // except for rows and cells that have the class 'not-clickable'
        _init: function() {
            var self = this;
            (self.element.find("tr").each(function() {
                var $row,
                    href,
                    event,
                    $cell;
                $row = $(this);
                href = $row.attr("data-href");
                event = $row.attr("data-event");
                if (!href && !event) {
                    return;
                }
                if ($row.hasClass("not-clickable")) {
                    return;
                }
                $("td, th", $row).each(function() {
                    $cell = $(this);
                    if (!$cell.hasClass("not-clickable")) {
                        $cell.addClass("clickable");
                        $cell.on("click", function() {
                            if (event) {
                                z_event(event);
                            } else {
                                document.location = href;
                            }
                        });
                    }
                });
            }));
            (self.element.find("td").each(function() {
                var $cell,
                    href,
                    event;
                $cell = $(this);
                href = $cell.attr("data-href");
                event = $cell.attr("data-event");
                if (!href && !event) {
                    return;
                }
                if ($cell.hasClass("not-clickable")) {
                    return;
                }
                $cell.addClass("clickable");
                $cell.on("click", function() {
                    if (event) {
                        z_event(event);
                    } else {
                        document.location = href;
                    }
                });
            }));
        }
    });

    $.widget("ui.autofocus",
    {
        _init: function() {
            self = this;
            setTimeout(() => self.element.focus(), 0);
        }
    });

    var scrollTimer = undefined;
    $(window).scroll(function() {
        if (scrollTimer) clearTimeout(scrollTimer);
        scrollTimer = setTimeout(function() {
            if ($(window).scrollTop() > 118) {
                $('body').addClass('scrolled');
            } else {
                $('body').removeClass('scrolled');
            }
        }, 200);
    });

    window.addEventListener('keydown', (e) => {
        if ((e.ctrlKey || e.metaKey) && e.key === 's') {
            event.preventDefault();
            $('#rscform').submit();
        }
    });

})(jQuery);

/*
After a page connection is done. Calls a named wire (that must exist).
See: _admin_edit_content_page_connections_list.tpl
*/

window.zAdminLinkDone = function(v) {
    window.z_zlink(v.url_language, v.title_language);
};

window.zAdminMediaDone = function(v) {
    window.z_choose_zmedia(v.object_id, v);
};

window.zEditLanguage = function() {
    return $('.language-tabs li.active').attr('lang');
};

function z_admin_ensure_block_names() {
    var names = [];
    $('input.block-name').each(function() {
        var name = $(this).val();
        if (name !== '') {
            names.push(name);
        }
    });

    $('input.block-name').each(function() {
        var name = $(this).val();
        if (name === '')
        {
            var $type = $("input.block-type", $(this).closest(".block"));
            if ($type.length > 0) {
                name = $type.val().split("_").pop();
                var ct = 1;
                while (names.indexOf(name+ct) != -1) {
                    ct++;
                }
                $(this).val(name+ct);
                names.push(name+ct);
            }
        }
    });
};
