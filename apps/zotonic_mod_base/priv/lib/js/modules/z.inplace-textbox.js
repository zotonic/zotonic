/* inputoverlay js
----------------------------------------------------------

@package:       Zotonic 2010
@Author:        Konstantin Nikiforov <helllamer@gmail.com>

Copyright 2010 Konstantin Nikiforov

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

/* switches the state of inplace_textbox to reading mode */
function inplace_textbox_write2read(div) {    
    div_read = div + "-r";
    div_nodata = div + "-n";
    input = div + "-i";
    div_edit = div + "-e";

    $("#" + div_edit).hide();
    
    value_input = $("#" + input).attr('value');
    $("#" + div_read + " span:first").html(value_input);
    if (jQuery.trim(value_input) == '') {
        $("#" + div_nodata).show();
    } else {
        $("#" + div_read).show();
    }
}

/* switches the state of inplace_textbox to editing mode */
function inplace_textbox_read2write(div) {
    div_read = div + "-r";
    $("#" + div_read).hide();
    
    div_nodata = div + "-n";
    $("#" + div_nodata).hide();

    div_edit = div + "-e";
    input = div + "-i";

    value = $("#" + div_read + " span:first").html();
    $("#" + input).attr('value', value); // copypaste value from text-span to input control
    $("#" + div_edit).show();
    $("#" + div_edit + " input:first").focus().select();
}


// show hint in read mode
function inplace_textbox_show_hint(div) { $("#" + div + "-r" + " > #hint:first").show(); }
function inplace_textbox_hide_hint(div) { $("#" + div + "-r" + " > #hint:first").hide(); }


// initialize inplace_textboxes
$(".inplace_textbox").each( function() {inplace_textbox_write2read($(this).attr('id')) } );

