//vatriable definition
var the = { 
    beautify_in_progress: false,
    coloring_active: true,
    autocompletion_active: true
} 
var hlLine = null; 
var lastPos = null;
var lastQuery = null;
var marked = [];
var pluginCodeMirror = null;
var pluginOptions = {
    lineNumbers: true,
    mode: "text/html",
    indentUnit: 4,
    matchBrackets: true,
    onCursorActivity: function() {
        pluginCodeMirror.setLineClass(hlLine, null);
        hlLine = pluginCodeMirror.setLineClass(pluginCodeMirror.getCursor().line, "activeline");
    },
    onKeyEvent: function(i, e) {
        if(the.autocompletion_active) {
            /* Hook into charcode < */
            if(String.fromCharCode(e.which == null ? e.keyCode : e.which) == "<") {
                e.stop();
                
                var cur = pluginCodeMirror.getCursor(false), token = pluginCodeMirror.getTokenAt(cur);
                pluginCodeMirror.replaceRange("<", cur);
                
                setTimeout(startComplete, 50);
                return true;
                
            // Hook into ctrl + space
            } if (e.keyCode == 32 && (e.ctrlKey || e.metaKey) && !e.altKey) {
                e.stop(); 
                return startComplete();    
            } 
        }
    },
    onHighlightComplete: onChangeCallback
}

//language and init
tinyMCEPopup.requireLangPack();
tinyMCEPopup.onInit.add(onLoadInit);


//onload
function onLoadInit() {
	tinyMCEPopup.resizeToInnerSize();
    
    // Remove Gecko spellchecking
	if (tinymce.isGecko)
		document.body.spellcheck = tinyMCEPopup.editor.getParam("gecko_spellcheck");

    //insert html from tinymce to htmlSource textarea
	document.getElementById("htmlSource").value = tinyMCEPopup.editor.getContent({ source_view : true });
    
    //format the source code using JS Beautifier plugin
    beautify("htmlSource");
    
    if(the.coloring_active) {
        //activate syntax coloring
        activateCodeColoring("htmlSource");
    }
    
    //resize window to fit the textarea
    resizeInputs("htmlSource"); 
    
    window.onresize = function(event) {
        //resize window to fit the textarea
        resizeInputs("htmlSource"); 
    }

}


//activating syntax coloring
function activateCodeColoring(id) {
    the.coloring_active = true;
    
    document.getElementById("search_replace").className = "";
    document.getElementById("reintendt").className = "";
    document.getElementById("autocompletion").disabled = false;
    
    pluginCodeMirror = CodeMirror.fromTextArea(document.getElementById(id), pluginOptions);
    hlLine = pluginCodeMirror.setLineClass(0, "activeline"); 
    pluginCodeMirror.focus(); 
}


//deactivating syntax coloring
function deactivateCodeColoring() {
    the.coloring_active = false;
    
    document.getElementById("undo").className = "disabled";
    document.getElementById("redo").className = "disabled";
    document.getElementById("search_replace").className = "disabled";
    document.getElementById("reintendt").className = "disabled";
    document.getElementById("autocompletion").disabled = true;
    
    pluginCodeMirror.toTextArea();
    pluginCodeMirror = null;   
    
    //resize window to fit the textarea
    resizeInputs("htmlSource"); 
}


//toggle highlighting usint a checkbox
function toggleHighlighting(elm, id) {
    if (elm.checked) {
        activateCodeColoring(id);
    } else {
        deactivateCodeColoring();
    }
}

//toggle code autocompletion
function toggleAutocompletion(elm) {
    if (elm.checked) {
        the.autocompletion_active = true;
    } else {
        the.autocompletion_active = false;
    }
}     

//save content bact to tinymce editor
function saveContent() {
    tinyMCEPopup.editor.setContent(pluginCodeMirror.getValue(), { source_view : true }); 
    tinyMCEPopup.close();
}


//resize textarea input
function resizeInputs(id) {
	var vp = tinyMCEPopup.dom.getViewPort(window), el;

	el = document.getElementById(id);

	if (el) {
		//el.style.width = (vp.w - 20) + "px";
		el.style.height = (vp.h - 65) + "px";
	}
}


//toggle search window
function toggleSearch(elm, id) {
    if(!the.coloring_active) return false;
    
    elm.className = "";
    var element = document.getElementById(id);
    
    if(element.style.display == "none") {
        elm.className = "selected";
        element.style.display = "block";
    }
    else element.style.display = "none";
}






/**
*  CodeMirror 2
*  -------------
*    
*  CodeMirror 2 is a rewrite of CodeMirror 1 (http://github.com/marijnh/CodeMirror). 
*  The docs live here http://codemirror.net/2/manual.html, 
*  and the project page is http://codemirror.net/2/.
*                                                        * 
*  http://codemirror.net/
* 
*  Copyright (C) 2011 by Marijn Haverbeke <marijnh@gmail.com>
*/                         


CodeMirror.defineMode("htmlmixed", function(config, parserConfig) {
    var htmlMode = CodeMirror.getMode(config, {name: "xml", htmlMode: true});
    var jsMode = CodeMirror.getMode(config, "javascript");
    var cssMode = CodeMirror.getMode(config, "css");
    
    function html(stream, state) {
        var style = htmlMode.token(stream, state.htmlState);
        if (style == "xml-tag" && stream.current() == ">" && state.htmlState.context) {
            if (/^script$/i.test(state.htmlState.context.tagName)) {
                state.token = javascript;
                state.localState = jsMode.startState(htmlMode.indent(state.htmlState, ""));
            } else if (/^style$/i.test(state.htmlState.context.tagName)) {
                state.token = css;
                state.localState = cssMode.startState(htmlMode.indent(state.htmlState, ""));
            }
        }                               
        return style;
    }
    function maybeBackup(stream, pat, style) {
        var cur = stream.current();
        var close = cur.search(pat);
        if (close > -1) stream.backUp(cur.length - close);
        return style;
    }
    
    function javascript(stream, state) {
        if (stream.match(/^<\/\s*script\s*>/i, false)) {
            state.token = html;
            state.curState = null;
            return html(stream, state);
        }
        return maybeBackup(stream, /<\/\s*script\s*>/, jsMode.token(stream, state.localState));
    }
    function css(stream, state) {
        if (stream.match(/^<\/\s*style\s*>/i, false)) {
            state.token = html;
            state.localState = null;
            return html(stream, state);
        } 
        return maybeBackup(stream, /<\/\s*style\s*>/, cssMode.token(stream, state.localState));
    }

    return {
        startState: function() {
            var state = htmlMode.startState();
            return {token: html, localState: null, htmlState: state};
        },

        copyState: function(state) {
            if (state.localState)
                var local = CodeMirror.copyState(state.token == css ? cssMode : jsMode, state.localState);
            return {token: state.token, localState: local, htmlState: CodeMirror.copyState(htmlMode, state.htmlState)};
        },

        token: function(stream, state) {
            return state.token(stream, state);
        },

        indent: function(state, textAfter) {
            if (state.token == html || /^\s*<\//.test(textAfter))
                return htmlMode.indent(state.htmlState, textAfter);
            else if (state.token == javascript)
                return jsMode.indent(state.localState, textAfter);
            else
                return cssMode.indent(state.localState, textAfter);
        },

        electricChars: "/{}:"
    }
});

CodeMirror.defineMIME("text/html", "htmlmixed");

////////////////////////
//undo last action 
function undo() {
    pluginCodeMirror.undo();                       
} 


//redo last action
function redo() {
    pluginCodeMirror.redo();
}


//callback to onchange event on editor
function onChangeCallback(editor) {
    var undo = editor.historySize().undo;
    var redo = editor.historySize().redo;
    
    if(undo > 0) document.getElementById("undo").className = "";
    else document.getElementById("undo").className = "disabled";
    
    if(redo > 0) document.getElementById("redo").className = "";
    else document.getElementById("redo").className = "disabled";
}
 
//////////////////////// 
//reintendt html source code
function reIntendt(id) {
    if(!the.coloring_active) return false;
    
    deactivateCodeColoring();  
    document.getElementById("searchWindow").style.display = "none"; 
    beautify(id);
    activateCodeColoring(id); 
}
  
////////////////////////
//search and replace

//unmarks all searched words
function unmark() {
    for (var i = 0; i < marked.length; ++i) marked[i]();
    marked.length = 0;
}   

//search for query from #query input
function searchCode() {
    unmark();
    
    var text = document.getElementById("query").value;
    if (!text)  return false;    
    
    if(!pluginCodeMirror.getSearchCursor(text).findNext()) {
        alert(tinyMCEPopup.getLang('codemagic_dlg.nothing_found'));
        return false;    
    } 
    
    for (var cursor = pluginCodeMirror.getSearchCursor(text); cursor.findNext();)
        marked.push(pluginCodeMirror.markText(cursor.from(), cursor.to(), "searched"));
    
    if (lastQuery != text) lastPos = null;
    
    var cursor = pluginCodeMirror.getSearchCursor(text, lastPos || pluginCodeMirror.getCursor());
    if (!cursor.findNext()) {
        cursor = pluginCodeMirror.getSearchCursor(text);
        if (!cursor.findNext()) return;
    }
    pluginCodeMirror.setSelection(cursor.from(), cursor.to());
    lastQuery = text; lastPos = cursor.to();
}

//replace
function replaceCode() {
    unmark();
    
    var text = document.getElementById("query").value;
    var replace = document.getElementById("replace").value;
    
    if (!text) return false;    
    
    if(!pluginCodeMirror.getSearchCursor(text).findNext()) {
        alert(tinyMCEPopup.getLang('codemagic_dlg.nothing_to_replace'));
        return false;    
    }
    
    for (var cursor = pluginCodeMirror.getSearchCursor(text); cursor.findNext();)
        pluginCodeMirror.replaceRange(replace, cursor.from(), cursor.to());
} 


////////////////////////
//autocompletion  
var tagNames = ("a abbr acronym address applet area b base basefont bdo big blockquote body br button" + 
                " caption center cite code col colgroup dd del dfn dir div dl dt em fieldset font form frame " +
                " frameset h1 h2 h3 h4 h5 h6 head hr html i iframe img input ins isindex kbd label legend li link map" +
                " menu meta noframes noscript object ol optgroup option p param pre q s samp script select small" + 
                " span strike strong style sub sup table tbody td textarea tfoot th thead title tr tt u ul var").split(" ");
               
var pairedTags = ("a abbr acronym address applet b bdo big blockquote body button" + 
                  " caption center cite code colgroup del dfn dir div dl em fieldset font form" +
                  " frameset h1 h2 h3 h4 h5 h6 head html i iframe ins kbd label legend li map" +
                  " menu noframes noscript object ol optgroup option p pre q s samp script select small" + 
                  " span strike strong style sub sup table tbody td textarea tfoot th thead title tr tt u ul var").split(" "); 
                
var unPairedTags = ("area base basefont br col dd dt frame hr img input isindex link meta param").split(" ");

var specialTags = {
    "applet" : { tag: 'applet width="" height=""></applet>', cusror: 8 },
    "area" : { tag: 'area alt="" />', cusror: 6 },
    "base" : { tag: 'base href="" />', cusror: 7 },
    "form" : { tag: 'form action=""></form>', cusror: 9 },
    "img" : { tag: 'img src="" alt="" />', cusror: 6 },
    "map" : { tag: 'map name=""></map>', cusror: 7 },
    "meta" : { tag: 'meta content="" />', cusror: 10 },
    "optgroup" : { tag: 'optgroup label=""></optgroup>', cusror: 8 },
    "param" : { tag: 'param name="" />', cusror: 7 },
    "script" : { tag: 'script type=""></script>', cusror: 7 },
    "style" : { tag: 'style type=""></style>', cusror: 7 },
    "textarea" : { tag: 'textarea cols="" rows=""></textarea>', cusror: 7 }
}

// Minimal event-handling wrapper.
function stopEvent() {
    if (this.preventDefault) {this.preventDefault(); this.stopPropagation();}
    else {this.returnValue = false; this.cancelBubble = true;}
}
function addStop(event) {
    if (!event.stop) event.stop = stopEvent;
    return event;
}
function connect(node, type, handler) {
    function wrapHandler(event) {
        handler(addStop(event || window.event));
    }
    
    if (typeof node.addEventListener == "function")
      node.addEventListener(type, wrapHandler, false);
    else
      node.attachEvent("on" + type, wrapHandler);
}

function forEach(arr, f) {
    for (var i = 0, e = arr.length; i < e; ++i) f(arr[i]);
}

Array.prototype.inArray = function(value){
    for (var key in this)
        if (this[key] === value) return true;
    return false;
}



//autocompletion start
function startComplete() {
    var startingTag, unPaired;
    
    // We want a single cursor position.
    if (pluginCodeMirror.somethingSelected()) return;
    
    // Find the token at the cursor
    var cur = pluginCodeMirror.getCursor(false), token = pluginCodeMirror.getTokenAt(cur), tprop = token;
    
    if(token.string.indexOf("<") == 0 && token.string.indexOf("</") != 0) {
        token.string = token.string.replace("<", ""); 
        token.start++;
        startingTag = true; 
    } else if(token.string.indexOf("</") == 0) {
        token.string = token.string.replace("</", "");   
        token.start += 2;
        startingTag = false;
    } else {
        return;
    }         
    
    //get the tags    
    var completions = getCompletions(token, startingTag);
    if (!completions.length) return;
    
    
    function insert(str) {
        if(str == "") return;
        
        //trim
        str = str.replace(/^\s+|\s+$/g,"");
        
        //is this an unpaired tag?                
        unPaired = unPairedTags.inArray(str) ? true : false;
    
        if(specialTags[str] != null && startingTag) {
            var insertTag = specialTags[str].tag;    
            var jumpTo = (token.start + str.length + specialTags[str].cusror);
        } else if(startingTag && unPaired) {
            var insertTag = str + " />";    
            var jumpTo = (token.start + str.length + 3); 
        } else if (startingTag) {
            var insertTag = str + "></" + str + ">";    
            var jumpTo = (token.start + str.length + 1); 
        } else {
            var insertTag = str + ">";    
            var jumpTo = (token.start + str.length + 1);
        }
        
        pluginCodeMirror.replaceRange(insertTag, {line: cur.line, ch: token.start}, {line: cur.line, ch: token.end});
        pluginCodeMirror.setCursor({line: cur.line, ch: jumpTo});
    }
    
    // When there is only one completion, use it directly.
    /*if (completions.length == 1) {
        insert(completions[0]); return true;
    }*/

    // Build the select widget
    var complete = document.createElement("div");
    complete.className = "completions";
    
    var sel = complete.appendChild(document.createElement("select"));
    sel.multiple = false;
    
    if (completions.length == 1) sel.multiple = true;
    
    for (var i = 0; i < completions.length; ++i) {
        var opt = sel.appendChild(document.createElement("option"));
        opt.appendChild(document.createTextNode(completions[i]));
    }
    
    sel.firstChild.selected = true;
    sel.size = Math.min(10, completions.length);
    
    var pos = pluginCodeMirror.cursorCoords();
    complete.style.left = pos.x + "px";
    
    //top position
    if(pos.yBot > 448) pos.yBot = pos.yBot - 165;
    complete.style.top = pos.yBot + "px";
    
    document.body.appendChild(complete);
    
    
    var done = false;
    function close() {    
        if (done) return;
        done = true;
        complete.parentNode.removeChild(complete);
    }
    function pick() {  
        insert(sel.options[sel.selectedIndex].innerHTML);
        close();
        setTimeout(function(){
            pluginCodeMirror.focus();
        }, 50);
    }
    
    //bind events
    connect(sel, "blur", close);
    connect(sel, "keydown", function(event) {
        var code = event.keyCode;
      
        // Enter, space, tab
        if (code == 13 || code == 32 || code == 9) {event.stop(); pick();}
        // Escape (FIX: closes the tinymce popup window)
        else if (code == 27) {
            event.stop(); close(); pluginCodeMirror.focus();                                                   
        }
        //other than arrow up/down
        else if (code != 38 && code != 40 && code != 16 && code != 17 && code != 18 && code != 91 && code != 92) {
            console.log([code]);                        
            close(); 
            pluginCodeMirror.focus(); 
            if(code != 39 && code != 37) setTimeout(startComplete, 50); 
            else event.stop();
        }
    });
    connect(sel, "dblclick", pick);

    sel.focus();
    
    // Opera sometimes ignores focusing a freshly created node
    if (window.opera) setTimeout(function(){if (!done) sel.focus();}, 100);
    return true;
}

function getCompletions(token, startingTag) {
    var found = [], start = token.string;
    
    function maybeAdd(str) {       
        if (str.indexOf(start) == 0) found.push(str);  
    }
    
    if(startingTag) {
        forEach(tagNames, maybeAdd)    
    } else {
        forEach(pairedTags, maybeAdd)
    }
    
    return found;
}       





/**
*  JS Beautifier
*  ---------------
*  ...or, more specifically, all of the code powering jsbeautifier.org.
* 
*   
*  Written by Einar Lielmanis, <einar@jsbeautifier.org>
*
*  Thanks to Jason Diamond, Patrick Hof, Nochum Sossonko, Andreas Schneider, Dave
*  Vasilevsky, Vital Batmanov, Ron Baldwin, Gabriel Harrison, Chris J. Hull and
*  others.
*/
function beautify(id) {
    
    if (the.beautify_in_progress) return;
    the.beautify_in_progress = true;
    
    var source = document.getElementById(id).value.replace(/^\s+/, '');
    
    var indent_size = 4;
    var indent_char = ' ';
    var brace_style = 'collapse';  //collapse, expand, end-expand 
    
    document.getElementById(id).value = style_html(source, indent_size, indent_char, 120, brace_style);
    the.beautify_in_progress = false;
}            