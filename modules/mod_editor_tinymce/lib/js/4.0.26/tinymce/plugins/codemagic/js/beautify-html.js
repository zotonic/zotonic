/*

 Style HTML
---------------

  Written by Nochum Sossonko, (nsossonko@hotmail.com)

  Based on code initially developed by: Einar Lielmanis, <elfz@laacz.lv>
    http://jsbeautifier.org


  You are free to use this in any way you want, in case you find this useful or working for you.

  Usage:
    style_html(html_source);

  Recent changes:
  2013-04-08, bsy-web (bsy.github@gmail.com)
  	- added:   (array)  Parser.Utils.inline_token 		- to hold list of HTML inline elements for identification
  	- added:   (string) new TK_TAG_INLINE token 			- to handle formatting of inline tags specifically
  	- added:   (bool)   Parser.found_leading_whitespace 	- to help with conditional inline tag formatting
  	- added:   (bool)   Parser.found_trailing_whitespace 	- to help with conditional inline tag formatting
  	- added:   Comments through out the file to help follow flow of data and understand algorithm
  	- fixed:   Space left when trimming whitespace after "<" inside tag (e.g. "<  tag>"). Now outputs "<tag>"
  	- changed: get_token(), get_string, get_content(), and get_tag() only return array[string, string] (previously array[string, string] or just string)
  	- changed: unfinished content (unclosed tags or scripts) now get formatted even when EOF is reached
  	- todo:    add option to print inline tags on same line as content (inline tags currently get newline if whitespace allows)
  	- todo:    add indentation formatting for nested inline tags (e.g. <em>italic <strong>and bold</strong><em>)
  	- todo:	   create test driver script with input test cases to track how changes to this file affect final output (regression testing)
*/

// Wrapper function to invoke all the necessary constructors and deal with the output.
function style_html(html_source, indent_size, indent_character, max_char, brace_style) {

    var Parser, multi_parser;

    /* Function: Parser()
       This function initializes the parsing environment and contains many other sub-functions
       which will be called as needed by an instance of Parser */
    function Parser() {
        this.pos = 0; // Parser position
        this.found_leading_whitespace = false;
        this.found_trailing_whitespace = false;
        this.token = '';
        this.current_mode = 'CONTENT'; // reflects the current Parser mode: TAG/CONTENT
        this.tags = { // An object to hold tags, their position, and their parent-tags, initiated with default values
            parent: 'parent1',
            parentcount: 1,
            parent1: ''
        };

        this.tag_type = '';
        this.token_text = this.last_token = this.last_tag_token = this.last_text = this.token_type = '';

        this.Utils = { // Uilities made available to the various functions
            // Entities considered white-space
            whitespace: "\n\r\t ".split(''),

            /* HTML elements which are considered inline (and thus affected by whitespace such as newlines;
               i.e. indentation considerations required for these tags)

               SCA, 2013-04-04: Note: Some elements are both inline and single (e.g. br, input, img).
               Check how this effects display logic and opt for keeping it in token array instead.
            */
            inline_token: 'a,abbr,acronym,b,bdo,big,br,button,cite,code,dfn,em,i,img,input,kbd,label,map,object,q,samp,script,select,small,span,strong,sub,sup,textarea,tt'.split(','),

            // HTML elements without a closing tag pair
            single_token: '!doctype,?xml,area,base,basefont,br,embed,hr,img,input,isindex,link,meta,param,wbr'.split(','),

            // Elements which get get extra blank line before them (stylistic preference)
            extra_liners: 'body,head,/html'.split(','), // for tags that need a line of whitespace before them

            // Function to check if given element is in given array
            in_array: function (what, arr) {
                for (var i = 0; i < arr.length; i++) {
                    if (what === arr[i]) {
                        return true;
                    }
                }
                return false;
            }
        } /* End of Function: Parser() */

        /* Function: get_content() */
        this.get_content = function () { // function to capture regular content between tags
            var input_char = '';
            var content = [];
            var space = false; // if a space is needed

            // Ingest content until start of new tag ('<')
            while (this.input.charAt(this.pos) !== '<') { // content ingestion complete if new tag is starting
                if (this.pos >= this.input.length) {    // we know we're at EOF...
                    return [content.join(''),'TK_EOF']; // ...so just pass back any ingested content to get printed
                }

                input_char = this.input.charAt(this.pos);
                this.pos++;
                this.line_char_count++;

                if (this.Utils.in_array(input_char, this.Utils.whitespace)) {
                    if (content.length) { // only mark space as true if non-spaces have already been ingested (i.e leading whitespace doesn't count)
                        space = true; // flag non-leading whitespace encountered (can be used for breaking lines > max_char)
                    } else { // no content yet hence this is leading whitespace
                        this.found_leading_whitespace = true; // flag that whitespace came first so we can format inline tags correctly
                    }

                    this.line_char_count--;
                    this.found_trailing_whitespace = true; // flag that last char so far (even if singleton) is whitespace (for formatting inline tags correctly)
                    continue; // don't want to insert unnecessary space (start next iteration of loop)
                }
                else if (space) { // current char not a space but previous char may have been (which allows for long-line breaking)

                    // Insert a newline when the max_char is reached.
                    if (this.line_char_count >= this.max_char) {
                        // SCA: Note: This is the place to insert whitespace of choice for the inline tag case (or no newline)
                        content.push('\n'); // Start newline

                        // indent per nested tag depth
                        for (var i = 0; i < this.indent_level; i++) {
                            content.push(this.indent_string);
                        }

                        this.line_char_count = 0;
                    }
                    else {
                        content.push(' '); // put in that space that was left out in previous iteration
                        this.line_char_count++;
                    }

                    space = false;
                    this.found_trailing_whitespace = false; // last read char not space hence reset trailing space flag
                }

                content.push(input_char); // letter at-a-time (or string) inserted to an array
            }

            return [content.join(''), 'TK_CONTENT']; // start of new tag ('<') encountered so return current ingested string
        } /* End of Function: get_content() */

        /* Function: get_script() */
        this.get_script = function () { // get the full content of a script to pass to js_beautify
            var input_char = '';
            var content = [];
            var script_text = '';
            var reg_match = new RegExp('\<\/script' + '\>', 'igm');
            reg_match.lastIndex = this.pos;
            var reg_array = reg_match.exec(this.input);
            var end_script = reg_array ? reg_array.index : this.input.length; // absolute end of script
            var reached_eof = (this.pos >= this.input.length);

            while ((this.pos < end_script) && !reached_eof) { // get everything in between the script tags
                input_char = this.input.charAt(this.pos);
                this.pos++;
                reached_eof = (this.pos >= this.input.length);

                content.push(input_char);
            }

            script_text = !content.length ? '' : // check if we have an content
                js_beautify( content.join(''),   // apply the JS Beautifier on that content (even if it is partial, i.e. reached EOF)
                {
                    indent_size: this.indent_size,
                    indent_char: this.indent_character,
                    indent_level: this.indent_level,
                    brace_style: this.brace_style
                }
            );

            // return content.length ? content.join('') : ''; // we might not have any content at all
            return [script_text, reached_eof ? 'TK_EOF' : 'TK_CONTENT'];
        } /* Function: get_script() */

        /* Function: record_tag(tag) */
        this.record_tag = function (tag) { // function to record a tag and its parent in this.tags Object
            if (this.tags[tag + 'count']) { // check for the existence of this tag type
                this.tags[tag + 'count']++;
                this.tags[tag + this.tags[tag + 'count']] = this.indent_level; // and record the present indent level
            } else { // otherwise initialize this tag type
                this.tags[tag + 'count'] = 1;
                this.tags[tag + this.tags[tag + 'count']] = this.indent_level; // and record the present indent level
            }
            this.tags[tag + this.tags[tag + 'count'] + 'parent'] = this.tags.parent; // set the parent (i.e. in the case of a div this.tags.div1parent)
            this.tags.parent = tag + this.tags[tag + 'count']; // and make this the current parent (i.e. in the case of a div 'div1')
        } /* End of Function: record_tag(tag) */

        /* Function: retrieve_tag(tag) */
        this.retrieve_tag = function (tag) { // function to retrieve the opening tag to the corresponding closer
            if (this.tags[tag + 'count']) { // if the openener is not in the Object we ignore it
                var temp_parent = this.tags.parent; // check to see if it's a closable tag.

                while (temp_parent) { // till we reach '' (the initial value);
                    if (tag + this.tags[tag + 'count'] === temp_parent) { // if this is it use it
                        break;
                    }

                    temp_parent = this.tags[temp_parent + 'parent']; // otherwise keep on climbing up the DOM Tree
                }

                if (temp_parent) { // if we caught something
                    this.indent_level = this.tags[tag + this.tags[tag + 'count']]; // set the indent_level accordingly
                    this.tags.parent = this.tags[temp_parent + 'parent']; // and set the current parent
                }

                delete this.tags[tag + this.tags[tag + 'count'] + 'parent']; // delete the closed tags parent reference...
                delete this.tags[tag + this.tags[tag + 'count']]; //...and the tag itself

                if (this.tags[tag + 'count'] == 1) {
                    delete this.tags[tag + 'count'];
                } else {
                    this.tags[tag + 'count']--;
                }
            }
        } /* End of Function: retrieve_tag(tag) */

        /* Function: get_tag() */
        this.get_tag = function () { // function to get a full tag and parse its type
            var input_char = '';
            var content = [];
            var space = false;

            do {
                if (this.pos >= this.input.length) { 		// if we reach end of input (we already have last char)
                    return [content.join(''), 'TK_EOF'];        // pass back anything ingested so far (so not to lose it)
                    // and flag as reaching EOF
                }

                input_char = this.input.charAt(this.pos);
                this.pos++;
                this.line_char_count++;

                if (this.Utils.in_array(input_char, this.Utils.whitespace)) { // don't want to insert unnecessary space
                    space = true;
                    this.line_char_count--;
                    continue; // jump to next iteration of loop hence trimming leading white space inside tag (e.g. <   p>)
                }

                // Case for quotes within the tag itself
                if (input_char === "'" || input_char === '"') { // start of quoted string
                    // SCA: VERIFY: What is "!content[1]" supposed to check? Whitespace after '<' should be discarded.
                    if (!content[1] || content[1] !== '!') { // if we're in a comment, strings don't get treated specially
                        input_char += this.get_unformatted(input_char); // read everything up to end of single/double quote
                        space = true; // queue inserting space after quote contents and end quote (a check below prevents it for start quote which follows a '=')
                    }
                }

                // Cases where previous space should disappear
                if (content[content.length - 1] === '<' ||  // if last stored char was '<' no space after it (e.g. "< a> -> "<a>)
                	input_char === '=')                 // no space before '=' (e.g. "<a href ='/code/"> -> "<a href='/code/'>)
                	// input_char !== '>')
                {
                    space = false;
                }

                // Case for determining what do with previous whitespace
                if (space && content.length && 		   	// if previous character was non-leading whitespace (came after some content)
                    content[content.length - 1] !== '=' && 	// if last stored char was "=" no space after it (e.g. "<a href= '/code/"> -> "<a href='/code/'>)
                    input_char !== '>') {			// no space before '>' (e.g. "<a href='/code/" > -> "<a href='/code/'>)

                    // Note: Space is still true for end quotes

                    // Allow opening tag to break if longer than max_char
                    if (this.line_char_count >= this.max_char) {
                        this.print_newline(false, content);
                        this.line_char_count = 0;
                    } else { // append the previously digested space
                        content.push(' ');
                        this.line_char_count++;
                    }

                    space = false;
                }

                content.push(input_char); // append current char to tag string
            } while (input_char !== '>'); // exit loop at tag closure character


            // Arriving here means a complete tag was captured (e.g. "<xxx>")
            var tag_complete = content.join('');
            var tag_index;

            if (tag_complete.indexOf(' ') != -1) {      // if there's whitespace, thats where the tag name ends
                tag_index = tag_complete.indexOf(' ');
            } else {                                    // otherwise go with the tag ending
                tag_index = tag_complete.indexOf('>');
            }

            // Identify the type of tag that has just been ingested
            var tag_check = tag_complete.substring(1, tag_index).toLowerCase();

            if (tag_complete.charAt(tag_complete.length - 2) === '/' ||
                this.Utils.in_array(tag_check, this.Utils.single_token)) { // if this tag name is a single tag type (either in the list or has a closing /)
                this.tag_type = 'SINGLE';
            }
            else if (tag_check === 'script') { // for later script handling
                this.record_tag(tag_check);
                this.tag_type = 'SCRIPT';
            }
            else if (tag_check === 'style') { // for future style handling (for now it justs uses get_content)
                this.record_tag(tag_check);
                this.tag_type = 'STYLE';
            }
            else if (this.Utils.in_array(tag_check, this.Utils.inline_token)) { // check if current tag is an inline element
                var inline_element = this.get_unformatted('</' + tag_check + '>', tag_complete); //...delegate to get_unformatted function
                content.push(inline_element);
                this.tag_type = 'INLINE';
            }
            else if (tag_check.charAt(0) === '!') { // peek for <!-- comment
                if (tag_check.indexOf('[if') != -1) { // peek for <!--[if conditional comment
                    if (tag_complete.indexOf('!IE') != -1) { // this type needs a closing --> so...
                        var comment = this.get_unformatted('-->', tag_complete); //...delegate to get_unformatted
                        content.push(comment);
                    }
                    this.tag_type = 'START';
                } else if (tag_check.indexOf('[endif') != -1) { // peek for <!--[endif end conditional comment
                    this.tag_type = 'END';
                    this.unindent();
                } else if (tag_check.indexOf('[cdata[') != -1) { // if it's a <[cdata[ comment...
                    var comment = this.get_unformatted(']]>', tag_complete); //...delegate to get_unformatted function
                    content.push(comment);
                    this.tag_type = 'SINGLE'; //<![CDATA[ comments are treated like single tags
                } else {
                    var comment = this.get_unformatted('-->', tag_complete);
                    content.push(comment);
                    this.tag_type = 'SINGLE';
                }
            }
            else {
                if (tag_check.charAt(0) === '/') { // this tag is a double tag (end tag, eg. </p>) so check for tag-ending
                    this.retrieve_tag(tag_check.substring(1)); // remove it and all ancestors
                    this.tag_type = 'END';
                }
                else { // otherwise it's a start-tag
                    this.record_tag(tag_check); // push it on the tag stack
                    this.tag_type = 'START'; // unidentified tags are labeled as a generic opening tag
                }

                if (this.Utils.in_array(tag_check, this.Utils.extra_liners)) { // check if this double needs an extra line
                    this.print_newline(true, this.output); // SCA: Note: Why is this here instead of in standard location (final case statement)?
                }
            }

            return [content.join(''), 'TK_TAG_' + this.tag_type]; // will only reach here if '>' was encountered
        } /* Function: get_tag() */

        /* Function: get_unformatted(delimiter, orig_tag) */
        this.get_unformatted = function (delimiter, orig_tag) { // function to return unformatted content in its entirety
            if (orig_tag && orig_tag.indexOf(delimiter) != -1) {
                return '';
            }

            var input_char = '';
            var content = '';
            var space = true;

            do {
                if (this.pos >= this.input.length) {
                    return content;
                }

                input_char = this.input.charAt(this.pos);
                this.pos++

                if (this.Utils.in_array(input_char, this.Utils.whitespace)) {
                    if (!space) {
                        this.line_char_count--;
                        continue;
                    }

                    if (input_char === '\n' || input_char === '\r') {
                        content += '\n';

                        for (var i = 0; i < this.indent_level; i++) {
                            content += this.indent_string;
                        }

                        space = false; //...and make sure other indentation is erased
                        this.line_char_count = 0;

                        continue;
                    }
                }

                content += input_char;
                this.line_char_count++;
                space = true;
            } while (content.indexOf(delimiter) == -1); // repeat digestion of each char until end tag is consumed

            return content;
        } /* End of Function: get_unformatted(delimiter, orig_tag) */

        /* Function: get_token() */
        this.get_token = function () { // initial handler for token-retrieval
            if (this.last_token === 'TK_TAG_SCRIPT') { // check if we need to format script content (after <script> tag)
                return this.get_script();
            }

            if (this.current_mode === 'CONTENT') {
                return this.get_content();
            }

            if (this.current_mode === 'TAG') {
                return this.get_tag();
            }
        } /* Function: get_token() */

        /* Function: printer(js_source, indent_character, indent_size, max_char, brace_style) */
        this.printer = function (js_source, indent_character,
            indent_size, max_char, brace_style) { // handles input/output and some other printing functions

            // Provides default values to arguments
            this.input = js_source || ''; // gets the input for the Parser
            this.output = [];
            this.indent_character = indent_character || ' '; // ' '
            this.indent_string = '';
            this.indent_size = indent_size || 2;
            this.brace_style = brace_style || 'collapse';
            this.indent_level = 0;
            this.max_char = max_char || 70; // maximum amount of characters per line
            this.line_char_count = 0; // count to see if max_char was exceeded

            // Create/cache indent unit: indent_character * indent_size
            for (var i = 0; i < this.indent_size; i++) {
                this.indent_string += this.indent_character;
            }

            /* Function: print_newline(ignore, arr) */
            this.print_newline = function (ignore, arr) {
                this.line_char_count = 0;

                if (!arr || !arr.length) {
                    return;
                }

                if (!ignore) { // we might want the extra line
                    while (this.Utils.in_array(arr[arr.length - 1],
                        this.Utils.whitespace)) {
                        arr.pop();
                    }
                }

                arr.push('\n');

                for (var i = 0; i < this.indent_level; i++) {
                    arr.push(this.indent_string);
                }
            } /* End of Function: print_newline(ignore, arr) */

            this.print_token = function (text) {
                this.output.push(text);
            }; /* End of Function: print_newline(ignore, arr) */

            this.indent = function () {
                this.indent_level++;
            }; /* End of Function: indent() */

            this.unindent = function () {
                if (this.indent_level > 0) {
                    this.indent_level--;
                }
            }; /* End of Function: unindent() */

        } /* End of Function: printer(js_source, indent_character, indent_size, max_char, brace_style) */

        return this;
    }

    // Create a new Parser
    var last_token_had_trailing_whitespace = false;
    multi_parser = new Parser(); // wrapping functions Parser

    /*
        html_source: Orginal source as stored in database.
        indent_character: Character to use as a single indent (e.g. space, tab, etc). As passed into style_html, which is called from CodeMagic.js.
        indent_size: Width of left indent margin. As passed into style_html, which is called from CodeMagic.js.
        max_char: Max length of line of source. As passed into style_html, which is called from CodeMagic.js.
        brace_style: Unsure how each style is different. As passed into style_html, which is called from CodeMagic.js.
    */
    multi_parser.printer(html_source, indent_character, indent_size,
        max_char, brace_style); // initialize starting values

    /*
        Start moving through source token by token until EOF (end of file) is reached.
    */
    do {
        var t = multi_parser.get_token();
        multi_parser.token_text = t[0];
        multi_parser.token_type = t[1];

        /*
    	    Handle each token type in a specific way.
        */
        switch (multi_parser.token_type) {
            case 'TK_TAG_START':
            case 'TK_TAG_SCRIPT':
            case 'TK_TAG_STYLE':
                multi_parser.print_newline(false, multi_parser.output);
                multi_parser.print_token(multi_parser.token_text);
                multi_parser.indent(); // increase indent level
                multi_parser.current_mode = 'CONTENT'; // toggle to check for content now that a tag has been digested
                multi_parser.last_tag_token = multi_parser.token_type;
                break;

            case 'TK_TAG_END':
                multi_parser.print_newline(true, multi_parser.output);
                multi_parser.print_token(multi_parser.token_text);
                multi_parser.current_mode = 'CONTENT'; // toggle to check for content now that a tag has been digested
                multi_parser.last_tag_token = multi_parser.token_type;
                break;

            case 'TK_TAG_SINGLE':
                multi_parser.print_newline(false, multi_parser.output);
                multi_parser.print_token(multi_parser.token_text);
                multi_parser.current_mode = 'CONTENT'; // toggle to check for content now that a tag has been digested
                multi_parser.last_tag_token = multi_parser.token_type;
                break;

            case 'TK_TAG_INLINE':
            	// SCA: TODO: Apply indentation formatting to nested inline tags (e.g. <em>italic <strong>and bold</strong><em>).
            	// Currently puts all content as it gets it using get_unformatted. Could call style_html() recursively on it.

                // text preceeding and adjacent to inline tag with out space in between
                if (
                        // it seems CONTENT is not guaranteed after a TAG (verified through logging of long HTML samples) hence the explicit condition statements
                        (last_token_had_trailing_whitespace && (multi_parser.last_token === "TK_CONTENT")) ||
                        // empty string between current inline tag and previous tag is not inline
                        (!multi_parser.last_text && (multi_parser.last_tag_token !== "TK_TAG_INLINE"))
                   )
                {
                    // only time inline tag shoud not be formatted is if preceding text was content with no trailing whitespace
                    multi_parser.print_newline(false, multi_parser.output);
                }

                multi_parser.print_token(multi_parser.token_text);
                multi_parser.current_mode = 'CONTENT'; // toggle to check for content now that a tag has been digested
                multi_parser.last_tag_token = multi_parser.token_type;
                break;

            case 'TK_EOF': // print anything ingested before exiting (so incomplete tags don't disappear from end of input
            case 'TK_CONTENT':
                if (multi_parser.token_text !== '') { // check if there is any content
                    if (multi_parser.last_token === 'TK_TAG_INLINE') { // if the previous tag was an inline element
                        if (multi_parser.found_leading_whitespace) { // and current content had leading white space
                            // output whitespace (a space can be added rather than newline in future versions)
                            multi_parser.print_newline(false, multi_parser.output);
                        } // else output content adjacent to inline element (i.e. no whitespace)
                    } else { // previous tag was not a inline element
                        multi_parser.print_newline(false, multi_parser.output); // output newline
                    }

                    multi_parser.print_token(multi_parser.token_text);
                }

                multi_parser.current_mode = 'TAG'; // toggle to check for a tag now that content has been digested
                break;
        }

        // Remember and reinitialize values for next loop iteration.
        multi_parser.last_token = multi_parser.token_type;
        multi_parser.last_text = multi_parser.token_text;

        last_token_had_trailing_whitespace = multi_parser.found_trailing_whitespace; // needed in "case 'TK_TAG_INLINE'" for inline tag formatting
        multi_parser.found_leading_whitespace = false;  // reset to false so it can be set to true if encountered for next token
        multi_parser.found_trailing_whitespace = false; // reset to false so it can be set to true if encountered for next token
    } while (multi_parser.token_type !== 'TK_EOF');

    return multi_parser.output.join(''); // return the fully formatted source
}
