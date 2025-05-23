// LiveValidation 1.3 (standalone version)
// Copyright (c) 2007-2008 Alec Hill (www.livevalidation.com)
// LiveValidation is licensed under the terms of the MIT License

// MW: 20100316: Adapted for async usage with Zotonic.
// MW: 20100629: Added support for presence check on radio buttons
// MW: 20110329: Dynamically fetch the validation fields from the DOM, this makes it possible to add/remove fields dynamically.
// AC: 20150129: Removed unused class names. Changed messageClass to Bootstrap 3 conform "help-block". Replaced hardcoded
//               "control-group" to fieldGroupClass, using Bootstrap 3 "form-group". Replaced hardcoded class "success" in favor
//               of fieldGroupSuccessClass (Bootstrap 3 "has-success"); replaced hardcoded class "error" in favor of
//               fieldGroupErrorClass (Bootstrap 3 "has-error").
// MW: 20241129: Fixed async submit in combination with Zotonic postbacks. Added extra classes: 'if-has-validated', 'if-has-error',
//               'if-form-validated' and 'if-form-error'. They are used to hide/show messages on field (with a form-group) or the
//               form level (within a 'form').


/*********************************************** LiveValidation class ***********************************/


function addLiveValidation(element, args) {
    if (!$(element).data("z_live_validation"))
        $(element).data("z_live_validation", new LiveValidation($(element).attr('id'), args));
}

function getLiveValidation(element) {
    return $(element).data("z_live_validation");
}


/**
 *  validates a form field in real-time based on validations you assign to it
 *
 *  @var element {mixed} - either a dom element reference or the string id of the element to validate
 *  @var optionsObj {Object} - general options, see below for details
 *
 *  optionsObj properties:
 *              validMessage {String}   - the message to show when the field passes validation
 *                            (DEFAULT: "Thankyou!")
 *              onAsync {Function}    - function to execute when field passes is waiting for async validation
 *                            (DEFAULT: function(){ this.insertMessage(this.createSpinnerSpan()); this.addFieldClass(); } )
 *              onValid {Function}    - function to execute when field passes validation
 *                            (DEFAULT: function(){ this.insertMessage(this.createMessageSpan()); this.addFieldClass(); } )
 *              onInvalid {Function}  - function to execute when field fails validation
 *                            (DEFAULT: function(){ this.insertMessage(this.createMessageSpan()); this.addFieldClass(); })
 *              insertAfterWhatNode {Int}   - position to insert default message
 *                            (DEFAULT: the field that is being validated)
 *              onlyOnBlur {Boolean} - whether you want it to validate as you type or only on blur
 *                            (DEFAULT: false)
 *              wait {Integer} - the time you want it to pause from the last keystroke before it validates (ms)
 *                            (DEFAULT: 0)
 *              onlyOnSubmit {Boolean} - whether should be validated only when the form it belongs to is submitted
 *                            (DEFAULT: false)
 */

var LiveValidation = function(element, optionsObj){
    this.initialize(element, optionsObj);
};

LiveValidation.VERSION = '1.3 standalone-zotonic';

/** element types constants ****/

LiveValidation.TEXTAREA = 1;
LiveValidation.TEXT     = 2;
LiveValidation.PASSWORD = 3;
LiveValidation.CHECKBOX = 4;
LiveValidation.SELECT   = 5;
LiveValidation.FILE     = 6;
LiveValidation.RADIO    = 7;
LiveValidation.FORM     = 8;


/****** prototype ******/

LiveValidation.prototype = {

    validClass: '', // was: z_valid
    invalidClass: 'z_invalid', // used ny mod_survey
    messageClass: 'z_validation help-block',
    validFieldClass: 'valid', // was: z_valid_field
    invalidFieldClass: 'invalid', // was: form-field-error
    asyncFieldClass: 'validating', // was: z_async_validation
    fieldGroupClass: 'form-group',
    fieldGroupErrorClass: 'has-error',
    fieldGroupSuccessClass: 'has-validated',
    fieldValidElementClass: 'if-has-validated',
    fieldInValidElementClass: 'if-has-error',

    /**
     *  initialises all of the properties and events
     *
     * @var - Same as constructor above
     */
    initialize: function(element, optionsObj){
      const self = this;

      if(!element)
        throw new Error("LiveValidation::initialize - No element reference or element id has been provided!");
      this.element = element.nodeName ? element : document.getElementById(element);
      if(!this.element)
        throw new Error("LiveValidation::initialize - No element with reference or id of '" + element + "' exists!");

      // default properties that could not be initialised above
      this.validations = [];
      this.elementType = this.getElementType();
      const options = optionsObj || {};
      if (this.elementType == LiveValidation.FORM) {
          this.form = this.element;
          this.onAsync = options.onAsync || function(){};
          this.onValid = options.onValid || function(){};
          this.onInvalid = options.onInvalid || function(){};
          this.onlyOnBlur =  options.onlyOnBlur || false;
          this.wait = options.wait || 0;
          this.onlyOnSubmit = true;
      } else {
          this.form = this.element.form;
          this.onAsync = options.onAsync || function(){ this.insertSpinner(this.createSpinnerSpan()); this.addFieldClass(); };
          this.onValid = options.onValid || function(){ this.insertMessage(this.createMessageSpan()); this.addFieldClass(); };
          this.onInvalid = options.onInvalid || function(){ this.insertMessage(this.createMessageSpan()); this.addFieldClass(); };
          this.onlyOnBlur =  options.onlyOnBlur || false;
          this.wait = options.wait || 0;
          this.onlyOnSubmit = options.onlyOnSubmit || false;
      }
      // options
      this.validMessage = options.validMessage || '';

      const node = options.insertAfterWhatNode || this.element;
      this.insertAfterWhatNode = node.nodeType ? node : document.getElementById(node);

      //if document.getElementById(node) returned null, then set the original element
      if(!this.insertAfterWhatNode)
          this.insertAfterWhatNode = this.element;

      this.validationAsync = false;

      // Initialize the form hooks, remember the LiveValidationForm object.
      if(this.form){
        this.formObj = LiveValidationForm.getInstance(this.form);
      }

      // events
      // collect old events
      if (this.elementType != LiveValidation.FORM) {
          this.oldOnFocus = this.element.onfocus || function(){};
          this.oldOnBlur = this.element.onblur || function(){};
          this.oldOnClick = this.element.onclick || function(){};
          this.oldOnChange = this.element.onchange || function(){};
          this.oldOnKeyup = this.element.onkeyup || function(){};
          this.element.onfocus = function(e){ self.doOnFocus(e); return self.oldOnFocus.call(this, e); };
          if(!this.onlyOnSubmit){
            switch(this.elementType){
              case LiveValidation.RADIO:
              case LiveValidation.CHECKBOX:
                this.element.onclick = function(e){ self.validate(); return self.oldOnClick.call(this, e); };
                this.element.onchange = function(e){ self.validate(); return self.oldOnChange.call(this, e); };
                break;
              case LiveValidation.SELECT:
              case LiveValidation.FILE:
                this.element.onchange = function(e){ self.validate(); return self.oldOnChange.call(this, e); };
                break;
              default:
                if(!this.onlyOnBlur) this.element.onkeyup = function(e){ self.deferValidation(); return self.oldOnKeyup.call(this, e); };
                this.element.onblur = function(e){ self.doOnBlur(e); return self.oldOnBlur.call(this, e); };
                break;
            }
          }
      }
    },

    /**
     *  destroys the instance's events (restoring previous ones) and removes it from any LiveValidationForms
     */
    destroy: function(){
        // remove events - set them back to the previous events
        if (this.elementType != LiveValidation.FORM) {
            this.element.onfocus = this.oldOnFocus;
            if(!this.onlyOnSubmit){
                switch(this.elementType){
                  case LiveValidation.RADIO:
                  case LiveValidation.CHECKBOX:
                    this.element.onclick = this.oldOnClick;
                    this.element.onchange = this.oldOnChange;
                    break;
                  case LiveValidation.SELECT:
                  case LiveValidation.FILE:
                    this.element.onchange = this.oldOnChange;
                    break;
                  default:
                    if(!this.onlyOnBlur) this.element.onkeyup = this.oldOnKeyup;
                    this.element.onblur = this.oldOnBlur;
                    break;
                }
            }
        }
        this.validations = [];
        this.removeMessageAndFieldClass();
    },

    /**
     * Adds a validation to perform to a LiveValidation object
     *
     * @var validationFunction {Function} - validation function to be used (ie Validate.Presence )
     * @var validationParamsObj {Object} - parameters for doing the validation, if wanted or necessary
     * @return {Object} - the LiveValidation object itself so that calls can be chained
     */
    add: function(validationFunction, validationParamsObj){
      this.validations.push( {type: validationFunction, params: validationParamsObj || {} } );
      return this;
    },

    /**
     * Removes a validation from a LiveValidation object - must have exactly the same arguments as used to add it
     *
     * @var validationFunction {Function} - validation function to be used (ie Validate.Presence )
     * @var validationParamsObj {Object} - parameters for doing the validation, if wanted or necessary
     * @return {Object} - the LiveValidation object itself so that calls can be chained
     */
    remove: function(validationFunction, validationParamsObj){
        let found = false;
        for( let i = 0, len = this.validations.length; i < len; i++ ){
            if( this.validations[i].type == validationFunction ){
                if (this.validations[i].params == validationParamsObj) {
                  found = true;
                  break;
                }
            }
        }
        if(found) this.validations.splice(i,1);
        return this;
    },


    /**
     * makes the validation wait the alotted time from the last keystroke
     */
    deferValidation: function(e){
      if (this.wait >= 300)
          this.removeMessageAndFieldClass();
      if (this.timeout)
          clearTimeout(this.timeout);
      const self = this;
      this.timeout = setTimeout( function(){ self.validate(); }, this.wait);
    },

    /**
     * sets the focused flag to false when field loses focus
     */
    doOnBlur: function(e){
      this.focused = false;
      this.validate();
    },

    /**
     * sets the focused flag to true when field gains focus
     */
    doOnFocus: function(e){
      this.focused = true;
      this.removeMessageAndFieldClass();
    },

    /**
     *  gets the type of element, to check whether it is compatible
     *
     *  @var validationFunction {Function} - validation function to be used (ie Validate.Presence )
     *  @var validationParamsObj {Object} - parameters for doing the validation, if wanted or necessary
     */
    getElementType: function(){
        const nodeName = this.element.nodeName.toUpperCase();
        if (nodeName == 'TEXTAREA')
            return LiveValidation.TEXTAREA;
        if (nodeName == 'INPUT' && this.element.type.toUpperCase() == 'TEXT')
            return LiveValidation.TEXT;
        if (nodeName == 'INPUT' && this.element.type.toUpperCase() == 'PASSWORD')
            return LiveValidation.PASSWORD;
        if (nodeName == 'INPUT' && this.element.type.toUpperCase() == 'CHECKBOX')
            return LiveValidation.CHECKBOX;
        if (nodeName == 'INPUT' && this.element.type.toUpperCase() == 'FILE')
            return LiveValidation.FILE;
        if (nodeName == 'INPUT' && this.element.type.toUpperCase() == 'RADIO')
            return LiveValidation.RADIO;
        if (nodeName == 'INPUT' && this.element.type.toUpperCase() == 'EMAIL')
            return LiveValidation.TEXT;
        if (nodeName == 'INPUT' && this.element.type.toUpperCase() == 'TEL')
            return LiveValidation.TEXT;
        if (nodeName == 'INPUT' && this.element.type.toUpperCase() == 'NUMBER')
            return LiveValidation.TEXT;
        if (nodeName == 'INPUT' && this.element.type.toUpperCase() == 'URL')
            return LiveValidation.TEXT;
        if (nodeName == 'INPUT' && this.element.type.toUpperCase() == 'HIDDEN')
            return LiveValidation.TEXT;
        if (nodeName == 'INPUT' && this.element.type.toUpperCase() == 'COLOR')
            return LiveValidation.TEXT;
        if (nodeName == 'INPUT' && this.element.type.toUpperCase() == 'DATE')
            return LiveValidation.TEXT;
        if (nodeName == 'INPUT' && this.element.type.toUpperCase() == 'DATETIME-LOCAL')
            return LiveValidation.TEXT;
        if (nodeName == 'INPUT' && this.element.type.toUpperCase() == 'MONTH')
            return LiveValidation.TEXT;
        if (nodeName == 'INPUT' && this.element.type.toUpperCase() == 'WEEK')
            return LiveValidation.TEXT;
        if (nodeName == 'INPUT' && this.element.type.toUpperCase() == 'TIME')
            return LiveValidation.TEXT;
        if (nodeName == 'INPUT' && this.element.type.toUpperCase() == 'RANGE')
            return LiveValidation.TEXT;
        if (nodeName == 'INPUT' && this.element.type.toUpperCase() == 'SEARCH')
            return LiveValidation.TEXT;
        if (nodeName == 'SELECT')
            return LiveValidation.SELECT;
        if (nodeName == 'FORM')
            return LiveValidation.FORM;
        if (nodeName == 'INPUT')
            throw new Error('LiveValidation::getElementType - Cannot use LiveValidation on an ' + this.element.type + ' input!');
        throw new Error('LiveValidation::getElementType - Element must be an input, select, or textarea!');
    },

    /**
     * Loops through all the validations added to the LiveValidation object and checks them one by one
     *
     * @var validationFunction {Function} - validation function to be used (ie Validate.Presence )
     * @var validationParamsObj {Object} - parameters for doing the validation, if wanted or necessary
     * @return {Boolean} - whether the all the validations passed or if one failed
     */
    doValidations: function(isSubmit, submitTrigger){
        let result = true;

        this.validationFailed = false;
        this.validationAsync = false;
        for(let i = 0, len = this.validations.length; i < len; ++i){
            const validation = this.validations[i];
            switch(validation.type){
                case Validate.Presence:
                case Validate.Confirmation:
                case Validate.Acceptance:
                case Validate.Custom:
                  this.displayMessageWhenEmpty = true;
                  break;
                default:
                  break;
            }
            const v = this.validateElement(validation.type, validation.params, isSubmit, submitTrigger);
            if (v === 'async') {
                this.validationAsync = true;
                result = 'async';
            } else if (!v) {
                this.validationFailed = true;
                return false;
            }
        }
        this.message = this.validMessage;
        return result;
    },

    /**
     * Check if there is an async validation.
     */
    isAsync: function (){
        for(let i = 0, len = this.validations.length; i < len; ++i) {
            const validation = this.validations[i];
            if (validation.type == Validate.Postback || validation.params.isAsync === true)
                return true;
        }
        return false;
    },

    /**
     * Performs validation on the element and handles any error (validation or otherwise) it throws up
     *
     * @var validationFunction {Function} - validation function to be used (ie Validate.Presence )
     * @var validationParamsObj {Object} - parameters for doing the validation, if wanted or necessary
     * @var isSubmit {Boolean} - is this a form submit or an individual field check
     * @var submitTrigger {Object} - the element that triggered the submit
     * @return {Boolean} or {"async"} - whether the validation has passed, failed or waits for an async server side check
     */
    validateElement: function(validationFunction, validationParamsObj, isSubmit, submitTrigger){
        if (!this.element.disabled) {
            let value = this.getValue();
            if(validationFunction == Validate.Acceptance){
                if(this.elementType != LiveValidation.CHECKBOX)
                    throw new Error('LiveValidation::validateElement - Element to validate acceptance must be a checkbox!');
                value = this.element.checked;
            }
            let isValid = true;
            try {
                isValid = validationFunction(value, validationParamsObj, isSubmit, submitTrigger);
                if (isValid === 'async') {
                    this.validationAsync = true;
                }
            }
            catch(error) {
                if(error instanceof Validate.Error){
                    if( value !== '' || (value === '' && this.displayMessageWhenEmpty) ){
                        this.validationFailed = true;
                        this.message = error.message;
                        isValid = false;
                    }
                } else {
                    throw error;
                }
            }
            return isValid;
        } else {
            return true;
        }
    },


    getValue: function() {
        switch (this.elementType) {
        case LiveValidation.SELECT:
            if (this.element.selectedIndex >= 0) return this.element.options[this.element.selectedIndex].value;
            else return "";
        case LiveValidation.RADIO:
            return $('input[name="'+this.element.name+'"]:checked').val();
        case LiveValidation.CHECKBOX:
            const val = [];
            $('input[name="'+this.element.name+'"]:checked').each(function() { val.push($(this).val()); });
            if (val.length === 0) {
                return undefined;
            } else {
                return val;
            }
        case LiveValidation.FORM:
            return "";
        default:
            return this.element.value;
        }
    },

    /**
     * Do all the validations and fires off the onValid or onInvalid callbacks
     *
     * @var isSubmit {Boolean} - is this a form submit or an individual field check
     * @var submitTrigger {Object} - the element that triggered the submit
     * @return {Boolean} or "async" - whether all the validations passed or if one failed
     */
    validate: function(isSubmit, submitTrigger){
        if(!this.element.disabled) {
            const valid = this.doValidations(isSubmit, submitTrigger);
            if (valid === 'async') {
                this.onAsync();
                return 'async';
            } else if (valid) {
                this.onValid();
                return true;
            } else {
                this.onInvalid();
                return false;
            }
        } else {
            return true;
        }
    },

    /**
     * Called when there is an async validation result.
     * The caller has already checked if the current input value hasn't changed.
     */
    asyncValidationResult: function(isValid){
        if (this.validationAsync){
            // Find which validation was waiting for async, assume only one async postback per field.
            for(let i = 0, len = this.validations.length; i < len; ++i){
                const validation = this.validations[i];
                if(validation.type == Validate.Postback || validation.params.isAsync === true) {
                    // Clear the async wait flag
                    this.validationAsync = false;
                    this.validationFailed = !isValid;
                    if (isValid){
                        this.onValid();
                    } else {
                        this.showErrorMessage(validation.params?.failureMessage);
                        this.onInvalid();
                    }
                    if (this.formObj) {
                        this.formObj.asyncResult(this, isValid);
                    }
                }
            }
        }
    },

    /**
     *  enables the field
     *
     *  @return {LiveValidation} - the LiveValidation object for chaining
     */
    enable: function(){
        this.element.disabled = false;
        return this;
    },

    /**
     *  disables the field and removes any message and styles associated with the field
     *
     *  @return {LiveValidation} - the LiveValidation object for chaining
     */
    disable: function(){
        this.element.disabled = true;
        this.removeMessageAndFieldClass();
        return this;
    },

    /** Message insertion methods ****************************
     *
     * These are only used in the onValid and onInvalid callback functions and so if you overide the default callbacks,
     * you must either impliment your own functions to do whatever you want, or call some of these from them if you
     * want to keep some of the functionality
     */

     /**
      *  makes a span containing a spinner image
      *
      * @return {HTMLSpanObject} - a span element with the message in it
      */
     createSpinnerSpan: function(){
         const span = document.createElement('span');
         span.innerHTML = '<img src="/lib/images/spinner.gif" height="16" width="16" alt="Validating..." />';
         return span;
     },

    /**
     *  makes a span containg the passed or failed message
     *
     * @return {HTMLSpanObject} - a span element with the message in it
     */
    createMessageSpan: function(){
        if (!this.message) {
            return null;
        }
        const span = document.createElement('span');
        span.appendChild(document.createTextNode(this.message));
        return span;
    },

    /**
     * Show an error message
     */
    showErrorMessage: function(message){
        this.message = message;
        this.onInvalid();
    },

    /**
     * Insert a spinner in the message element.
     */
    insertSpinner: function (elementToInsert){
        this.removeMessage();
        if( (this.displayMessageWhenEmpty && (this.elementType == LiveValidation.CHECKBOX || this.element.value === ''))
          || this.element.value !== '' ){

          elementToInsert.className += ' ' + this.messageClass + ' ' + this.asyncFieldClass;
          if(this.insertAfterWhatNode.nextSibling){
              this.insertAfterWhatNode.parentNode.insertBefore(elementToInsert, this.insertAfterWhatNode.nextSibling);
          }else{
              this.insertAfterWhatNode.parentNode.appendChild(elementToInsert);
          }
        }

    },

    /**
     *  inserts the element containing the message in place of the element that already exists (if it does)
     *
     * @var elementToIsert {HTMLElementObject} - an element node to insert
     */
    insertMessage: function(elementToInsert){
        this.removeMessage();
        if (elementToInsert) {
          if( (this.displayMessageWhenEmpty && (this.elementType == LiveValidation.CHECKBOX || this.element.value === ''))
            || this.element.value !== '' ) {

              const className = this.validationFailed ? this.invalidClass : this.validClass;
              elementToInsert.className += ' ' + this.messageClass + ' ' + className;
              if(this.insertAfterWhatNode.nextSibling){
                  this.insertAfterWhatNode.parentNode.insertBefore(elementToInsert, this.insertAfterWhatNode.nextSibling);
              }else{
                  this.insertAfterWhatNode.parentNode.appendChild(elementToInsert);
              }
          }
        }
    },


    /**
     *  changes the class of the field based on whether it is valid or not
     */
    addFieldClass: function(){
        this.removeFieldClass();
        const fieldGroup = $(this.element).closest('.' + this.fieldGroupClass);
        if (!this.validationFailed){
            if (this.displayMessageWhenEmpty || this.element.value !== ''){
                fieldGroup.addClass(this.fieldGroupSuccessClass);
                $("."+this.fieldInValidElementClass, fieldGroup).hide();
                $("."+this.fieldValidElementClass, fieldGroup).fadeIn();

                $('label[for="'+this.element.id+'"]')
                    .addClass(this.validFieldClass);

                switch (this.elementType) {
                    case LiveValidation.RADIO:
                    case LiveValidation.CHECKBOX:
                        $('input[name="'+this.element.name+'"]')
                          .addClass(this.validFieldClass)
                          .closest('label')
                          .addClass(this.validFieldClass);
                        break;
                    case LiveValidation.FORM:
                        break;
                    default:
                        $(this.element)
                          .addClass(this.validFieldClass);
                        break;
                    }
            }
        } else {
            fieldGroup.addClass(this.fieldGroupErrorClass);
            $("."+this.fieldValidElementClass, fieldGroup).hide();
            $("."+this.fieldInValidElementClass, fieldGroup).fadeIn();

            $('label[for="'+this.element.id+'"]')
                .removeClass(this.validFieldClass)
                .addClass(this.invalidFieldClass);

            switch (this.elementType) {
                case LiveValidation.RADIO:
                case LiveValidation.CHECKBOX:
                    $('input[name="'+this.element.name+'"]')
                      .addClass(this.invalidFieldClass)
                      .closest('label')
                      .addClass(this.invalidFieldClass);
                    break;
                case LiveValidation.FORM:
                    break;
                default:
                    $(this.element)
                      .addClass(this.invalidFieldClass);
                    break;
            }
        }
    },

    /**
     *  removes the message element if it exists, so that the new message will replace it
     */
    removeMessage: function(){
      let nextEl;
      let el = this.insertAfterWhatNode;
      while(el.nextSibling){
          if(el.nextSibling.nodeType === 1){
            nextEl = el.nextSibling;
            break;
        }
        el = el.nextSibling;
      }
      if(nextEl && nextEl.className.indexOf(this.messageClass) != -1)
        this.insertAfterWhatNode.parentNode.removeChild(nextEl);
    },

    /**
     *  removes the class that has been applied to the field to indicate if valid or not
     */
    removeFieldClass: function(){
        $(this.element)
          .closest('.' + this.fieldGroupClass)
          .removeClass(this.fieldGroupSuccessClass)
          .removeClass(this.fieldGroupErrorClass);

        $('label[for="'+this.element.id+'"]')
          .removeClass(this.validFieldClass)
          .removeClass(this.invalidFieldClass);

        switch (this.elementType) {
        case LiveValidation.RADIO:
        case LiveValidation.CHECKBOX:
            $('input[name="'+this.element.name+'"]')
              .closest('label')
              .removeClass(this.invalidFieldClass)
              .removeClass(this.validFieldClass);
            break;
        case LiveValidation.FORM:
            break;
        default:
            $(this.element)
              .removeClass(this.invalidFieldClass)
              .removeClass(this.validFieldClass);
            break;
        }
    },

    /**
     *  removes the message and the field class
     */
    removeMessageAndFieldClass: function(){
      this.removeMessage();
      this.removeFieldClass();
    }

}; // end of LiveValidation class




/*************************************** LiveValidationForm class ****************************************/
/**
 * This class is used internally by LiveValidation class to associate a LiveValidation field with a form it is icontained in one
 *
 * It will therefore not really ever be needed to be used directly by the developer, unless they want to associate a LiveValidation
 * field with a form that it is not a child of
 */

/**
   *  handles validation of LiveValidation fields belonging to this form on its submittal
   *
   *  @var element {HTMLFormElement} - a dom element reference to the form to turn into a LiveValidationForm
   */
var LiveValidationForm = function(element){
  this.initialize(element);
};

/**
   *  gets the instance of the LiveValidationForm if it has already been made or creates it if it doesnt exist
   *
   *  @var element {HTMLFormElement} - a dom element reference to a form
   */
LiveValidationForm.getInstance = function(element){
  if(!$(element).attr("id")) {
      const rand = Math.random() * Math.random();
      $(element).attr("id", 'formId_' + rand.toString().replace(/\./, '') + new Date().valueOf());
  }
  let instance = $(element).data("z_live_validation_instance");
  if (!instance) {
      instance = new LiveValidationForm(element);
      $(element).data("z_live_validation_instance", instance);
  }
  return instance;
};

LiveValidationForm.prototype = {
  validFormClass: 'z_form_valid',
  invalidFormClass: 'z_form_invalid',

  ifValidFormClass: 'if-form-validated',
  ifInvalidFormClass: 'if-form-error',

  /**
   *  constructor for LiveValidationForm - handles validation of LiveValidation fields belonging to this form on its submittal
   *
   *  @var element {HTMLFormElement} - a dom element reference to the form to turn into a LiveValidationForm
   */
  initialize: function(element){
    this.name = $(element).attr("id");
    this.element = element;
    this.skipValidations = 0;
    this.submitEvent = undefined;
    this.submitWaitForAsync = [];
    this.isWaitForFormAsync = false;
    this.clk = undefined;

    // Preserve the old onsubmit event - used when destroying the LiveValidationForm
    this.oldOnSubmit = this.element.onsubmit || function(){};
    const self = this;

    this.onInvalid = function() {
        $(this).removeClass(self.validFormClass).addClass(self.invalidFormClass);
        $(`.${self.validFormClass}, .${self.ifValidFormClass}`, this).hide();
        $(`.${self.invalidFormClass}, .${self.ifInvalidFormClass}`, this).fadeIn();
    };
    this.onValid = function() {
        $(this).removeClass(self.invalidFormClass).addClass(self.validFormClass);
        $(`.${self.invalidFormClass}, .${self.ifInvalidFormClass}`, this).hide();
        $(`.${self.validFormClass}, .${self.ifValidFormClass}`, this).fadeIn();
    };

    this.onValidSubmit = function(event, form) {
        return true;
    };

    this.onBeforeSubmit = function(event, form) {
        return true;
    };

    this.element.onsubmit = function(event) {
        self.onBeforeSubmit(event, this);

        event.preventDefault();

        let result = true;
        const fields = self.getFields();

        self.submitEvent = event;
        self.submitWaitForAsync = [];
        self.isWaitForFormAsync = false;

        for(let i = 0, len = fields.length; i < len; ++i ) {
            if (!fields[i].element.disabled) {
                const ve = fields[i].validate(true, self.element.clk);
                if (ve === 'async') {
                    self.submitWaitForAsync.push(fields[i]);
                } else if (!ve) {
                    result = false;
                }
            }
        }
        if (result === false) {
            self.submitWaitForAsync = [];
        }

        // Optionally check validations attached to the form itself.
        // Only done if all other validations are done and passing.
        if (result && self.submitWaitForAsync.length === 0) {
            const formValidation = $(this).data('z_live_validation');
            if (formValidation) {
                const vf = formValidation.validate(true, self.element.clk);
                if (vf === 'async') {
                    self.submitWaitForAsync = [formValidation];
                    self.isWaitForFormAsync = true;
                } else if (!vf) {
                    result = false;
                }
            }
        }
        if (self.submitWaitForAsync.length > 0) {
            event.stopImmediatePropagation();
            return false;
        } else if (!result) {
            self.onInvalid.call(this);
            event.stopImmediatePropagation();
            return false;
        } else {
            self.onValid.call(this);
            return self.onValidSubmit(event, self.element);
        }
    };
  },

  /**
   *  destroy this instance and its events
   *
   * @var force {Boolean} - whether to force the destruction even if there are fields still associated
   */
  destroy: function(force){
    if (force || this.getFields().length === 0) {
        // remove events - set back to previous events
        this.element.onsubmit = this.oldOnSubmit;
        // remove from the instances namespace
        $(this.element).removeData("z_live_validation_instance");
        return true;
    } else {
        return false;
    }
  },

  /**
   * get the to-be-validated fields
   */
  getFields: function() {
    let fields = [];
    $(this.element.elements).each(function() {
        const field = $(this).data('z_live_validation');
        if (field) {
            fields.push(field);
        }
    });
    return fields;
  },

  /**
   * Handle receiving an async validation result for the form.
   * Check if the form is ready to be submitted.
   * The form is 'this'.
   */
  asyncResult: function(Validation, isValid){
      const index = $.inArray(Validation, this.submitWaitForAsync);
      if (index >= 0) {
          if (isValid) {
              this.submitWaitForAsync.splice(index, 1);
              if (this.submitWaitForAsync.length === 0) {
                  let isFormValid;

                  if (!this.isWaitForFormAsync) {
                      const formValidation = $(this.element).data('z_live_validation');
                      if (formValidation) {
                          const result = formValidation.validate(true, this.clk);
                          if (result === 'async') {
                              this.submitWaitForAsync = [formValidation];
                              this.isWaitForFormAsync = true;
                              isFormValid = false;
                          } else if (!result) {
                              isFormValid = false;
                          } else {
                              isFormValid = true;
                          }
                      } else {
                          isFormValid = true;
                      }
                  } else {
                      this.isWaitForFormAsync = false;
                      isFormValid = true;
                  }

                  if (!this.isWaitForFormAsync) {
                      const event = this.submitEvent;
                      this.submitEvent = undefined;

                      if (isFormValid){
                          this.onValid.call(this.element);
                          this.onValidSubmit(event, this.element);
                      } else {
                          this.onInvalid.call(this.element);
                      }
                  }
              }
          } else {
              this.submitWaitForAsync = [];
              this.isWaitForFormAsync = false;
              this.submitEvent = undefined;
              this.onInvalid.call(this.element);
          }
      }
    }
}; // end of LiveValidationForm prototype


/*************************************** Validate class ****************************************/
/**
 * This class contains all the methods needed for doing the actual validation itself
 *
 * All methods are static so that they can be used outside the context of a form field
 * as they could be useful for validating stuff anywhere you want really
 *
 * All of them will return true if the validation is successful, but will raise a ValidationError if
 * they fail, so that this can be caught and the message explaining the error can be accessed ( as just
 * returning false would leave you a bit in the dark as to why it failed )
 *
 * Can use validation methods alone and wrap in a try..catch statement yourself if you want to access the failure
 * message and handle the error, or use the Validate::now method if you just want true or false
 */

var Validate = {

    /**
     *  validates that the field has been filled in
     *
     *  @var value {mixed} - value to be checked
     *  @var paramsObj {Object} - parameters for this particular validation, see below for details
     *
     *  paramsObj properties:
     *              failureMessage {String} - the message to show when the field fails validation
     *                            (DEFAULT: "Can't be empty!")
     */
    Presence: function(value, paramsObj){
        paramsObj = paramsObj || {};
        if(value === '' || value === null || value === undefined){
            Validate.fail(paramsObj.failureMessage || "");
        }
        return true;
    },

    /**
     *  validates that the value is numeric, does not fall within a given range of numbers
     *
     *  @var value {mixed} - value to be checked
     *  @var paramsObj {Object} - parameters for this particular validation, see below for details
     *
     *  paramsObj properties:
     *              notANumberMessage {String} - the message to show when the validation fails when value is not a number
     *                                (DEFAULT: "Must be a number!")
     *              notAnIntegerMessage {String} - the message to show when the validation fails when value is not an integer
     *                                (DEFAULT: "Must be a number!")
     *              wrongNumberMessage {String} - the message to show when the validation fails when is param is used
     *                                (DEFAULT: "Must be {is}!")
     *              tooLowMessage {String}    - the message to show when the validation fails when minimum param is used
     *                                (DEFAULT: "Must not be less than {minimum}!")
     *              tooHighMessage {String}   - the message to show when the validation fails when maximum param is used
     *                                (DEFAULT: "Must not be more than {maximum}!")
     *              is {Int}          - the length must be this long
     *              minimum {Int}         - the minimum length allowed
     *              maximum {Int}         - the maximum length allowed
     *                         onlyInteger {Boolean} - if true will only allow integers to be valid
     *                                                             (DEFAULT: false)
     *
     *  NB. can be checked if it is within a range by specifying both a minimum and a maximum
     *  NB. will evaluate numbers represented in scientific form (ie 2e10) correctly as numbers
     */
    Numericality: function(value, paramsObj){
        const suppliedValue = value;
        value = Number(value);
        paramsObj = paramsObj || {};
        const minimum = ((paramsObj.minimum) || (paramsObj.minimum === 0)) ? paramsObj.minimum : null;
        const maximum = ((paramsObj.maximum) || (paramsObj.maximum === 0)) ? paramsObj.maximum : null;
        const is = ((paramsObj.is) || (paramsObj.is === 0)) ? paramsObj.is : null;
        const notANumberMessage = paramsObj.notANumberMessage || "Must be a number.";
        const notAnIntegerMessage = paramsObj.notAnIntegerMessage || "Must be an integer.";
        const wrongNumberMessage = paramsObj.wrongNumberMessage || "Must be " + is + ".";
        const tooLowMessage = paramsObj.tooLowMessage || "Must not be less than " + minimum + ".";
        const tooHighMessage = paramsObj.tooHighMessage || "Must not be more than " + maximum + ".";

        if (!isFinite(value))
            Validate.fail(notANumberMessage);
        if (paramsObj.onlyInteger && (/\.0+$|\.$/.test(String(suppliedValue))  || value != parseInt(value,10)) )
            Validate.fail(notAnIntegerMessage);
        switch(true){
            case (is !== null):
                if( value != Number(is) ) Validate.fail(wrongNumberMessage);
                break;
            case (minimum !== null && maximum !== null):
                Validate.Numericality(value, {tooLowMessage: tooLowMessage, minimum: minimum});
                Validate.Numericality(value, {tooHighMessage: tooHighMessage, maximum: maximum});
                break;
            case (minimum !== null):
                if( value < Number(minimum) ) Validate.fail(tooLowMessage);
                break;
            case (maximum !== null):
                if( value > Number(maximum) ) Validate.fail(tooHighMessage);
                break;
        }
        return true;
    },

    /**
     *  validates against a RegExp pattern
     *
     *  @var value {mixed} - value to be checked
     *  @var paramsObj {Object} - parameters for this particular validation, see below for details
     *
     *  paramsObj properties:
     *              failureMessage {String} - the message to show when the field fails validation
     *                            (DEFAULT: "")
     *              pattern {RegExp}    - the regular expression pattern
     *                            (DEFAULT: /./)
     *             negate {Boolean} - if set to true, will validate true if the pattern is not matched
   *                           (DEFAULT: false)
     *
     *  NB. will return true for an empty string, to allow for non-required, empty fields to validate.
     *    If you do not want this to be the case then you must either add a LiveValidation.PRESENCE validation
     *    or build it into the regular expression pattern
     */
    Format: function(value, paramsObj){
      value = String(value);
      paramsObj = paramsObj || {};
      const message = paramsObj.failureMessage || "";
      const pattern = paramsObj.pattern || /./;
      const negate = paramsObj.negate || false;
      if (!negate && !pattern.test(value)) {
        Validate.fail(message); // normal
      }
      if (negate && pattern.test(value)) {
        Validate.fail(message); // negated
      }
      return true;
    },

    /**
     *  validates that the field contains a valid email address
     *
     *  @var value {mixed} - value to be checked
     *  @var paramsObj {Object} - parameters for this particular validation, see below for details
     *
     *  paramsObj properties:
     *              failureMessage {String} - the message to show when the field fails validation
     *                            (DEFAULT: "Must be a number!" or "Must be an integer!")
     */
    Email: function(value, paramsObj){
      paramsObj = paramsObj || {};
      const message = paramsObj.failureMessage || "";
      value = $.trim(value);
      // see validator_base_email.erl:43
      const re = /^$|^(("[^"\f\n\r\t\v\b]+")|([\w\!\#\$\%\&\'\*\+\-\~\/\^\`\|\{\}]+(\.[\w\!\#\$\%\&\'\*\+\-\~\/\^\`\|\{\}]+)*))@((([A-Za-z0-9\-])+\.)+[A-Za-z\-]{2,})$/;
      Validate.Format(value, { failureMessage: message, pattern: re } );
      return true;
    },

    /**
     *  validates that the field contains a valid JSON
     *
     *  @var value {mixed} - value to be checked
     *  @var paramsObj {Object} - parameters for this particular validation, see below for details
     *
     *  paramsObj properties:
     *              failureMessage {String} - the message to show when the field fails validation
     *                            (DEFAULT: "Must be a number!" or "Must be an integer!")
     */
    Json: function(value, paramsObj){
      paramsObj = paramsObj || {};
      const message = paramsObj.failureMessage || "";
      value = $.trim(value);

      try {
          JSON.parse(value);
          return true
      } catch (error) {
          Validate.fail(message);
      }
    },

    /*
     *  validates that the field contains a valid date
     *
     *  @var value {mixed} - value to be checked
     *  @var paramsObj {Object} - parameters for this particular validation, see below for details
     *
     *  paramsObj properties:
     *              failureMessage {String} - the message to show when the field fails validation
     *                            (DEFAULT: "Incorrect Date")
     *              format {String} - l, m,b endian
     *                             (DEFAULT: "l")
     *              separator {String} - a character which is not a number
     *                             (DEFAULT: "-")
     *
     */

    Date: function(value, paramsObj){
      function to_integer(value) {
          if (parseInt(value, 10) == value) {
              return parseInt(value, 10);
          } else {
              return parseInt("NaN", 10);
          }
      }

      paramsObj = paramsObj || {};
      const message = paramsObj.failureMessage || "Incorrect Date";
      const format = paramsObj.format || "l";
      const separator = paramsObj.separator || "-";
      value = $.trim(value);

      const date_components = value.split(separator);

      if (date_components.length != 3) {
          Validate.fail(message);
      } else {
          let day;
          let month;
          let year;

          not_a_number = to_integer(separator);
          if (!isNaN(not_a_number)) {
              throw "Seperator cannot be a number!";
          }
          if (format == 'l') {
              day = to_integer(date_components[0]);
              month = to_integer(date_components[1]);
              year = to_integer(date_components[2]);
          } else if (format == 'b') {
              day = to_integer(date_components[2]);
              month = to_integer(date_components[1]);
              year = to_integer(date_components[0]);
          } else if (format == 'm') {
              day = to_integer(date_components[1]);
              month = to_integer(date_components[0]);
              year = to_integer(date_components[2]);
          } else {
              throw "Bad date format error! Must be one of: l, b or m";
          }
          const date_object = new Date(year, month-1, day);
          if (!((date_object.getDate() == day) && (date_object.getMonth()+1 == month) && (date_object.getFullYear() == year))) {
              Validate.fail(message);
          }
      }
      return true;
    },

    /**
     *  validates the length of the value
     *
     *  @var value {mixed} - value to be checked
     *  @var paramsObj {Object} - parameters for this particular validation, see below for details
     *
     *  paramsObj properties:
     *              wrongLengthMessage {String} - the message to show when the fails when is param is used
     *                                (DEFAULT: "Must be {is} characters long!")
     *              tooShortMessage {String}  - the message to show when the fails when minimum param is used
     *                                (DEFAULT: "Must not be less than {minimum} characters long!")
     *              tooLongMessage {String}   - the message to show when the fails when maximum param is used
     *                                (DEFAULT: "Must not be more than {maximum} characters long!")
     *              is {Int}          - the length must be this long
     *              minimum {Int}         - the minimum length allowed
     *              maximum {Int}         - the maximum length allowed
     *
     *  NB. can be checked if it is within a range by specifying both a minimum and a maximum
     */
    Length: function(value, paramsObj){
        value = String(value);
        paramsObj = paramsObj || {};
        const minimum = ((paramsObj.minimum) || (paramsObj.minimum === 0)) ? paramsObj.minimum : null;
        const maximum = ((paramsObj.maximum) || (paramsObj.maximum === 0)) ? paramsObj.maximum : null;
        const is = ((paramsObj.is) || (paramsObj.is === 0)) ? paramsObj.is : null;
        const wrongLengthMessage = paramsObj.wrongLengthMessage || "Must be " + is + " characters long.";
        const tooShortMessage = paramsObj.tooShortMessage || "Must not be less than " + minimum + " characters long.";
        const tooLongMessage = paramsObj.tooLongMessage || "Must not be more than " + maximum + " characters long.";
        switch(true){
            case (is !== null):
                if( value.length != Number(is) ) Validate.fail(wrongLengthMessage);
                break;
            case (minimum !== null && maximum !== null):
                Validate.Length(value, {tooShortMessage: tooShortMessage, minimum: minimum});
                Validate.Length(value, {tooLongMessage: tooLongMessage, maximum: maximum});
                break;
            case (minimum !== null):
                if( value.length < Number(minimum) ) Validate.fail(tooShortMessage);
                break;
            case (maximum !== null):
                if( value.length > Number(maximum) ) Validate.fail(tooLongMessage);
                break;
            default:
                throw new Error("Validate::Length - Length(s) to validate against must be provided");
        }
        return true;
    },

    /**
     *  validates that the value falls within a given set of values
     *
     *  @var value {mixed} - value to be checked
     *  @var paramsObj {Object} - parameters for this particular validation, see below for details
     *
     *  paramsObj properties:
     *              failureMessage {String} - the message to show when the field fails validation
     *                            (DEFAULT: "Must be included in the list!")
     *              within {Array}      - an array of values that the value should fall in
     *                            (DEFAULT: [])
     *              allowNull {Bool}    - if true, and a null value is passed in, validates as true
     *                            (DEFAULT: false)
     *             partialMatch {Bool}  - if true, will not only validate against the whole value to check but also if it is a substring of the value
     *                            (DEFAULT: false)
     *             caseSensitive {Bool} - if false will compare strings case insensitively
     *                          (DEFAULT: true)
     *             negate {Bool}    - if true, will validate that the value is not within the given set of values
     *                            (DEFAULT: false)
     */
    Inclusion: function(value, paramsObj){
      paramsObj = paramsObj || {};
      const message = paramsObj.failureMessage || "Must be included in the list!";
      const caseSensitive = (paramsObj.caseSensitive === false) ? false : true;
      if(paramsObj.allowNull && value === null)
        return true;
      if(!paramsObj.allowNull && value === null)
        Validate.fail(message);
      const within = paramsObj.within || [];
      let length;

      //if case insensitive, make all strings in the array lowercase, and the value too
      if(!caseSensitive){
        const lowerWithin = [];
        length = within.length;
        for(let j = 0; j < length; ++j){
          let item = within[j];
          if(typeof item == 'string')
            item = item.toLowerCase();
          lowerWithin.push(item);
        }
        within = lowerWithin;
        if(typeof value == 'string')
          value = value.toLowerCase();
      }
      let found = false;
      length = within.length;
      for(let i = 0; i < length; ++i){
        if(within[i] == value) found = true;
        if(paramsObj.partialMatch){
          if(value.indexOf(within[i]) != -1) found = true;
        }
      }
      if( (!paramsObj.negate && !found) || (paramsObj.negate && found) )
        Validate.fail(message);
      return true;
    },

    /**
     *  validates that the value does not fall within a given set of values
     *
     *  @var value {mixed} - value to be checked
     *  @var paramsObj {Object} - parameters for this particular validation, see below for details
     *
     *  paramsObj properties:
     *              failureMessage {String} - the message to show when the field fails validation
     *                            (DEFAULT: "Must not be included in the list!")
     *              within {Array}      - an array of values that the value should not fall in
     *                            (DEFAULT: [])
     *              allowNull {Bool}    - if true, and a null value is passed in, validates as true
     *                            (DEFAULT: false)
     *             partialMatch {Bool}  - if true, will not only validate against the whole value to check but also if it is a substring of the value
     *                            (DEFAULT: false)
     *             caseSensitive {Bool} - if false will compare strings case insensitively
     *                          (DEFAULT: true)
     */
    Exclusion: function(value, paramsObj){
      paramsObj = paramsObj || {};
      paramsObj.failureMessage = paramsObj.failureMessage || "Must not be included in the list";
      paramsObj.negate = true;
      Validate.Inclusion(value, paramsObj);
      return true;
    },

    /**
     *  validates that the value matches that in another field
     *
     *  @var value {mixed} - value to be checked
     *  @var paramsObj {Object} - parameters for this particular validation, see below for details
     *
     *  paramsObj properties:
     *              failureMessage {String} - the message to show when the field fails validation
     *                            (DEFAULT: "Does not match!")
     *              match {String}      - id of the field that this one should match
     */
    Confirmation: function(value, paramsObj){
        if(!paramsObj.match)
            throw new Error("Validate::Confirmation - Error validating confirmation: Id of element to match must be provided");
        paramsObj = paramsObj || {};
        const message = paramsObj.failureMessage || "Does not match.";
        const match = paramsObj.match.nodeName ? paramsObj.match : document.getElementById(paramsObj.match);
        if(!match)
            throw new Error("Validate::Confirmation - There is no reference with name of, or element with id of '" + paramsObj.match + "'");
        if(value != match.value){
          Validate.fail(message);
        }
        return true;
    },

    /**
     *  validates that the value is true (for use primarily in detemining if a checkbox has been checked)
     *
     *  @var value {mixed} - value to be checked if true or not (usually a boolean from the checked value of a checkbox)
     *  @var paramsObj {Object} - parameters for this particular validation, see below for details
     *
     *  paramsObj properties:
     *              failureMessage {String} - the message to show when the field fails validation
     *                            (DEFAULT: "Must be accepted!")
     */
    Acceptance: function(value, paramsObj){
        paramsObj = paramsObj || {};
        const message = paramsObj.failureMessage || "Must be accepted.";
        if(!value){
          Validate.fail(message);
        }
        return true;
    },

   /**
     *  validates against a custom function that returns true or false (or throws a Validate.Error) when passed the value
     *
     *  @var value {mixed} - value to be checked
     *  @var paramsObj {Object} - parameters for this particular validation, see below for details
     *
     *  paramsObj properties:
     *              failureMessage {String} - the message to show when the field fails validation
     *                            (DEFAULT: "")
     *              against {Function}      - a function that will take the value and object of arguments and return true or false
     *                            (DEFAULT: function(){ return true; })
     *              args {Object}     - an object of named arguments that will be passed to the custom function so are accessible through this object within it
     *                            (DEFAULT: {})
     */
    Custom: function(value, paramsObj, isSubmit, submitTrigger){
        paramsObj = paramsObj || {};
        const against = paramsObj.against || function(){ return true; };
        const args = paramsObj.args || {};
        const message = paramsObj.failureMessage || "";
        let result;

        if (typeof against == "string") {
            result = z_call_function_by_name(against, window, value, args, isSubmit, submitTrigger);
        } else {
            result = against(value, args, isSubmit, submitTrigger);
        }
        if (result === 'async') {
          return 'async';
        } else {
          if(!result) Validate.fail(message);
          return true;
        }
    },


    /**
     * Performs a postback, delays the check till the postback is returned. till then a spinner is shown
     * next to the input element.
     */
    Postback: function(value, paramsObj, isSubmit, submitTrigger) {
        paramsObj = paramsObj || {};
        const against = paramsObj.against || function(){ return true; };
        const args = paramsObj.args || {};
        const message = paramsObj.failureMessage || "";

        if (!against(value, args, isSubmit, submitTrigger)) {
            Validate.fail(message);
        } else if (paramsObj.z_postback) {
            // Perform the async postback
            const extraParams = [];
            if (isSubmit) {
                extraParams.push({name: 'z_submitter', value: (submitTrigger && submitTrigger.name) ? submitTrigger.name : ''});
            }
            z_queue_postback(paramsObj.z_id, paramsObj.z_postback, extraParams);
            return 'async';
        } else {
            return true;
        }
     },


    /**
     *  validates whatever it is you pass in, and handles the validation error for you so it gives a nice true or false reply
     *
     *  @var validationFunction {Function} - validation function to be used (ie Validation.validatePresence )
     *  @var value {mixed} - value to be checked if true or not (usually a boolean from the checked value of a checkbox)
     *  @var validationParamsObj {Object} - parameters for doing the validation, if wanted or necessary
     */
    now: function(validationFunction, value, validationParamsObj){
      if(!validationFunction)
        throw new Error("Validate::now - Validation function must be provided!");
      let isValid = true;
      try {
        validationFunction(value, validationParamsObj || {});
      } catch(error) {
        if (error instanceof Validate.Error) {
          isValid =  false;
        } else {
          throw error;
        }
      } finally {
        return isValid;
      }
    },

    /**
     * shortcut for failing throwing a validation error
     *
     *  @var errorMessage {String} - message to display
     */
    fail: function(errorMessage){
      throw new Validate.Error(errorMessage);
    },

    Error: function(errorMessage){
      this.message = errorMessage;
      this.name = 'ValidationError';
    }

};
