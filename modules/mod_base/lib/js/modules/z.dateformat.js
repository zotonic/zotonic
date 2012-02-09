function is_leap_year(year) {
    if (year % 4 == 0) {
        if (year % 100 == 0) {
            if (year % 400 == 0) {
                return true;
            } else {
                return false;
            }
        } else {
            return true;
        }
    } else {
        return false;
    }
}

function to_integer(value) {
    if (parseInt(value) == value) {
        return parseInt(value);
    } else {
        return parseInt("NaN");
    }
}

function valid_date(my_date, format, separator) {
    var format = typeof(format) != 'undefined' ? format : 'l';
    var separator = typeof(separator) != 'undefined' ? separator : '-';
    var date_components = my_date.split(separator);

    if (date_components.length != 3) {
        return false;
    } else {
        not_a_number = to_integer(separator);
        if (!isNaN(not_a_number)) {
            throw "Seperator cannot be a number!";
        }
	    
        if (format == 'l') {
            var day = to_integer(date_components[0]);
            var month = to_integer(date_components[1]);
            var year = to_integer(date_components[2]);
        } else if (format == 'b') {
            var day = to_integer(date_components[2]);
            var month = to_integer(date_components[1]);
            var year = to_integer(date_components[0]);
        } else if (format == 'm') {
            var day = to_integer(date_components[1]);
            var month = to_integer(date_components[0]);
            var year = to_integer(date_components[2]);
        } else {
            throw "Bad date format error!";
        }
	    
        if (year >= 0) {
            if ( (month == 9 || month == 4 || month == 6 || month == 11) && day >= 1 && day <= 30) {
                return true;
            } else if ((month == 2  && day >= 1 && day <= 28) || (month == 2  && day == 29 && is_leap_year(year))) {
                return true;
            } else if ((month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12) && day >= 1 && day <= 31) {
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }
}