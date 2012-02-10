
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
        var date_object = new Date(year, month-1, day);
        if ((date_object.getDate() == day) && (date_object.getMonth()+1 == month) && (date_object.getFullYear() == year)) {
            return true;
        }
        else {
            return false;
        }
    }
}