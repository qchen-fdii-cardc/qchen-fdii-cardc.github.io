
function updateFactorialText() {
    var n = $("#num").val();
    if (n == "") {
        return;
    }
    try {
        n = parseInt(n);
    } catch (e) {
        return;
    }
    var value = factorial(n);
    $("#fact").text(value);

    if ($("#" + n).length) {
        return;
    }

    $("#line").before("<div class='equation' id='" + n + "'>" + n + "!=" + value + "</div>");
}

function factorial(n) {
    if (n === 0) {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}



// setup enter key response to input element
$("#num").keypress(function (e) {
    if (e.which == 13) {
        updateFactorialText();
    }
});
// setup out of focus response to input element
$("#num").focusout(function () {
    updateFactorialText();
});