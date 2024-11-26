
function text2key(text) {
    if (text == 'Pressure [Pa]')
        return 'P';
    else if (text == 'Temperature [K]')
        return 'T';
    else if (text == 'Density [kg/m&#179;]')
        return 'D';
}

function flush_fluid_list() {

    if (Module.get_global_param_string) {

        let fl = Module.get_global_param_string("FluidsList").split(",");

        for (let i = 0; i < fl.length; i++) {
            let name = fl[i];
            $('#FluidName').append(`<option value="${name}">${name}</option>`);
        }

        return true;
    }
    return false;
}


$(function () {
    // click calculation
    //using jQuery
    $('#calc').click(function () {
        console.log("clicked calc");
        var name = $('#FluidName :selected').text()
        var key1 = text2key($('#Name1 :selected').text())
        var key2 = text2key($('#Name2 :selected').text())
        var val1 = parseFloat($('#Value1').val())
        var val2 = parseFloat($('#Value2').val())

        var T = Module.PropsSI('T', key1, val1, key2, val2, name)
        var rho = Module.PropsSI('D', key1, val1, key2, val2, name)
        var p = Module.PropsSI('P', key1, val1, key2, val2, name)
        var s = Module.PropsSI('S', key1, val1, key2, val2, name)
        var h = Module.PropsSI('H', key1, val1, key2, val2, name)
        var cp = Module.PropsSI('C', key1, val1, key2, val2, name)

        text = ''
        text += 'T = ' + T + ' K\n' + '<br>'
        text += 'rho = ' + rho + ' kg/m&#179; <br>'
        text += 'p = ' + p + ' Pa<br>'
        text += 's = ' + s + ' J/kg/K<br>'
        text += 'h = ' + h + ' J/kg<br>'
        text += 'cp = ' + cp + ' J/kg/K<br>'

        $("#output").html(text);

    });


    setTimeout(function () {
        flush_fluid_list();
        // default values
        $('#FluidName').get(0).selectedIndex = 0;
        $('#Name1').get(0).selectedIndex = 1;
        $('#Name2').get(0).selectedIndex = 2;
        $('#Value1').val(1e6);
        $('#Value2').val(300);
    }, 1000);
});

