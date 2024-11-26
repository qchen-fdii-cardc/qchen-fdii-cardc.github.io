+++
title = '002 Coolprop'
date = 2024-11-23T16:06:55+08:00
draft = true
mathjax = false
categories = ['javascript']
tags = ['javascript']
toc = false
tocBorder = true
+++



{{<rawhtml>}}
<link rel="stylesheet" href="http://code.jquery.com/ui/1.11.0/themes/smoothness/jquery-ui.css" />
<script src="http://code.jquery.com/jquery-1.11.0.min.js"></script>
<script src="http://code.jquery.com/jquery-migrate-1.2.1.min.js"></script>
<script src="http://code.jquery.com/ui/1.10.4/jquery-ui.js"></script>

<script src="/javascript/coolprop.js"></script>

    <div class="ui-widget">
        <label>FluidName: </label>
        <select id="FluidName">
            <option>Nitrogen</option>
            <option>R134a</option>
        </select>
    </div>
    <div class="ui-widget">
        <label>Input #1: </label>
        <select id="Name1">
            <option value="">Select one...</option>
            <option value="Pressure">Pressure [Pa]</option>
            <option value="Temperature">Temperature [K]</option>
            <option value="Density">Density [kg/m&#179;]</option>
        </select>
        <input id ='Value1'></input>
    </div>
    <div class="ui-widget">
        <label>Input #2: </label>
        <select id="Name2">
            <option value="">Select one...</option>
            <option value="Pressure">Pressure [Pa]</option>
            <option value="Temperature">Temperature [K]</option>
            <option value="Density">Density [kg/m&#179;]</option>
        </select>
        <input id ='Value2'></input>
    </div>
    
    <button id="calc">Calculate</button>
    <div  class="ui-widget"> 
    <label>Output: </label>
    </div>
    <div class="ui-widget">
        <p id="output">
    </div>

    <script>
    function text2key(text)
    {
        if (text == 'Pressure [Pa]')
            return 'P';
        else if (text == 'Temperature [K]')
            return 'T';
        else if (text == 'Density [kg/m&#179;]')
            return 'D';
    }
    //using jQuery
    $('#calc').click( function() {
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
        
        $( "#output" ).html( text);


        var humid = Module.HAPropsSI('H','T',298.15,'P',101325,'R',0.5);
        console.log(humid);
        
    });
    </script>


    <h1>Humid Air Properties</h1>
    <div class="ui-widget">
        <label>Input #1: </label>
        <select id="Name3">
            <option value="">Select one...</option>
            <option value="Pressure">Pressure [Pa]</option>
            <option value="Temperature">Temperature [K]</option>
            <option value="RelativeHumidity">Relative Humidity [kg/kg]</option>
        </select>
        <input id ='Value3'></input>
    </div>
    <div class="ui-widget">
        <label>Input #2: </label>
        <select id="Name4">
            <option value="">Select one...</option>
            <option value="Pressure">Pressure [Pa]</option>
            <option value="Temperature">Temperature [K]</option>
            <option value="RelativeHumidity">Relative Humidity [kg/kg]</option>
        </select>
        <input id ='Value4'></input>    

    </div>

    <button id="calc2">Calculate</button>
    <div  class="ui-widget">
    <label>Output: </label>
    </div>

    <div class="ui-widget">
        <p id="output2">
    </div>

    <script>
    function text2key(text)
    {
        if (text == 'Pressure [Pa]')
            return 'P';
        else if (text == 'Temperature [K]')
            return 'T';
        else if (text == 'Relative Humidity [kg/kg]')
            return 'R';
    }
    //using jQuery

    $('#calc2').click( function() {
        var key3 = text2key($('#Name3 :selected').text())
        var key4 = text2key($('#Name4 :selected').text())
        var val3 = parseFloat($('#Value3').val())
        var val4 = parseFloat($('#Value4').val())
        
        var T = Module.HAPropsSI('T', key3, val3, key4, val4)
        var rho = Module.HAPropsSI('D', key3, val3, key4, val4)
        var p = Module.HAPropsSI('P', key3, val3, key4, val4)
        var s = Module.HAPropsSI('S', key3, val3, key4, val4)
        var h = Module.HAPropsSI('H', key3, val3, key4, val4)
        var cp = Module.HAPropsSI('C', key3, val3, key4, val4)
        
        text = ''
        text += 'T = ' + T + ' K\n' + '<br>'
        text += 'rho = ' + rho + ' kg/m&#179; <br>'
        text += 'p = ' + p + ' Pa<br>'
        text += 's = ' + s + ' J/kg/K<br>'
        text += 'h = ' + h + ' J/kg<br>'
        text += 'cp = ' + cp + ' J/kg/K<br>'
        
        $( "#output2" ).html( text);
    });



{{</rawhtml>}}