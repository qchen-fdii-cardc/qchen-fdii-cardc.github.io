
const parameters = {
    "Wet-Bulb Temperature": 'B',
    "Dew-Point Temperature": 'D',
    "Mixture enthalpy per dry air": 'H',
    "Mixture enthalpy per humid air": "Hha",
    "Water mole fraction": "Y",
    "Pressure": "P",
    "Partial pressure of water vapor": "P_w",
    "Relative humidity in [0, 1]": 'R',
    "Mixture entropy per unit dry air": "S",
    "Mixture entropy per unit humid air": "Sha",
    "Dry-Bulb Temperature": "T",
    "Mixture volume per unit dry air": "V",
    "Mixture volume per unit humid air": "Vha",
    "Humid ratio": "W"
};

const pp = new Map();

for (const [key, value] of parameters){
    pp.set(value, key);
}


$(function () {
    $('button').button().click(function () {
        let d = Module.PropsSI("D", "P", 101325, "T", 300, "Air");
        let h = Module.HAPropsSI("D", "P", 101325, "T", 300, "R", 0.5);
        $("#result").text(`${d}-${h}`);
    })
});