function play(outlook, temp, humidity, windy) {
    if (outlook === 'sunny' && temp > 80 && humidity > 85) {
        return {output1: false, output2: 1};
    } else if (outlook === 'overcast') {
        return {output1: true, output2: 1};
    } else if (outlook === 'rain' && windy === true) {
        return {output1: false, output2: 0};
    } else if (outlook === 'rain' && windy === false) {
        return {output1: true, output2: 0};
    } else if (outlook === 'sunny' && temp >= 71 && temp <80 && humidity >70 && humidity <= 95) {
        return {output1: false, output2: 1};
    } else {
        return {output1: true, output2: 1};
    }
}
function which(golf, swimming) {
    if (swimming === 1) {
        return 'swimming';
    } else if (golf === true && swimming === 0) {
        return 'golf';
    } else {
        return 'stay at home';
    }
}
function wear_sunglasses(outlook, sport) {
    if (outlook === 'sunny' && sport === 'golf') {
        return true;
    } else {
        return false;
    }
}
let {output1: golf, output2: swim} = play('sunny', 75, 90, true);
let choice = which(golf, swim);
let sunglasses = wear_sunglasses('sunny', choice);

console.log ( 'Choice:', choice, ', Sunglasses:', sunglasses );