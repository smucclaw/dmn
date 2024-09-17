function results ( mark, test ) {
    if (mark >= 70 && test === true) {
        return {output1: 'A', output2: 'pass'};
    } else if (mark >= 60 && mark <70) {
        return {output1: 'B', output2: 'pass'};
    } else if (mark >= 50 && mark <= 59) {
        return {output1: 'C', output2: 'pass'};
    } else if (mark >= 40 && mark <= 49) {
        return {output1: 'D', output2: 'fail'};
    } else if (mark >= 30 && mark <= 39) {
        return {output1: 'E', output2: 'fail'};
    } else {
        return {output1: 'F', output2: 'fail'};
    }
}
let {output1: grade, output2: result} = results (50, true);

console.log ( 'Grade:', grade, 'Result:', result );