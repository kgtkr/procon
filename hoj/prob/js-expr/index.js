var s = require('fs').readFileSync('/dev/stdin', 'utf8');
try {
    new Function(s);
    console.log("Yes");
} catch (e) {
    console.log("No");
}