var sudoku =
	  [ 0, 0, 0, 0, 0, 5, 3, 0, 7,
	    0, 9, 2, 1, 4, 0, 0, 0, 0,
	    0, 0, 0, 8, 0, 0, 0, 0, 0,
	    0, 0, 0, 0, 7, 0, 5, 0, 1,
	    0, 4, 0, 0, 0, 0, 0, 7, 0,
	    5, 0, 6, 0, 8, 0, 0, 0, 0,
	    0, 0, 0, 0, 0, 6, 0, 0, 0,
	    0, 0, 0, 0, 3, 9, 2, 5, 0,
	    2, 0, 9, 4, 0, 0, 0, 0, 3 ]
function sudokuDigit(x, y) {
    var idx = x + y*9;
    return sudoku[idx]}

var positiveUnitPropagationAssignments =
    [723, 709, 601, 679, 675, 254, 650, 158, 635, 623, 292, 701, 621,
     606, 537, 451, 449, 421, 429, 410, 394, 337, 316, 302, 286, 197,
     121, 109, 101, 99, 79, 57, 50]

var cutset =
   [6, 15, 64, 67, 71, 87, 143, 152, 166, 168, 177, 184, 209, 210,
    217, 220, 226, 229, 246, 251, 264, 314, 327, 332, 333, 345, 372,
    386, 404, 518, 523, 527, 544, 549, 553, 558, 562, 567, 643, 645,
    689, 717]

var leftVars =
      [1, 4, 6, 8, 10, 15, 17, 19, 22, 26, 64, 67, 71, 84, 87, 88, 89,
       129, 133, 143, 152, 163, 165, 166, 168, 169, 172, 174, 176, 177,
       181, 183, 184, 185, 187, 209, 210, 214, 217, 220, 226, 229, 246,
       251, 264, 269, 314, 325, 327, 332, 333, 343, 345, 350, 372, 386,
       404, 487, 489, 490, 493, 494, 496, 498, 500, 503, 505, 507, 508,
       509, 511, 512, 518, 523, 527, 541, 544, 548, 549, 550, 553, 557,
       558, 562, 566, 567, 568, 571, 573, 575, 577, 582, 584, 586, 589,
       593, 643, 645, 647, 658, 662, 663, 685, 689, 712, 717]

var rightVars =
      [6, 15, 29, 33, 36, 38, 42, 45, 64, 65, 67, 69, 71, 72, 87, 141,
       143, 150, 152, 166, 168, 177, 184, 200, 204, 207, 209, 210, 217,
       220, 222, 225, 226, 227, 229, 231, 234, 236, 238, 240, 243, 246,
       251, 252, 264, 273, 276, 279, 309, 312, 314, 315, 327, 332, 333,
       345, 353, 354, 356, 357, 360, 362, 365, 366, 369, 371, 372, 384,
       386, 387, 398, 402, 404, 405, 434, 435, 441, 463, 468, 470, 471,
       472, 477, 479, 481, 486, 515, 518, 523, 524, 527, 544, 549, 553,
       558, 562, 567, 643, 645, 689, 717]

// inverse of
// var x y d = d + ((x-1)+(y-1)*9) * 9
// in ../Sudoku.hs
function coordDigitFromVarId(varid) {
    var digit = 1 + (varid-1) % 9;
    var x = Math.floor((varid-digit) / 9) % 9
    var y = Math.floor((varid-digit-x*9) / 81) % 9
    return { digit: digit, x: x, y: y }}

function posObjectKey(x, y) {
    return '' + x + ',' + y}

function posObject(possibilities) {
    var res = new Object();
    possibilities.forEach(function (varid) {
	var cd = coordDigitFromVarId(varid);
	var key = posObjectKey(cd.x, cd.y);
	res[key] = (res[key] || '') + cd.digit})
    return res}




window.addEventListener('load', loadedEvent, false)
function loadedEvent() {
    var c = document.getElementById('theCanvas')
    app(c)}



function app(canvas) {

var ctx = canvas.getContext('2d');
var w = canvas.width, h = canvas.height;

var d = w / 9;

function clearCanvas() {
    ctx.fillStyle = '#ffeeee'
    ctx.fillRect(0, 0, w, h)}

function rect(x, y, rw, rh, ins) {
    ins = ins ? ins : 0;
    ctx.strokeRect(x*rw+ins, y*rh+ins,
		   (x+1)*rw-ins, (y+1)*rh-ins)}

function line(x, y, xe, ye, lineWidth) {
    ctx.beginPath();
    ctx.lineWidth = lineWidth;
    ctx.moveTo(x, y);
    ctx.lineTo(xe, ye);
    ctx.stroke();
    ctx.closePath()}

function drawSquare() {
    ctx.fillStyle ='black'
    for (var i = 0; i <= 9; i++) {
	var lwid = ((i%3) == 0) ? 3 : 1;
	line(i*d, 0, i*d, h, lwid)
	line(0, i*d, w, i*d, lwid)
    }
    var up = posObject(positiveUnitPropagationAssignments);
    var cs = posObject(cutset);
    var lv = posObject(leftVars);
    var rv = posObject(rightVars);
    for (var x = 0; x < 9; x++)
	for (var y = 0; y < 9; y++) {
	    var dig = sudokuDigit(x, y)
	    if (dig > 0) {
		ctx.fillStyle = 'black';
		ctx.font = 'normal bold 50px sans-serif'
		ctx.fillText('' + dig, x*d+d/3, y*d+d*0.7)}
	    var upd  = up[posObjectKey(x, y)]
	    if (upd && dig == 0) {
		ctx.fillStyle = '#e88';
		ctx.font = 'normal normal 30px sans-serif'
		ctx.fillText('' + upd, x*d +d*0.1, y*d +d*0.3)}
	    var csd  = cs[posObjectKey(x, y)]
	    if (csd) {
		ctx.fillStyle = '#222';
		ctx.font = 'normal normal 20px sans-serif'
		ctx.fillText('' + csd, x*d +d*0.1, y*d +d*0.2)}
	    var lvd  = lv[posObjectKey(x, y)]
	    if (lvd) {
		ctx.fillStyle = 'green';
		ctx.font = 'normal normal 20px sans-serif'
		ctx.fillText('' + lvd, x*d +d*0.1, y*d +d*0.4)}
	    var rvd  = rv[posObjectKey(x, y)]
	    if (rvd) {
		ctx.fillStyle = 'blue';
		ctx.font = 'normal normal 20px sans-serif'
		ctx.fillText('' + rvd, x*d +d*0.1, y*d +d*0.6)}
		
	}
    
}
    
    clearCanvas();
    drawSquare();

}
