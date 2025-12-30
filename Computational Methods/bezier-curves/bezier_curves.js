function bezier_quad(t, p0, p1, p2){
    x = (1 - t)**2 * p0[0] + 2 * (1 - t) * t * p1[0] + t**2 * p2[0] 
    y = (1 - t)**2 * p0[1] + 2 * (1 - t) * t * p1[1] + t**2 * p2[1] 
    return (x, y);
}

function get_points(){
    let p0x = parseFloat(document.getElementById("p0x").value);
    let p0y = parseFloat(document.getElementById("p0y").value);
    let p1x = parseFloat(document.getElementById("p1x").value);
    let p1y = parseFloat(document.getElementById("p1y").value);
    let p2x = parseFloat(document.getElementById("p2x").value);
    let p2y = parseFloat(document.getElementById("p2y").value);
    return {'p0':{'x':p0x, 'y':p0y}, 'p1':{'x':p1x, 'y':p1y}, 'p2':{'x':p2x, 'y':p2y}};
}

/**
 * Draws a Quadratic Bézier curve, its control polygon, control points, 
 * and the tangent lines at the start (P0) and end (P2) points.
 * * @param {CanvasRenderingContext2D} ctx - The 2D rendering context.
 * @param {Object} points - Object containing P0, P1, P2 coordinates (e.g., {p0: {x: 100, y: 500}, ...}).
 * @param {number} width - Canvas width.
 * @param {number} height - Canvas height.
 */
function draw_bezier( ctx, points, width, height) {
    ctx.clearRect(0, 0, width, height);
    
    //  Prepare points and t-values
    let t_values = [];
    let bezier_points = [];
    let num_points = 100;

    for (let i = 0; i < num_points; i++) {
        t_values.push(i / num_points);
    }

    let p0 = [points.p0.x, points.p0.y];
    let p1 = [points.p1.x, points.p1.y];
    let p2 = [points.p2.x, points.p2.y];

    // Calculate curve points using the Quadratic Bézier formula
    for (let t of t_values) {
        let x = Math.pow(1 - t, 2) * p0[0] + 2 * (1 - t) * t * p1[0] + Math.pow(t, 2) * p2[0];
        let y = Math.pow(1 - t, 2) * p0[1] + 2 * (1 - t) * t * p1[1] + Math.pow(t, 2) * p2[1];
        bezier_points.push([x, y]);
    }

    //  Draw the Control Polygon (Dotted Lines)
    ctx.save();
    ctx.strokeStyle = "gray";
    ctx.setLineDash([5, 5]);
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.moveTo(points.p0.x, points.p0.y);
    ctx.lineTo(points.p1.x, points.p1.y); // Line from P0 to P1
    ctx.lineTo(points.p2.x, points.p2.y); // Line from P1 to P2
    ctx.stroke();
    ctx.setLineDash([]); // Reset to solid line
    ctx.restore();

    // Draw the Quadratic Bézier Curve
    ctx.strokeStyle = "blue";
    ctx.lineWidth = 3; 
    ctx.beginPath();
    ctx.moveTo(bezier_points[0][0], bezier_points[0][1]);
    for (let i = 1; i < bezier_points.length; i++) {
        ctx.lineTo(bezier_points[i][0], bezier_points[i][1]);
    }
    ctx.stroke();

    //  choose a scale factor (TANGENT_LENGTH) to make the lines visible
    const TANGENT_LENGTH = 150; 
    ctx.strokeStyle = "red";
    ctx.lineWidth = 2;
    ctx.lineCap = "round";

    // Tangent at P0 (parallel to P0P1)
    // Vector V_01 = P1 - P0
    let v01_x = points.p1.x - points.p0.x;
    let v01_y = points.p1.y - points.p0.y;

    // Normalize the vector (make it length 1)
    let length01 = Math.sqrt(v01_x * v01_x + v01_y * v01_y);
    let unit_x01 = v01_x / length01;
    let unit_y01 = v01_y / length01;

    // Start line at P0 and extend it TANGENT_LENGTH in both directions
    ctx.beginPath();
    // Extend backward from P0
    ctx.moveTo(points.p0.x - unit_x01 * TANGENT_LENGTH, points.p0.y - unit_y01 * TANGENT_LENGTH);
    // Extend forward through P0
    ctx.lineTo(points.p0.x + unit_x01 * TANGENT_LENGTH, points.p0.y + unit_y01 * TANGENT_LENGTH);
    ctx.stroke();


    //  Tangent at P2 (parallel to P1P2)
    // Vector V_12 = P2 - P1 
    let v12_x = points.p2.x - points.p1.x;
    let v12_y = points.p2.y - points.p1.y;

    // Normalize the vector
    let length12 = Math.sqrt(v12_x * v12_x + v12_y * v12_y);
    let unit_x12 = v12_x / length12;
    let unit_y12 = v12_y / length12;

    // Start line at P2 and extend it TANGENT_LENGTH in both directions
    ctx.beginPath();
    // Extend backward from P2
    ctx.moveTo(points.p2.x - unit_x12 * TANGENT_LENGTH, points.p2.y - unit_y12 * TANGENT_LENGTH);
    // Extend forward through P2
    ctx.lineTo(points.p2.x + unit_x12 * TANGENT_LENGTH, points.p2.y + unit_y12 * TANGENT_LENGTH);
    ctx.stroke();

    // Draw the three control points (P0, P1, P2)
    ctx.save();
    
    // P0 (start) 
    ctx.fillStyle = "green";
    ctx.beginPath();
    ctx.arc(points.p0.x, points.p0.y, 6, 0, 2 * Math.PI);
    ctx.fill();
    ctx.closePath();

    // P2 (end) -
    ctx.fillStyle = "red";
    ctx.beginPath();
    ctx.arc(points.p2.x, points.p2.y, 6, 0, 2 * Math.PI);
    ctx.fill();
    ctx.closePath();

    // P1 (control) 
    ctx.fillStyle = "black";
    ctx.beginPath();
    ctx.arc(points.p1.x, points.p1.y, 6, 0, 2 * Math.PI);
    ctx.fill();
    ctx.closePath();
    ctx.restore();
    
}
document.addEventListener("DOMContentLoaded", function(){
    const canvas = document.getElementById("bezierCanvas");
    const ctx = canvas.getContext("2d");
    const width = canvas.width;
    const height = canvas.height;
    initial_points = get_points();
    draw_bezier( ctx, initial_points, width, height);
    let is_dragging = false;
    canvas.addEventListener("mousedown", function(event){
        console.log("mousedown");
        const p1 = initial_points.p1;
        if (
            Math.abs(event.offsetX - p1.x) <= 4 &&
            Math.abs(event.offsetY - p1.y) <= 4
        ) {
            console.log("dragging");
            is_dragging = true;
        }
    });
    canvas.addEventListener("mouseup", function(event){
        is_dragging = false;
    });
    canvas.addEventListener("mousemove", function(event){
        if(is_dragging){
            const rect = canvas.getBoundingClientRect();
            const x = event.clientX - rect.left;
            const y = event.clientY - rect.top;
            initial_points.p1.x = x;
            initial_points.p1.y = y;
            draw_bezier( ctx, initial_points, width, height);
        }
    });
   
});
