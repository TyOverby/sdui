
const image = document.querySelector("img");
const stack = document.querySelector(".stack");
const img_canvas = stack.querySelectorAll("canvas")[0];
const draw_canvas = stack.querySelectorAll("canvas")[1];
const outline_canvas = stack.querySelectorAll("canvas")[2];
const img_ctx = img_canvas.getContext("2d", { willReadFrequently: true });
const draw_ctx = draw_canvas.getContext("2d");
const outline_ctx = outline_canvas.getContext("2d");

function drawPill(ctx, x1, y1, x2, y2, thickness) {
    const angle = Math.atan2(y2 - y1, x2 - x1);
    const length = Math.sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2);

    ctx.save();
    ctx.translate(x1, y1);
    ctx.rotate(angle);

    ctx.beginPath();
    ctx.arc(0, 0, thickness / 2, Math.PI / 2, -Math.PI / 2);
    ctx.lineTo(length, -thickness / 2);
    ctx.arc(length, 0, thickness / 2, -Math.PI / 2, Math.PI / 2);
    ctx.lineTo(0, thickness / 2);
    ctx.closePath();
    ctx.fill();

    ctx.restore();
}

function init() {
    function draw() { img_ctx.drawImage(image, 0, 0, image.naturalWidth, image.naturalWidth); }
    if (image.complete) { draw() }
    else { image.onload = draw; }
    outline_ctx.strokeStyle = "rgba(0,0,0,0.5)";
    outline_ctx.lineWidth = 1.0;
}

var color = "rgb(255,0,0,1.0)"

function mousedown(event) {
    event.target.setPointerCapture(event.pointerId);

    var erasing = false;
    var picking = false;
    if (event.shiftKey) {
        draw_ctx.globalCompositeOperation = "destination-out";
        erasing = true;
    }
    else {
        draw_ctx.globalCompositeOperation = "source-over";
    }

    var last_x = null;
    var last_y = null;

    function move(event) {
        if (event.pressure === 0) {
            last_x = null;
            last_y = null;
            return;
        }
        var pressure = erasing ? 1 : event.pressure;

        var x = event.offsetX * (image.naturalWidth / image.width);
        var y = event.offsetY * (image.naturalHeight / image.height);

        draw_ctx.fillStyle = color;
        if (last_x === null || last_y === null) {
            draw_ctx.beginPath();
            draw_ctx.ellipse(x, y, 20 * pressure, 20 * pressure, 0, Math.PI * 2, 0);
            draw_ctx.fill();
        } else {
            drawPill(draw_ctx, last_x, last_y, x, y, 40 * pressure);
        }
        last_x = x;
        last_y = y;
    }

    event.target.addEventListener("pointermove", move);

    event.target.addEventListener("pointerup", (function (event) {
        event.target.releasePointerCapture(event.pointerId);
        event.target.removeEventListener("pointermove", move);
    }))
}

outline_canvas.addEventListener("pointerdown", mousedown);
outline_canvas.addEventListener("contextmenu", (function (e) { e.preventDefault() }));

outline_canvas.addEventListener("pointerout", function (event) {
    outline_ctx.clearRect(0, 0, outline_canvas.width, outline_canvas.height);
});
outline_canvas.addEventListener("pointermove", function (event) {
    var x = event.offsetX * (image.naturalWidth / image.width);
    var y = event.offsetY * (image.naturalHeight / image.height);

    outline_ctx.clearRect(0, 0, outline_canvas.width, outline_canvas.height);
    var image_data = img_ctx.getImageData(Math.round(x), Math.round(y), 1, 1);
    var inverse = "rgb(" +
        (255 - image_data.data[0]) + "," +
        (255 - image_data.data[1]) + "," +
        (255 - image_data.data[2]) + ")";
    outline_ctx.strokeStyle = inverse;
    if (event.ctrlKey) {
        color = "rgb(" +
            image_data.data[0] + "," +
            image_data.data[1] + "," +
            image_data.data[2] + ")";
        outline_ctx.fillStyle = color;
        outline_ctx.beginPath();
        outline_ctx.ellipse(x, y, 20, 20, 0, Math.PI * 2, 0);
        outline_ctx.fill();
    }

    outline_ctx.beginPath();
    outline_ctx.ellipse(x, y, 20, 20, 0, Math.PI * 2, 0);
    outline_ctx.stroke();

});

outline_canvas.addEventListener('auxclick', function (e) {
    if (e.button == 1) {
        alert("middle button clicked")
    }
})

init()