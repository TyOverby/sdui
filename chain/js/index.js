//Provides:drawPill
function drawPill(ctx, x1, y1, x2, y2, radius1, radius2) {
    const angle = Math.atan2(y2 - y1, x2 - x1);
    const length = Math.sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2);

    ctx.save();
    ctx.translate(x1, y1);
    ctx.rotate(angle);

    ctx.beginPath();
    ctx.arc(0, 0, radius1, Math.PI / 2, -Math.PI / 2);
    ctx.lineTo(length, -radius2);
    ctx.arc(length, 0, radius2, -Math.PI / 2, Math.PI / 2);
    ctx.lineTo(0, radius1);
    ctx.closePath();
    ctx.fill();

    ctx.restore();
}

//Provides:rgbToHex
function rgbToHex(r, g, b) {
    function componentToHex(c) {
        const hex = c.toString(16);
        return hex.length === 1 ? "0" + hex : hex;
    }
    return "#" + componentToHex(r) + componentToHex(g) + componentToHex(b);
}

//Provides:sanatize_url
function sanatize_url(url) {
    return (url.startsWith("data:image") || url.startsWith("http://") || url.startsWith("https://")) ? url : "data:image/png;base64," + url;
}

//Provides:on_image_init
function on_image_init(image, f) {
    if (image.complete) { f() }
    else { image.onload = f; }
}

//Provides:createCanvas
function createCanvas(width, height, parent) {
    const canvas = document.createElement("canvas");
    canvas.setAttribute("width", width);
    canvas.setAttribute("height", height);
    if (parent) {
        parent.appendChild(canvas);
    }
    return canvas;
}

//Provides:painter_init
//Requires:drawPill, sanatize_url, on_image_init, rgbToHex, createCanvas
function painter_init(input) {
    var data_url = sanatize_url(input);
    var stack = document.createElement("div");
    stack.className = "stack";

    var image = document.createElement("img");
    image.crossOrigin = "Anonymous";
    image.setAttribute("src", data_url);

    var state = {
        clear: function () { console.log("cleared"); },
        penSize: 20,
        color: "rgb(255,0,0)",
        onColorChange: (function () { }),
        setDirty: (function () { })
    };

    on_image_init(image, function () {
        let img_canvas = createCanvas(image.naturalWidth, image.naturalHeight, stack);
        let draw_canvas = createCanvas(image.naturalWidth, image.naturalHeight, stack);
        let outline_canvas = createCanvas(image.naturalWidth, image.naturalHeight, stack);
        let composite_canvas = createCanvas(image.naturalWidth, image.naturalHeight);

        let img_ctx = img_canvas.getContext("2d", { willReadFrequently: true });
        let draw_ctx = draw_canvas.getContext("2d");
        let outline_ctx = outline_canvas.getContext("2d");
        let composite_ctx = composite_canvas.getContext("2d");

        outline_ctx.strokeStyle = "rgba(0,0,0,0.5)";
        outline_ctx.lineWidth = 1.0;

        img_ctx.drawImage(image, 0, 0, image.naturalWidth, image.naturalHeight);

        state.clear = function () {
            draw_ctx.clearRect(0, 0, outline_canvas.width, outline_canvas.height);
        }

        state.composite = function () {
            composite_ctx.drawImage(img_canvas, 0, 0);
            composite_ctx.drawImage(draw_canvas, 0, 0);
            return composite_canvas.toDataURL("image/png", 1);
        }

        state.compositeMask = function () {
            composite_ctx.clearRect(0, 0, image.naturalWidth, image.naturalHeight);
            composite_ctx.globalCompositeOperation = "source-over";
            composite_ctx.fillStyle = "black";
            composite_ctx.fillRect(0, 0, image.naturalWidth, image.naturalHeight)
            composite_ctx.globalCompositeOperation = "destination-in";
            composite_ctx.drawImage(draw_canvas, 0, 0);
            composite_ctx.globalCompositeOperation = "source-out";
            composite_ctx.fillStyle = "white";
            composite_ctx.fillRect(0, 0, image.naturalWidth, image.naturalHeight)
            return composite_canvas.toDataURL("image/png", 1);
        }

        state.updateImage = function (data_url) {
            data_url = sanatize_url(data_url);
            image = document.createElement("img");
            image.crossOrigin = "Anonymous";
            image.setAttribute("src", data_url);
            on_image_init(image, function () {
                img_ctx.drawImage(image, 0, 0, image.naturalWidth, image.naturalHeight);
            })
        }

        function mousedown(event) {
            event.target.setPointerCapture(event.pointerId);

            var erasing = false;
            if (event.shiftKey) {
                draw_ctx.globalCompositeOperation = "destination-out";
                erasing = true;
            } else {
                draw_ctx.globalCompositeOperation = "source-over";
            }

            var last_x = null;
            var last_y = null;
            var last_radius = null;

            function move(event) {
                if (event.pressure === 0) {
                    last_x = null;
                    last_y = null;
                    return;
                }
                var pressure = erasing ? 1 : event.pressure;

                var x = event.offsetX * (image.naturalWidth / image.width);
                var y = event.offsetY * (image.naturalHeight / image.height);

                draw_ctx.fillStyle = state.color;
                var radius = 0;
                if (state.penSize === 1) { radius = 1; } else { radius = state.penSize * pressure; }
                if (last_x === null || last_y === null) {
                    draw_ctx.beginPath();
                    draw_ctx.ellipse(x, y, radius, radius, 0, Math.PI * 2, 0);
                    draw_ctx.fill();
                } else {
                    drawPill(draw_ctx, last_x, last_y, x, y, last_radius, radius);
                }
                last_x = x;
                last_y = y;
                last_radius = radius;
                state.setDirty();
            }

            event.target.addEventListener("pointermove", move);

            event.target.addEventListener("pointerup", (function (event) {
                event.target.releasePointerCapture(event.pointerId);
                event.target.removeEventListener("pointermove", move);
            }))
        }

        outline_canvas.addEventListener("pointerdown", mousedown);
        outline_canvas.addEventListener("contextmenu", (function (e) { e.preventDefault() }));

        outline_canvas.addEventListener("pointerout", function () {
            outline_ctx.clearRect(0, 0, outline_canvas.width, outline_canvas.height);
        });

        outline_canvas.addEventListener("pointermove", function (event) {
            var x = event.offsetX * (image.naturalWidth / image.width);
            var y = event.offsetY * (image.naturalHeight / image.height);

            outline_ctx.clearRect(0, 0, outline_canvas.width, outline_canvas.height);
            var image_data = img_ctx.getImageData(Math.round(x), Math.round(y), 1, 1);
            var r = image_data.data[0];
            var g = image_data.data[1];
            var b = image_data.data[2];
            var inverse = "rgb(" + (255 - r) + "," + (255 - g) + "," + (255 - b) + ")";
            outline_ctx.strokeStyle = inverse;
            if (event.ctrlKey) {
                state.color = "rgb(" + r + "," + g + "," + b + ")";
                state.onColorChange(rgbToHex(r, g, b));

                outline_ctx.fillStyle = state.color;
                outline_ctx.beginPath();
                outline_ctx.ellipse(x, y, state.penSize, state.penSize, 0, Math.PI * 2, 0);
                outline_ctx.fill();
            }

            outline_ctx.beginPath();
            outline_ctx.ellipse(x, y, state.penSize, state.penSize, 0, Math.PI * 2, 0);
            outline_ctx.stroke();
        });
    });

    return [0, state, stack];
}