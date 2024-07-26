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
function createCanvas(name, image, parent) {
    const canvas = document.createElement("canvas");
    canvas.setAttribute("data-name", name);

    canvas.setAttribute("width", image.naturalWidth);
    canvas.setAttribute("height", image.naturalHeight);
    canvas.style.width = (image.naturalWidth / window.devicePixelRatio) + "px";
    canvas.style.height = (image.naturalHeight / window.devicePixelRatio) + "px";

    if (parent) {
        parent.appendChild(canvas);
    }

    return canvas;
}

//Provides:isCanvasAllWhite
function isCanvasAllWhite(canvas, ctx) {
    const imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
    const data = imageData.data;

    for (let i = 0; i < data.length; i += 4) {
        if (data[i] !== 255 || data[i + 1] !== 255 || data[i + 2] !== 255 || data[i + 3] !== 255) {
            return false;
        }
        data[i + 3] = 255;
    }

    return true;
}

//Provides:painter_init
//Requires:drawPill, sanatize_url, on_image_init, rgbToHex, createCanvas, isCanvasAllWhite
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
        setDirty: (function () { }),
        mode: "paint"
    };

    on_image_init(image, function () {
        let img_canvas = createCanvas("background", image, stack);
        let draw_canvas = createCanvas("draw", image, stack);
        let mask_canvas = createCanvas("mask", image, stack);
        let outline_canvas = createCanvas("outline", image, stack);
        let composite_canvas = createCanvas("composite", image);
        let composite_mask_canvas = createCanvas("composite-mask", image);

        outline_canvas.className = "outline_layer"
        mask_canvas.className = "mask_layer"

        let img_ctx = img_canvas.getContext("2d", { willReadFrequently: true });
        let mask_ctx = mask_canvas.getContext("2d");
        let draw_ctx = draw_canvas.getContext("2d");
        let outline_ctx = outline_canvas.getContext("2d");
        let composite_ctx = composite_canvas.getContext("2d");
        let composite_mask_ctx = composite_mask_canvas.getContext("2d");

        outline_ctx.strokeStyle = "rgba(0,0,0,0.5)";
        outline_ctx.lineWidth = 1.0;

        img_ctx.drawImage(image, 0, 0, image.naturalWidth, image.naturalHeight);

        state.clear = function () {
            draw_ctx.clearRect(0, 0, outline_canvas.width, outline_canvas.height);
            mask_ctx.clearRect(0, 0, outline_canvas.width, outline_canvas.height);
        }

        state.composite = function () {
            composite_ctx.clearRect(0, 0, image.naturalWidth, image.naturalHeight);
            outline_ctx.globalCompositeOperation = 'source-over';
            composite_ctx.drawImage(img_canvas, 0, 0);
            composite_ctx.drawImage(draw_canvas, 0, 0);
            return composite_canvas.toDataURL("image/png", 1);
        }

        state.compositeMask = function () {
            composite_mask_ctx.globalCompositeOperation = "source-over";
            composite_mask_ctx.fillStyle = "white";
            composite_mask_ctx.fillRect(0, 0, image.naturalWidth, image.naturalHeight);
            composite_mask_ctx.drawImage(mask_canvas, 0, 0);

            if (isCanvasAllWhite(composite_mask_canvas, composite_mask_ctx)) {
                return ""
            } else {
                return composite_mask_canvas.toDataURL("image/png", 1);
            }
        }

        state.updateImage = function (data_url) {
            data_url = sanatize_url(data_url);
            image = document.createElement("img");
            image.crossOrigin = "Anonymous";
            image.setAttribute("src", data_url);
            state.setDirty();
            on_image_init(image, function () {
                img_ctx.drawImage(image, 0, 0, image.naturalWidth, image.naturalHeight);
            })
        }

        function mousedown(event) {
            event.target.setPointerCapture(event.pointerId);

            var target_ctx = state.mode === "mask" ? mask_ctx : draw_ctx;

            var erasing = false;
            if (event.ctrlKey) {
                target_ctx.globalCompositeOperation = "destination-out";
                erasing = true;
            } else {
                target_ctx.globalCompositeOperation = "source-over";
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

                var x = event.offsetX * window.devicePixelRatio;
                var y = event.offsetY * window.devicePixelRatio;

                if (state.mode !== "mask") {
                    target_ctx.fillStyle = state.color;
                } else {
                    target_ctx.fillStyle = "black";
                }

                var radius = 0;
                if (state.penSize === 1) { radius = 1; } else { radius = state.penSize * pressure; }
                if (last_x === null || last_y === null) {
                    target_ctx.beginPath();
                    target_ctx.ellipse(x, y, radius, radius, 0, Math.PI * 2, 0);
                    target_ctx.fill();
                } else {
                    drawPill(target_ctx, last_x, last_y, x, y, last_radius, radius);
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
            var x = event.offsetX * window.devicePixelRatio;
            var y = event.offsetY * window.devicePixelRatio;

            outline_ctx.clearRect(0, 0, outline_canvas.width, outline_canvas.height);
            var image_data = img_ctx.getImageData(Math.round(x), Math.round(y), 1, 1);
            var r = image_data.data[0];
            var g = image_data.data[1];
            var b = image_data.data[2];
            var inverse = "rgb(" + (255 - r) + "," + (255 - g) + "," + (255 - b) + ")";
            outline_ctx.strokeStyle = inverse;
            if (event.shiftKey) {
                state.color = "rgb(" + r + "," + g + "," + b + ")";
                state.onColorChange(rgbToHex(r, g, b));

                outline_ctx.fillStyle = state.color;
                outline_ctx.beginPath();
                outline_ctx.ellipse(x, y, state.penSize * 2, state.penSize * 2, 0, Math.PI * 2, 0);
                outline_ctx.fill();

                outline_ctx.globalCompositeOperation = 'destination-out';
                outline_ctx.beginPath();
                outline_ctx.ellipse(x, y, state.penSize, state.penSize, 0, 0, Math.PI * 2);
                outline_ctx.fill();
                outline_ctx.globalCompositeOperation = 'source-over';
            }

            outline_ctx.beginPath();
            outline_ctx.ellipse(x, y, state.penSize, state.penSize, 0, Math.PI * 2, 0);
            outline_ctx.stroke();

            if (event.shiftKey) {
                outline_ctx.beginPath();
                outline_ctx.ellipse(x, y, state.penSize * 2, state.penSize * 2, 0, Math.PI * 2, 0);
                outline_ctx.stroke();
            }

            outline_ctx.beginPath();
            outline_ctx.ellipse(x, y, 0.25, 0.25, 0, Math.PI * 2, 0);
            outline_ctx.stroke();
        });
    });

    return [0, state, stack];
}