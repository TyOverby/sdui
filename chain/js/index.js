//Provides:painter_init
function painter_init(settings) {
    var data_url = settings.startsWith("data:image")
        ? settings
        : "data:image/png;base64," + settings;

    var stack = document.createElement("div");
    stack.className = "stack";

    var image = document.createElement("img");
    image.setAttribute("src", data_url);

    function init(image, f) {
        if (image.complete) { f() }
        else { image.onload = f; }
    }

    var state = {
        clear: function () { console.log("cleared"); },
    };

    init(image, function () {
        const img_canvas = document.createElement("canvas");
        img_canvas.setAttribute("width", image.naturalWidth);
        img_canvas.setAttribute("height", image.naturalHeight);
        stack.appendChild(img_canvas);

        const draw_canvas = document.createElement("canvas");
        draw_canvas.setAttribute("width", image.naturalWidth);
        draw_canvas.setAttribute("height", image.naturalHeight);
        stack.appendChild(draw_canvas);

        const outline_canvas = document.createElement("canvas");
        outline_canvas.setAttribute("width", image.naturalWidth);
        outline_canvas.setAttribute("height", image.naturalHeight);
        stack.appendChild(outline_canvas);

        const composite_canvas = document.createElement("canvas");
        composite_canvas.setAttribute("width", image.naturalWidth);
        composite_canvas.setAttribute("height", image.naturalHeight);

        let img_ctx = img_canvas.getContext("2d", { willReadFrequently: true });
        const draw_ctx = draw_canvas.getContext("2d");
        const outline_ctx = outline_canvas.getContext("2d");
        const composite_ctx = composite_canvas.getContext("2d");

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
            var image = document.createElement("img");
            image.setAttribute("src", "data:image/png;base64," + data_url);
            init(image, function () {
                img_ctx.drawImage(image, 0, 0, image.naturalWidth, image.naturalHeight);
            })
        }

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
    });

    return [0, state, stack];
}