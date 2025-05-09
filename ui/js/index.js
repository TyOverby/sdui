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

function rgbToHex(r, g, b) {
    function componentToHex(c) {
        const hex = c.toString(16);
        return hex.length === 1 ? "0" + hex : hex;
    }
    return "#" + componentToHex(r) + componentToHex(g) + componentToHex(b);
}

function sanatize_url(url) {
    return (url.startsWith("data:image") || url.startsWith("http://") || url.startsWith("https://")) ? url : "data:image/png;base64," + url;
}

function on_image_init(image, f) {
    if (image.complete) { f() }
    else { image.onload = f; }
}

function on_images_init(images, f) {
    if (images.length === 0) {
        f();
    } else {
        let hd = images.shift();
        if (hd) {
            on_image_init(hd, function () {
                on_images_init(images, f)
            })
        } else {
            on_images_init(images, f)
        }
    }
}

function resizeCanvas(image, canvas) {
    canvas.setAttribute("width", image.naturalWidth);
    canvas.setAttribute("height", image.naturalHeight);
    canvas.style.width = (image.naturalWidth / window.devicePixelRatio) + "px";
    canvas.style.height = (image.naturalHeight / window.devicePixelRatio) + "px";
}

function createCanvas(name, image, parent) {
    const canvas = document.createElement("canvas");
    canvas.setAttribute("data-name", name);
    resizeCanvas(image, canvas)

    if (parent) {
        parent.appendChild(canvas);
    }

    return canvas;
}

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

function randomPointInCircle(cx, cy, r, dist_x, dist_y, stddev) {
    function randn_bm() {
        let u = 0, v = 0;
        while (u === 0) u = Math.random(); // Convert [0,1) to (0,1)
        while (v === 0) v = Math.random();
        return Math.sqrt(-2.0 * Math.log(u)) * Math.cos(2.0 * Math.PI * v);
    }

    let x, y;
    do {
        x = dist_x + randn_bm() * stddev;
        y = dist_y + randn_bm() * stddev;
    } while (Math.sqrt((x - cx) ** 2 + (y - cy) ** 2) > r);

    return { x: x, y: y };
}

function is_in_circle(cx, cy, r, x, y) {
    const d2 = (x - cx) * (x - cx) + (y - cy) * (y - cy);
    return d2 <= r * r;
}

function scramble(img_data, draw_data, cx, cy, r, prev_cx, prev_cy, prev_r, w, h) {
    cx = Math.round(cx);
    cy = Math.round(cy);
    function lookup(x, y) {
        let idx = (y * w + x) * 4;
        let draw_a = draw_data.data[idx + 3];
        if (draw_a > 0) {
            return [
                idx,
                draw_data.data[idx + 0],
                draw_data.data[idx + 1],
                draw_data.data[idx + 2]
            ]
        } else {
            return [
                idx,
                img_data.data[idx + 0],
                img_data.data[idx + 1],
                img_data.data[idx + 2]
            ]
        }
    }

    function swap_colors(ax, ay, bx, by) {
        let [a_idx, ar, ag, ab] = lookup(ax, ay);
        let [b_idx, br, bg, bb] = lookup(bx, by);
        draw_data.data[a_idx + 0] = br;
        draw_data.data[a_idx + 1] = bg;
        draw_data.data[a_idx + 2] = bb;
        draw_data.data[a_idx + 3] = 255;

        draw_data.data[b_idx + 0] = ar;
        draw_data.data[b_idx + 1] = ag;
        draw_data.data[b_idx + 2] = ab;
        draw_data.data[b_idx + 3] = 255;
    }

    r = Math.round(r);
    for (let dy = -r; dy < r; dy++) {
        let y = dy + cy;
        if (y < 0) { continue; }
        if (y >= h) { break; }

        for (let dx = -r; dx < r; dx++) {
            let x = dx + cx;
            if (x < 0) { continue; }
            if (x >= w) { break; }
            if (dx * dx + dy * dy > r * r) { continue; }

            if (prev_cx != null && prev_cy != null && prev_r != null) {
                if (is_in_circle(prev_cx, prev_cy, prev_r, x, y)) {
                    continue;
                }
            }

            let next_x, next_y;
            do {
                let { x: rand_x, y: rand_y } = randomPointInCircle(cx, cy, r, x, y, r / 10);
                next_x = Math.round(rand_x);
                next_y = Math.round(rand_y);
            } while (next_x < 0 || next_y < 0 || next_x >= w || next_y >= h);

            swap_colors(x, y, next_x, next_y);
        }
    }
}

function prep_img(img_string) {
    var image = document.createElement("img");
    image.crossOrigin = "Anonymous";
    var data_url = sanatize_url(img_string);
    image.setAttribute("src", data_url);
    return image;
}

function painter_init(input, paint_input, mask_input, blur_mask_input) {
    var stack = document.createElement("div");
    stack.className = "stack";

    var images = [];

    var image = prep_img(input);
    images.push(image);

    var paint_input_image = paint_input ? prep_img(paint_input) : null;
    images.push(paint_input_image);
    var mask_input_image = mask_input ? prep_img(mask_input) : null;
    images.push(mask_input_image);
    var blur_mask_input_image = blur_mask_input ? prep_img(blur_mask_input) : null;
    images.push(blur_mask_input_image);

    var state = {
        clear: function () { console.log("cleared"); },
        penSize: 20,
        color: "rgb(255,0,0)",
        mode: "paint",
        alt: "shuffle",
        img_base64_cache: null,
        mask_base64_cache: null,
        onColorChange: (function () { }),
        setDirty: (function () { }),
        onUpdateImage: (function () { }),
        getPaintLayer: (function () { }),
        getMaskLayer: (function () { }),
        getBlurMaskLayer: (function () { }),
        compositeMask: (function () { }),
        flipCanvas: (function () { }),
        updateImage: (function () { }),
        setPaintImage: (function () { }),
    };

    on_images_init(images, function () {
        let img_canvas = createCanvas("background", image, stack);
        let draw_canvas = createCanvas("draw", image, stack);
        let mask_canvas = createCanvas("mask", image, stack);
        let blur_mask_canvas = createCanvas("blur_mask", image, stack);
        let outline_canvas = createCanvas("outline", image, stack);
        let composite_canvas = createCanvas("composite", image);
        let composite_mask_canvas = createCanvas("composite-mask", image);

        outline_canvas.className = "outline_layer"
        mask_canvas.className = "mask_layer"
        blur_mask_canvas.className = "blur_mask_layer"

        let img_ctx = img_canvas.getContext("2d", { willReadFrequently: true });
        let mask_ctx = mask_canvas.getContext("2d");
        let blur_mask_ctx = blur_mask_canvas.getContext("2d");
        let draw_ctx = draw_canvas.getContext("2d", { willReadFrequently: true });
        let outline_ctx = outline_canvas.getContext("2d");
        let composite_ctx = composite_canvas.getContext("2d");
        let composite_mask_ctx = composite_mask_canvas.getContext("2d");

        outline_ctx.strokeStyle = "rgba(0,0,0,0.5)";
        outline_ctx.lineWidth = 1.0;

        img_ctx.drawImage(image, 0, 0, image.naturalWidth, image.naturalHeight);

        if (paint_input_image) {
            state.img_base64_cache = null;
            draw_ctx.drawImage(paint_input_image, 0, 0, image.naturalWidth, image.naturalHeight);
        }

        if (mask_input_image) {
            state.mask_base64_cache = null;
            mask_ctx.drawImage(mask_input_image, 0, 0, image.naturalWidth, image.naturalHeight);
        }

        if (blur_mask_input_image) {
            blur_mask_ctx.drawImage(blur_mask_input_image, 0, 0, image.naturalWidth, image.naturalHeight);
        }

        state.clear = function () {
            draw_ctx.clearRect(0, 0, outline_canvas.width, outline_canvas.height);
            mask_ctx.clearRect(0, 0, outline_canvas.width, outline_canvas.height);
            state.img_base64_cache = null;
            state.mask_base64_cache = null;
        }

        state.composite = function () {
            if (state.img_base64_cache) { return state.img_base64_cache }

            composite_ctx.clearRect(0, 0, image.naturalWidth, image.naturalHeight);
            outline_ctx.globalCompositeOperation = 'source-over';
            composite_ctx.drawImage(img_canvas, 0, 0);
            composite_ctx.drawImage(draw_canvas, 0, 0);
            let to_return = composite_canvas.toDataURL("image/png", 1);
            state.img_base64_cache = to_return;
            return to_return
        }

        state.flipCanvas = function () {
            state.composite();
            draw_ctx.save();
            draw_ctx.translate(image.naturalWidth, 0);
            draw_ctx.scale(-1, 1);
            draw_ctx.drawImage(composite_canvas, 0, 0);
            draw_ctx.restore();
            state.img_base64_cache = null;
        }

        state.getPaintLayer = function () {
            return draw_canvas.toDataURL("image/png", 1);
        }

        state.getMaskLayer = function () {
            return mask_canvas.toDataURL("image/png", 1);
        }

        state.getBlurMaskLayer = function () {
            return blur_mask_canvas.toDataURL("image/png", 1);
        }

        state.compositeMask = function () {
            if (state.mask_base64_cache) { return state.mask_base64_cache }

            composite_mask_ctx.globalCompositeOperation = "source-over";
            composite_mask_ctx.fillStyle = "white";
            composite_mask_ctx.fillRect(0, 0, image.naturalWidth, image.naturalHeight);
            composite_mask_ctx.drawImage(mask_canvas, 0, 0);

            if (isCanvasAllWhite(composite_mask_canvas, composite_mask_ctx)) {
                return ""
            } else {
                let to_return = composite_mask_canvas.toDataURL("image/png", 1);
                state.mask_base64_cache = to_return;
                return to_return;
            }
        }

        function draw_image(data_url, ctx, before_draw) {
            data_url = sanatize_url(data_url);
            image = document.createElement("img");
            image.crossOrigin = "Anonymous";
            image.setAttribute("src", data_url);
            state.setDirty();
            state.img_base64_cache = null;
            state.mask_base64_cache = null;
            on_image_init(image, function () {
                before_draw(image);
                ctx.drawImage(image, 0, 0, image.naturalWidth, image.naturalHeight);
            })
        }

        state.updateImage = function (data_url) {
            state.img_base64_cache = null;
            draw_image(data_url, img_ctx, function (image) {
                resizeCanvas(image, img_canvas);
                resizeCanvas(image, draw_canvas);
                resizeCanvas(image, mask_canvas);
                resizeCanvas(image, blur_mask_canvas);
                resizeCanvas(image, outline_canvas);
                resizeCanvas(image, composite_canvas);
                resizeCanvas(image, composite_mask_canvas);
            });
        }

        state.setPaintImage = function (data_url) {
            state.img_base64_cache = null;
            draw_image(data_url, draw_ctx, function () { });
        }

        function mousedown(event) {
            event.target.setPointerCapture(event.pointerId);

            var target_ctx = null;
            switch (state.mode) {
                case "mask":
                    target_ctx = mask_ctx;
                    break
                case "blur":
                    target_ctx = blur_mask_ctx;
                    break
                case "paint":
                default:
                    target_ctx = draw_ctx;

            }

            var erasing = false;
            var shuffling = false;
            if (event.ctrlKey) {
                if (state.alt === "erase") {
                    target_ctx.globalCompositeOperation = "destination-out";
                    erasing = true;
                } else if (state.alt === "shuffle") {
                    shuffling = true;
                }
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

                if (state.mode === "paint") {
                    target_ctx.fillStyle = state.color;
                } else {
                    target_ctx.fillStyle = "black";
                }

                var radius = 0;
                if (state.penSize === 1) { radius = 1; } else { radius = state.penSize * pressure; }

                if (shuffling) {
                    let img_data = img_ctx.getImageData(0, 0, img_canvas.width, img_canvas.height);
                    let target_data = draw_ctx.getImageData(0, 0, draw_canvas.width, draw_canvas.height);
                    scramble(img_data, target_data, x, y, radius, last_x, last_y, last_radius, img_canvas.width, img_canvas.height);
                    target_ctx.putImageData(target_data, 0, 0);
                } else {
                    if (last_x === null || last_y === null) {
                        target_ctx.beginPath();
                        target_ctx.ellipse(x, y, radius, radius, 0, Math.PI * 2, 0);
                        target_ctx.fill();
                    } else {
                        drawPill(target_ctx, last_x, last_y, x, y, last_radius, radius);
                    }
                }
                last_x = x;
                last_y = y;
                last_radius = radius;
                state.setDirty();
                state.img_base64_cache = null;
                state.mask_base64_cache = null;
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
            //var inverse = "rgb(" + (255 - r) + "," + (255 - g) + "," + (255 - b) + ")";
            outline_ctx.strokeStyle = "rgb(255,255,255)";
            if (event.shiftKey) {
                outline_canvas.style.mixBlendMode = "normal"
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
            } else {
                outline_canvas.style.mixBlendMode = "difference"
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

    return {state : state, element : stack};
}

globalThis.painter = { init : painter_init }; 

function empty_white_image(width, height) {
    const canvas = document.createElement("canvas");
    canvas.setAttribute("width", width);
    canvas.setAttribute("height", height);
    const ctx = canvas.getContext("2d");
    ctx.fillStyle = "white";
    ctx.fillRect(0, 0, width, height);
    return canvas.toDataURL("image/png", 1);
}
