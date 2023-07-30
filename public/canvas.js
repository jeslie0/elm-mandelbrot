let mandlebrotCanvas;
let ctx;

let canvasHeight;
let canvasWidth;

let imageData;

const addRow = (rowData) => {
    const { row, computedColours } = rowData;

    const offset = 4 * row * computedColours.length;

    for (let i = 0; i < computedColours.length; i++) {
        const { red, blue, green, alpha } = computedColours[i]
        const scaledIdx= 4 * i + offset
        imageData.data[scaledIdx + 0] = 255 * red
        imageData.data[scaledIdx + 1] = 255 * green
        imageData.data[scaledIdx + 2] = 255 * blue
        imageData.data[scaledIdx + 3] = 255 * alpha
    }

    // Draw image data to the canvas
    ctx.putImageData(imageData, 0, 0);
}

const addRows = (rowsData) => {
    const helper = (_acc, rowData) => {
        const { row, computedColours } = rowData;

        const offset = 4 * row * computedColours.length;

        for (let i = 0; i < computedColours.length; i++) {
            const { red, blue, green, alpha } = computedColours[i]
            const scaledIdx= 4 * i + offset
            imageData.data[scaledIdx + 0] = 255 * red
            imageData.data[scaledIdx + 1] = 255 * green
            imageData.data[scaledIdx + 2] = 255 * blue
            imageData.data[scaledIdx + 3] = 255 * alpha
        }
    }

    rowsData.reduce(helper, undefined);
    ctx.putImageData(imageData, 0, 0);
}

const setInitialSettings = (settings) => {
    const {height, width} = settings

    canvasHeight = height
    canvasWidth = width

    mandlebrotCanvas = document.getElementById("mandelbrot");
    ctx = mandlebrotCanvas.getContext("2d");
    imageData = ctx.createImageData(width, height);

    app.ports.settingsSet.send(1);
}



app.ports.sendRow.subscribe(addRow)
app.ports.sendRows.subscribe(addRows)
app.ports.sendInitialSettings.subscribe(setInitialSettings)
