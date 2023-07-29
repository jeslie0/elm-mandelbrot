const mandlebrotCanvas = document.getElementById("mandelbrot");
const ctx = mandlebrotCanvas.getContext("2d");
const imageData = ctx.createImageData(400, 400)

const addRow = (rowData) => {
    console.log("adding row", rowData)
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



app.ports.sendRow.subscribe(addRow)
