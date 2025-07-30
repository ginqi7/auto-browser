el => {
    const canvas = document.createElement("canvas");
    canvas.width = el.naturalWidth;
    canvas.height = el.naturalHeight;

    const ctx = canvas.getContext("2d");
    ctx.drawImage(el, 0, 0);

    const base64Data = canvas.toDataURL("image/png");
    el.src = base64Data
    console.log(`Image converted to Base64: ${base64Data}`);
}
