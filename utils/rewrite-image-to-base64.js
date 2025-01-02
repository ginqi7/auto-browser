const canvas = document.createElement("canvas");
canvas.width = this.naturalWidth;
canvas.height = this.naturalHeight;

const ctx = canvas.getContext("2d");
ctx.drawImage(this, 0, 0);

const base64Data = canvas.toDataURL("image/png");
this.src = base64Data
console.log(`Image converted to Base64: ${base64Data}`);        

